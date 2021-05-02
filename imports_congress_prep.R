#Title: Testing Models of Public Policy (Quant III)
#Author: Liam Tay Kearney, ltk2118
#Date: Spring 2021
#Effects of import competition on house FTA voting behaviour 

# [0]  Setup ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(Rvoteview)
library(wnominate)
library(stringr)
library(tidycensus)
library(tmap)
library(stargazer)
library(lmtest)
library(sandwich)
library(mfx)
library(conflicted)

#note, Rvoteview must be installed via devtools
#devtools::install_github("voteview/Rvoteview")

#declare preferences for conflicts
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("day", "lubridate")
conflict_prefer("lag", "dplyr")

# [1]  Collect and clean roll call data -------------------------------------


#pull all the metadata on FTA votes from 108th to 117th congresses
#this corresponds to the period of interest w.r.t our import competition data
#keep only "On Passage" bills
res_freetrade <- voteview_search("'free trade'", 
                                 congress = c(108:117), chamber = "House") %>% 
                        filter(id!="RH1111537") %>% 
                        filter(bill_number!="HR644") %>% 
                        filter(question=="On Passage"|
                                 question=="Sustain the Ruling of the Chair")


## define a function to return detailed roll call data in long-form
## add columns for party total yea/nay votes, party majority votes
## summary function is the same but without party vote detail 
extract_votedata <- function(search_results){
  
  #download detailed roll call data for the search results
  rcall <- voteview_download(search_results$id)
  
  #convert roll call data from wide to long form and tidy up
  fta_rollcalls <- melt_rollcall(rcall,
                           legiscols = c("name", "state_abbrev","party_code", 
                                         "dim1", "dim2", "cqlabel"),
                           votecols = c("vname", "date", "congress", 
                                        "party_vote_counts.200.1", 
                                        "party_vote_counts.200.6",
                                        "party_vote_counts.100.1", 
                                        "party_vote_counts.100.6")) %>% 
    
    #reformat districts to match trade data for joining later
    mutate(district=paste(state_abbrev,              
                          as.numeric(str_extract(cqlabel,"[:digit:]+")))) %>% 
    filter(cqlabel!="(POTUS)") %>% #remove potus
    select(-cqlabel) %>% 
    
    #remove abstentions and recode yeas/nays as 1s and 0s
    mutate(vote=ifelse(vote==1,1, ifelse(vote==6,0,NA))) %>% 
    
    #change Bernie Sanders from Independent to Democrat 
    mutate(party_code=replace(party_code, 
                              name == "SANDERS, Bernard", 100)) %>% 
    
    #calculate party vote behaviour in aggregate
    mutate(R_yea=as.numeric(party_vote_counts.200.1), 
           R_nay = as.numeric(party_vote_counts.200.6), 
           D_yea=as.numeric(party_vote_counts.100.1), 
           D_nay = as.numeric(party_vote_counts.100.6)) %>% 
    
    select(-party_vote_counts.200.1,-party_vote_counts.200.6,
           -party_vote_counts.100.1,-party_vote_counts.100.6) %>% 
    
    mutate(R_yea = replace_na(R_yea, 0), R_nay = replace_na(R_nay, 0),
           D_yea = replace_na(D_yea, 0), D_nay = replace_na(D_nay, 0)) %>% 
    mutate(R_yea_vote = R_yea/(R_yea+R_nay), 
           D_yea_vote = D_yea/(D_yea+D_nay)) %>% 
    
    #code party majority vote as >75% yeas (yea) or <25% yeas (nay)
    mutate(R_maj = ifelse(R_yea_vote>0.75, 1, 
                          ifelse(R_yea_vote<0.25,-1,0))) %>% 
    mutate(D_maj = ifelse(D_yea_vote>0.75, 1, 
                          ifelse(D_yea_vote<0.25,-1,0)))
  
    return(fta_rollcalls)
}
extract_votedata_summary <- function(search_results){
  
  #download detailed roll call data for the search results
  rcall <- voteview_download(search_results$id)
  
  #convert roll call data from wide to long form and tidy up
  fta_rollcalls <- melt_rollcall(rcall,
                                 legiscols = c("name", "state_abbrev","party_code", 
                                               "dim1", "dim2", "cqlabel"),
                                 votecols = c("vname", "date", "congress", 
                                              "party_vote_counts.200.1", 
                                              "party_vote_counts.200.6",
                                              "party_vote_counts.100.1", 
                                              "party_vote_counts.100.6")) %>% 
    
    #reformat districts to match trade data for joining later
    mutate(district=paste(state_abbrev,              
                          as.numeric(str_extract(cqlabel,"[:digit:]+")))) %>% 
    filter(cqlabel!="(POTUS)") %>% #remove potus
    select(-cqlabel) %>% 
    
    #remove abstentions and recode yeas/nays as 1s and 0s
    mutate(vote=ifelse(vote==1,1, ifelse(vote==6,0,NA))) %>% 
    
    #change Bernie Sanders from Independent to Democrat 
    mutate(party_code=replace(party_code, 
                              name == "SANDERS, Bernard", 100))
  
  return(fta_rollcalls)
}

#save fta rollcalls to object 
fta_rollcalls <- extract_votedata(res_freetrade)



# [2]  Process import competition and controls data  --------------------

#read-in imports data and population shares, at county-district cell level
imports <- read_csv("https://raw.githubusercontent.com/ltk2118/congress_trade/main/house_2002_2016.csv") %>% 
  rename(fips=cty_fips, 
         "district108"=congressionaldistrict) %>% 
  
  #select variables to keep
  select(district108,fips,czone, #geographies
         starts_with("reg_"), #regional controls
         d_imp_usch_pd, #import competition intensity (non-instrumented)
         d_imp_otch_lag_pd,  #import competition intensity (instrumented)
         starts_with("l_sh"), #labor market composition controls
                  sh_district_2002) %>%  #population share of district in 2002
  
  #reformat district code for joins
  mutate(ctycd108 = 100*fips + 
           as.numeric(str_extract(district108,
                                    "[:digit:]+"))) %>% 
  rename(sh_district_108=sh_district_2002)

#validation
#Check that all county x district cell population shares sum to one 
#for each district - there should be 432 x 1s 
imports %>% 
  group_by(district108) %>% 
  summarise(pop_total=round(sum(sh_district_108),2)) %>% 
  select(pop_total) %>% table()

# [3]  Make roll call objects for each congress ----------------------------

#define a function to split the long-form roll call data by congress
#and return vote, party, dim1, dim2 and party votes only
rc_by_congress <- function(rollcalls_object, congress_number){
  rollcalls_object %>% 
    filter(congress==congress_number) %>% 
    group_by(district) %>% 
    summarise(vote=mean(vote,na.rm=TRUE), 
              party_code = mean(as.numeric(party_code)),
              dim1=mean(dim1,na.rm=TRUE), dim2=mean(dim2,na.rm=TRUE),
              R_party_vote = mean(R_yea_vote, na.rm=TRUE),
              D_party_vote = mean(D_yea_vote, na.rm=TRUE)) %>% 
    mutate(diffparty = ifelse(party_code==200, 
                              vote-R_party_vote, 
                              vote-D_party_vote))
}

#save to individual objects for each congress
#note, there is no 111, 113, 114, 115th congress 
#because there were no FTA votes during those congresses
rc_108 <- fta_rollcalls %>% rc_by_congress(108)
rc_109 <- fta_rollcalls %>% rc_by_congress(109)
rc_110 <- fta_rollcalls %>% rc_by_congress(110)
rc_112 <- fta_rollcalls %>% rc_by_congress(112)
rc_116 <- fta_rollcalls %>% rc_by_congress(116)



# [4]  Aggregate import and controls data to district level ----------------

##define function to help merge data sets 
#store long and wide form full data sets to the global env
join_imports_rollcalls <- function(){
  
  #get mean of import and control variables for each county-district cell
  #then aggregate to district level 
  #weighting by each cell's population share in the district
  imports_108 <<- imports %>% 
    group_by(district108) %>% 
    summarise(across(reg_midatl:l_sh_dem_hispanic, 
                     ~ stats::weighted.mean(.x, w=sh_district_108)))
    
    #major redistricting prior to 113th congress, so ignore 113th for now
    #will deal with it later with population share crosswalks
    #for now, just join up import and vote data for 108-112th congresses
    #ignore minor redistricting between 108-112th congresses
    full_108_112_wide <<- imports_108 %>% 
      inner_join(rc_108, by=c("district108"="district")) %>% 
      inner_join(rc_109, by=c("district108"="district"),suffix=c("","_109")) %>% 
      inner_join(rc_110, by=c("district108"="district"),suffix=c("","_110")) %>% 
      inner_join(rc_112, by=c("district108"="district"),suffix=c("","_112")) %>% 
      
      #get average vote across 108th-112th congresses
      #cols 50,43,36,29 correspond to votes
      mutate(avg_vote = rowMeans(.[c(50,43,36,29)],na.rm=TRUE))
    
    #make long-form full data set for 108th to 112th congresses
    full_108_112_long <<- mutate(rc_108, congress=108) %>% 
      rbind(mutate(rc_109, congress=109)) %>% 
      rbind(mutate(rc_110, congress=110)) %>% 
      rbind(mutate(rc_112, congress=112)) %>% 
      inner_join(imports_108, by=c("district"="district108"))
    
}

join_imports_rollcalls()



# [5]  Redistricting for 113th congress onward -----------------------------

##define function to return import and control variables 
#re-weighted for major redistricting prior to the 113th congress
#essentially, "rebase" everything to the start of the 113th congress
#again, ignore minor redistricting after 113th congress
#returned in object "imports_113"

calculate_redistrict <- function(){
  
  #read-in district crosswalks with population share mappings by old district to new 
  cwd113 <- read_csv("ctycd_crosswalks/ctycd113.csv") %>% 
    rename(district108 = congressionaldistrict108, 
           district113 = congressionaldistrict113)
  
  #calculate population shares of new (county x district) cells in new districts
  congress_113 <<- imports %>% 
    
    #join import data with crosswalk and reformat var names 
    left_join(select(cwd113, -district108), 
              by=c("ctycd108"="ctycd108")) %>% 
    rename(afact108_113 = afact_ctycd108_ctycd113) %>% 
    select(-cty_fips_2000, 
           -cty_fips_2010,
           -state_fips,
           -afact_ctycd113_ctycd108) %>% 
    
    #repopulate data set with districts which had no redistricting
    #set crosswalk population share multiplier = 1 for these districts 
    mutate(district113 = ifelse(is.na(district113), district108, district113),
           ctycd113 = ifelse(is.na(ctycd113),ctycd108,ctycd113), 
           afact108_113 = ifelse(is.na(afact108_113),1,afact108_113),
           
           #calculate new cell populations
           pop_113 = sh_district_108*afact108_113) %>% 
           group_by(district113) %>% 
           mutate(distpop_113 = sum(pop_113)) %>% 
           ungroup() %>%
    
    #calculate new cell population shares in new districts
    mutate(sh_district_113 = pop_113/distpop_113) %>% 
    
    #tidy up
    select(-distpop_113, -fips, -czone, -pop_113, -district108)
  
  #validation for population shares
  #Check that all county x district cell population shares sum to one for each district
  congress_113 %>% 
    group_by(district113) %>% 
    summarise(sum(sh_district_113)) %>%
    select(`sum(sh_district_113)`) %>% 
    table() %>% 
    print()
  print("Validated -- all cell shares sum to 1")
  
  #collapse by district means, weighted by the product of 
  #(share of old cell population mapping to new cell) x 
  #(share of new cell population in new district population)
  #save this to the global env
  imports_113 <<- congress_113 %>% 
    group_by(district113) %>% 
    summarise(across(reg_midatl:l_sh_dem_hispanic, 
                     ~ stats::weighted.mean(.x, w=sh_district_113*afact108_113)))
  
}

calculate_redistrict()

#generate combined data set for the single FTA (USMCA) in the 116th congress
full_113_116 <- imports_113 %>%
  inner_join(rc_116, by=c("district113"="district")) %>% 
  mutate(congress=116) %>% 
  rename(district=district113)



# [6]  Preliminary plots (EDA) -------------------------------------------------

#Scatter plots by congress and party, with linear fits
plot_1 <- full_108_112_long %>% 
  filter(party_code==200 | party_code==100) %>% 
  ggplot() +
  aes(y=vote, x=log(d_imp_usch_pd), col=as.factor(party_code)) +
    geom_point(size=1) +
    geom_smooth(method="lm", formula=y~x) +
    scale_y_continuous(limits=c(0, 1)) +
       scale_color_manual(name = "Party", 
          labels = c("D", "R"), 
          values=c("blue", "red")) +
     ylab("Average vote") +
     xlab("Import competition intensity, 2000-2010") +
     ggtitle("Figure 3: 108th-112th Congresses", 
          subtitle = "Import competition intensity vs house FTA votes") +
     facet_wrap(.~congress) + 
     theme(strip.background =element_rect(fill="grey46")) + 
     theme(strip.text = element_text(colour = 'white',face="bold")) + 
     theme(legend.position="bottom") + 
     theme(text = element_text(size=9))

#Scatter plot for USMCA 
#Note, there were not many votes against
plot_2 <- full_113_116 %>% 
  filter(party_code==200 | party_code==100) %>% 
    ggplot() +
    aes(y=vote, x=log(d_imp_usch_pd), col=as.factor(party_code)) +
    geom_point() +
    geom_smooth(method="lm", formula=y~x) +
    scale_color_manual(name = "Party", 
                       labels = c("D", "R"), 
                       values=c("blue", "red")) +
    ylab("Average vote") +
    xlab("District change in import competition intensity after 2001") +
    ggtitle("116th Congress - USMCA", 
            subtitle = "Import competition intensity vs house voting outcome on USMCA")

##what about polarization? we can plot the average nominate scores over time.
plot_3 <- full_108_112_long %>% 
  rbind(full_113_116) %>% 
  group_by(district, congress) %>% 
  summarise(across(vote:diffparty, ~mean(.x, na.rm=TRUE))) %>% 
  group_by(congress, party_code) %>% 
  summarise(avg_dim1 = mean(dim1), sd_dim1 = sd(dim1)) %>% 
  filter(party_code==100 | party_code==200) %>% 
    ggplot() + 
     aes(y=avg_dim1, x=congress, col=as.factor(party_code)) +
      geom_line(size=1) +
      scale_color_manual(name = "Party", 
                       labels = c("D", "R"), 
                       values=c("blue", "red")) +
      xlab("Congress Number") + 
      ylab("Party Average Dim 1") +
    ggtitle("Change in Party Average Dim 1 (Nominate)")

#st deviation of vote within party
plot_4 <- full_108_112_long %>% 
    rbind(full_113_116) %>% 
    group_by(district, congress) %>% 
    summarise(across(vote:diffparty, ~mean(.x, na.rm=TRUE))) %>% 
    group_by(congress, party_code) %>% 
    summarise(sd_vote = sd(vote, na.rm=TRUE)) %>% 
    filter(party_code==100 | party_code==200) %>% 
    ggplot() + 
    aes(y=sd_vote, x=congress, col=as.factor(party_code)) +
    geom_line(size=1) +
    scale_color_manual(name = "Party", 
                       labels = c("D", "R"), 
                       values=c("blue", "red")) +
    xlab("Congress Number") + 
    ylab("Standard Deviation of Party Vote") +
  ggtitle("Within-party dispersion in voting behaviour")

#see for 108th-112th congress, before redistricting
plot_5 <- full_108_112_long %>% 
  mutate(imp_high = log(d_imp_usch_pd)>=median(log(d_imp_usch_pd))) %>% 
  group_by(imp_high, congress, party_code) %>% 
  filter(party_code==100 | party_code==200) %>%
  summarise(party_code=ifelse(mean(party_code)==100,"Democrat","Republican"),
            vote=mean(vote,na.rm=TRUE)) %>% 
  ggplot() + 
  aes(y=vote, x=congress, fill=imp_high) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(.~party_code) +
  scale_fill_manual(name = "Import Competition in District", 
                     labels = c("Low", "High"), 
                     values=c("pink","deeppink3")) +
  ylab("Average vote") +
  xlab("Congress") +
  theme(legend.position="bottom") +
  ggtitle("Figure 4: 108th-112th Congresses",
          subtitle = "Avg FTA vote by high/low import competition") + 
  theme(text = element_text(size=9))

# [7]  Maps --------------------------------------------------------------------

#make county-level map of import competition intensity
#load relevant shape data directly from web
load(url("https://github.com/ltk2118/congress_trade/blob/main/us_county_geometries.RData?raw=true"))

mapdata <- shapes_us_counties %>% 
              full_join(imports %>% 
                    group_by(fips) %>% 
                    summarise(across(d_imp_usch_pd:l_sh_dem_hispanic, 
                                     ~ stats::weighted.mean(.x, 
                                                  w=sh_district_108)))) %>% 
                    filter(!(stringr::str_detect(NAME, 
                                           "(, Alaska)|(, Hawaii)"))) %>% 
                    mutate(l_oth = log(d_imp_otch_lag_pd+1))

#plot map for import intensity 
import_map <- tm_shape(mapdata, projection = 2163) +
              tm_fill("l_oth",
                      palette = "BuPu", 
                      breaks=quantile(mapdata$l_oth, 
                                      c(0,.01,.1,.5,.9,.99,1), 
                                      na.rm = TRUE),
                      labels=c("<1", "1-10", "10-50", 
                               "50-90","90-99", ">99"),
                      border.col = "white", 
                      border.alpha = 0.1,
                      colorNA = "gray",
                      title = "Percentile") +
              tm_legend(legend.outside.position = "left",
                        legend.outside=TRUE) + 
  tm_layout(main.title = "Figure 1: Import Competition Intensity, 2000-2010", 
            main.title.position = "right",
            main.title.size = 1)


#map for manufacturing industry intensity - almost identical!
manuf_map <- tm_shape(mapdata, projection = 2163) +
  tm_fill("l_shind1_manuf_cbp",
          palette = "BuPu", 
          breaks=quantile(mapdata$l_shind1_manuf_cbp, 
                          c(0,.01,.1,.5,.9,.99,1), 
                          na.rm = TRUE),
          labels=c("<1", "1-10", "10-50", 
                   "50-90","90-99", ">99"),
          border.col = "white", 
          border.alpha = 0.1,
          colorNA = "gray",
          title = "Percentile") +
  tm_legend(legend.outside.position = "left",
            legend.outside=TRUE) + 
  tm_layout(main.title = "Industry share in manufacturing, 2000-2007", 
            main.title.position = "right",
            main.title.size = 1)

offshore_map <- tm_shape(mapdata, projection = 2163) +
  tm_fill("l_shind3_task_outsource",
          palette = "-BrBG", 
          breaks=quantile(mapdata$l_shind3_task_outsource, 
                          c(0,.01,.1,.5,.9,.99,1), 
                          na.rm = TRUE),
          labels=c("<1", "1-10", "10-50", 
                   "50-90","90-99", ">99"),
          border.col = "white", 
          border.alpha = 0.1,
          colorNA = "gray",
          title = "Percentile") +
  tm_legend(legend.outside.position = "left",
            legend.outside=TRUE) +
  tm_layout(main.title = "Offshorability Index of Occupations, 2000-2007", 
    main.title.position = "right",
    main.title.size = 1)


# [8]  scatterplot matrices ----------------------------------------------------

smatrices_out <- function(number){
  if(number==1){
        #import concentration, average vote, 108-112th congresses
            full_108_112_reg_2 <- full_108_112_reg %>% 
              rename(log_d_imp = l_oth, 
                     `Average vote, 108th-112th Congress` = avg_vote_108_112)
                  psych::pairs.panels(full_108_112_reg_2[,c(54,61)],
                    method = "pearson", # correlation method
                    hist.col = "#00AFBB",
                    density = TRUE,  # show density plots
                    ellipses = TRUE, # show correlation ellipses
                    main = "Figure 2: Scatter matrix of vote and import competition intensity") 
  }
  else if(number==2){
    #import concentration, dim1, vote (raw)
    full_108_112_long_2 <- full_108_112_long %>% 
      mutate(l_oth=log(d_imp_otch_lag_pd)) %>% 
      rename(log_d_imp = l_oth)
                  psych::pairs.panels(full_108_112_long_2[,c(2,4,38)],
                    method = "pearson", # correlation method
                    hist.col = "#00AFBB",
                    density = TRUE,  # show density plots
                    ellipses = TRUE) # show correlation ellipses
  }
  
  else if(number==3){
    #same but only for the 108th congess
                  psych::pairs.panels(full_108_112_reg[,c(30,31,61)],
                    method = "pearson", # correlation method
                    hist.col = "#00AFBB",
                    density = TRUE,  # show density plots
                    ellipses = TRUE) # show correlation ellipses
  }
    
  else if(number==4){
        #correlations between votes in the 108th congress
        #high correlations between votes but not as high as perhaps expected
                psych::pairs.panels(fta_rollcalls_108[,c(5:8)],
                    method = "pearson", # correlation method
                    hist.col = "#00AFBB",
                    density = TRUE,  # show density plots
                    ellipses = TRUE) # show correlation ellipses
    }
  }


# [9]  Prepare data for regression analysis ------------------------------

#recode variables for regression
full_108_112_reg <- full_108_112_wide %>% 
  
  #remove independents and recode republican = 1, democrat = 0
  filter(party_code==200 | party_code==100) %>% 
  mutate(partyR_108 = ifelse(party_code==200,1,0)) %>% 
  mutate(partyR_109 = ifelse(party_code_109==200,1,0)) %>% 
  mutate(partyR_110 = ifelse(party_code_110==200,1,0)) %>% 
  mutate(partyR_112 = ifelse(party_code_112==200,1,0)) %>% 
  
  #reformat naming to be consistent
  rename(vote_108=vote,
         dim1_108=dim1, 
         dim2_108=dim2, 
         R_party_vote_108=R_party_vote, 
         D_party_vote_108=D_party_vote,
         diffparty_108=diffparty, 
         avg_vote_108_112=avg_vote) %>% 
  
  #get average dim1 
  mutate(avg_dim1 = (dim1_108+dim1_109+dim1_110+dim1_112)/4) %>% 
  
  #take logs of import competition to normalize
  mutate(l_imp = log(d_imp_usch_pd), 
         l_oth = log(d_imp_otch_lag_pd)) %>% 
  
  #tidy up
  select(-party_code, -party_code_109, 
         -party_code_110, -party_code_112)

#inspect histogram of import data, justification for log transform
hist(full_108_112_reg$d_imp_otch_lag_pd)
hist(full_108_112_reg$l_oth)

#use same recoding procedure for 113th congress onwards (redistrited)
full_113_116_reg <- full_113_116 %>% 
  filter(party_code==200 | party_code==100) %>% 
  mutate(partyR_116 = ifelse(party_code==200,1,0)) %>% 
  rename(vote_116=vote,
         dim1_116=dim1, 
         dim2_116=dim2, 
         R_party_vote_116=R_party_vote, 
         D_party_vote_116=D_party_vote,
         diffparty_116=diffparty) %>%
  mutate(l_imp = log(d_imp_usch_pd), 
         l_oth = log(d_imp_otch_lag_pd)) %>%
  select(-party_code)


# [10] Regression analysis [OLS] -------------------------------------------

#create groups of covariates 
labmkt_covars <- colnames(full_108_112_reg) %>% 
  str_extract("l\\_shind.*") %>% 
  as.data.frame() %>% drop_na() %>% unlist() %>% paste(collapse = " + ")

demographic_covars <- colnames(full_108_112_reg) %>% 
  str_extract("l\\_sh\\_dem.*") %>% 
  as.data.frame() %>% drop_na() %>% unlist() %>% paste(collapse = " + ")

region_covars <- colnames(full_108_112_reg) %>% 
  str_extract("reg\\_.*") %>% 
  as.data.frame() %>% drop_na() %>% unlist() %>% paste(collapse = " + ")

#test different specifications, increasing in number/type of covariates
#bivariate
model1 <- lm(vote_108 ~ l_oth,
             data=full_108_112_reg) 
#add ideology (dim1)
model2 <- lm(vote_108 ~ l_oth + dim1_108, 
             data=full_108_112_reg) 
#add ideology and party
model3 <- lm(vote_108 ~ l_oth + dim1_108 + partyR_108, 
             data=full_108_112_reg) 
#add ideology, party and party interaction
model4 <- lm(vote_108 ~ l_oth + dim1_108 + partyR_108 + l_oth*partyR_108, 
             data=full_108_112_reg) 
#add regional covariates (mountain, pacific, midwest, etc)
model5 <- lm(paste0("vote_108 ~ l_oth + dim1_108 + partyR_108 + l_oth*partyR_108 +",
               region_covars),
               data=full_108_112_reg)
#add labor market covariates 
model6 <- lm(paste0("vote_108 ~ l_oth + dim1_108 + partyR_108 + l_oth*partyR_108 +",
             labmkt_covars),
             data=full_108_112_reg)
#add demographic covariates
model7 <- lm(paste0("vote_108 ~ l_oth + dim1_108 + partyR_108 + l_oth*partyR_108 +",
             demographic_covars),
             data=full_108_112_reg)
#add all covariates (full model)
model8 <- lm(paste0("vote_108 ~ l_oth + dim1_108 + partyR_108 + l_oth*partyR_108 +",
             region_covars, " + ", labmkt_covars, " + ", demographic_covars),
             data=full_108_112_reg)

olsmodels_108 <- list(model1,model2,model3,model4,
                  model5,model6,model7,model8)

#does the effect sustain through the 109th, 110th and 112th congresses?
model_108th <-lm(paste0("vote_108 ~ l_oth + dim1_k + partyR_k + l_oth*partyR_k +",
                  region_covars, " + ", labmkt_covars, " + ", demographic_covars),
                  data=full_108_112_reg %>% 
                    rename(partyR_k = partyR_108, dim1_k = dim1_108)) #YES

model_109th <-lm(paste0("vote_109 ~ l_oth + dim1_k + partyR_k + l_oth*partyR_k +",
                         region_covars, " + ", labmkt_covars, " + ", demographic_covars),
                  data=full_108_112_reg %>% 
                    rename(partyR_k = partyR_109, dim1_k = dim1_109)) #NO

model_112th <-lm(paste0("vote_112 ~ l_oth + dim1_k + partyR_k + l_oth*partyR_k +",
                         region_covars, " + ", labmkt_covars, " + ", demographic_covars),
                  data=full_108_112_reg %>% 
                    rename(partyR_k = partyR_112, dim1_k = dim1_112)) #YES

ols_108_112 <- list(model_108th,model_109th,model_112th)

# [11] Residual diagnostics ---------------------------------------------------------

lmtest::bptest(model8) #heteroskedasticity is present
#use heteroskedasticity-consistent standard errors in output

lmtest::bgtest(model8) #serial correlation does not appear to be a problem

regclass::qq(full_108_112_reg$l_oth)

#very high VIFS for some variables, particularly older age brackets
vifs <- regclass::VIF(model8) 

#therefore, remove them from subsequent estimations
#also suspect there might be some collinearity resulting from race categories
#so, define a new set of demographic covariates with these changes
demographic_covars_new <- colnames(full_108_112_reg) %>% 
str_extract("l\\_sh\\_dem.*") %>% 
as.data.frame() %>% drop_na() %>% 
  filter(!.%in%c("l_sh_dem_age_6069","l_sh_dem_age_7079",
                   "l_sh_dem_age_5059","l_sh_dem_age_3039",
                   "l_sh_dem_age_8000",
                   "l_sh_dem_black")) %>% 
    unlist() %>% paste(collapse = " + ") 

# [12] Probit helper functions -------------------------------------------------

#define helper functions to calculate heteroskedacity consistent SEs 
#and their corresponding coefficient estimates and p-values
serrors <- function(models){
  stack <- vector("list",length=length(models))
  for (i in 1:length(models)){
    stack[[i]] <- coeftest(models[[i]], vcov. = vcovHC, type = "HC1")[,2]
  }
  return(stack)
}
coefs <- function(models){
  stack <- vector("list",length=length(models))
  for (i in 1:length(models)){
    stack[[i]] <- coeftest(models[[i]], vcov. = vcovHC, type = "HC1")[,1]
  }
  return(stack)
}
pvals <- function(models){
  stack <- vector("list",length=length(models))
  for (i in 1:length(models)){
    stack[[i]] <- coeftest(models[[i]], vcov. = vcovHC, type = "HC1")[,4]
  }
  return(stack)
}

#Marginal Effects
#Define function to calculate marginal effect dF/dx at the mean
#only works with a list of models
marginal_effects <- function(models){
  stack <- list()
  for (i in 1:length(models)){
    mfx <- probitmfx(formula=models[[i]]$formula,
                     data=models[[i]]$data)
    stack[[i]] <- mfx[["mfxest"]][,1]
  }
  return(stack)
}



# [13] Probits on individual votes (110th, 116th congresses) --------------

#single bills in the 110th and 116th congresses
#United States-Peru Trade Promotion Agreement (110th)
probit_110 <- glm(paste0("vote_110 ~ l_oth + dim1_k + partyR_k + l_oth*partyR_k + ",
                         labmkt_covars, " + ",
                         region_covars, " + ",
                         demographic_covars_new),
                  family = binomial(link = "probit"),
                  data = full_108_112_reg %>%
                    rename(partyR_k = partyR_110) %>%
                    rename(dim1_k = dim1_110))

#United States-Mexico-Canada Agreement Implementation (116th)
probit_116 <- glm(formula=paste0("vote_116 ~ l_oth + dim1_k + partyR_k + l_oth*partyR_k + ",
                                 labmkt_covars, " + ",
                                 region_covars, " + ",
                                 demographic_covars_new),
                  family = binomial(link = "probit"),
                  data = full_113_116_reg %>%
                    rename(partyR_k = partyR_116) %>%
                    rename(dim1_k = dim1_116))

#put models into a list object
probits_110_116th <- list(probit_110,probit_116)

# [14] Probits on individual votes (108th congress)--------------------

#Use probit link to estimate models for each FTA bill of the 108th congress
#use full controls in each specification
fta_rollcalls_108 <- fta_rollcalls %>% 
  filter(congress==108) %>%
  filter(party_code==200 | party_code==100) %>%
  select(district,vname,vote,party_code,dim1) %>%
  mutate(party_code=as.numeric(party_code)) %>%
  spread(vname,vote) %>%
  inner_join(imports_108,by=c("district"="district108")) %>%
    mutate(party=ifelse(party_code==200,1,0)) %>%
    mutate(l_oth=log(d_imp_otch_lag_pd), 
           l_imp=log(d_imp_usch_pd)) %>% 
  rename(CAFTA = RH1080409,
         Singapore = RH1080430,
         Chile = RH1080434,
         Australia = RH1081049,
         Morocco = RH1081087)

probit_108_CAFTA <- glm(paste0("CAFTA ~ l_oth + dim1 + party + l_oth*party +",
                               #labmkt_covars, " + ",
                               region_covars, " + ",
                               demographic_covars_new),
                            family = binomial(link = "probit"),
                            data = fta_rollcalls_108)

probit_108_Singapore <- glm(paste0("Singapore ~ l_oth + dim1 + party + l_oth*party +",
                               #labmkt_covars, " + ",
                               region_covars, " + ",
                               demographic_covars_new),
                        family = binomial(link = "probit"),
                        data = fta_rollcalls_108)

probit_108_Chile <- glm(paste0("Chile ~ l_oth + dim1 + party + l_oth*party +",
                               #labmkt_covars, " + ",
                               region_covars, " + ",
                               demographic_covars_new),
                        family = binomial(link = "probit"),
                        data = fta_rollcalls_108)

probit_108_Australia <- glm(paste0("Australia ~ l_oth + dim1 + party + l_oth*party +",
                               #labmkt_covars, " + ",
                               region_covars, " + ",
                               demographic_covars_new),
                        family = binomial(link = "probit"),
                        data = fta_rollcalls_108)

probit_108_Morocco <- glm(paste0("Morocco ~ l_oth + dim1 + party + l_oth*party +",
                               #labmkt_covars, " + ",
                               region_covars, " + ",
                               demographic_covars_new),
                        family = binomial(link = "probit"),
                        data = fta_rollcalls_108)

probits_108th <- list(#probit_108_CAFTA, -- model did not converge
                      probit_108_Singapore,
                      probit_108_Chile,
                      probit_108_Australia,
                      probit_108_Morocco)


# [15] Probits on individual votes (with expanded labor market controls) -------

##Add share in manufacturing and level of task outsourcing
##The estimates on import competition are much less significant now.
probit_108_CAFTA_2 <- glm(paste0("CAFTA ~ l_oth + dim1 + party + l_oth*party +",
                               labmkt_covars, " + ",
                               region_covars, " + ",
                               demographic_covars_new),
                        family = binomial(link = "probit"),
                        data = fta_rollcalls_108)

probit_108_Singapore_2 <- glm(paste0("Singapore ~ l_oth + dim1 + party + l_oth*party +",
                                   labmkt_covars, " + ",
                                   region_covars, " + ",
                                   demographic_covars_new),
                            family = binomial(link = "probit"),
                            data = fta_rollcalls_108)

probit_108_Chile_2 <- glm(paste0("Chile ~ l_oth + dim1 + party + l_oth*party +",
                               labmkt_covars, " + ",
                               region_covars, " + ",
                               demographic_covars_new),
                        family = binomial(link = "probit"),
                        data = fta_rollcalls_108)

probit_108_Australia_2 <- glm(paste0("Australia ~ l_oth + dim1 + party + l_oth*party +",
                                   labmkt_covars, " + ",
                                   region_covars, " + ",
                                   demographic_covars_new),
                            family = binomial(link = "probit"),
                            data = fta_rollcalls_108)

probit_108_Morocco_2 <- glm(paste0("Morocco ~ l_oth + dim1 + party + l_oth*party +",
                                 labmkt_covars, " + ",
                                 region_covars, " + ",
                                 demographic_covars_new),
                          family = binomial(link = "probit"),
                          data = fta_rollcalls_108)

probits_108th_2 <- list(#probit_108_CAFTA_2, --Note: deleted due to non-convergence
                      probit_108_Singapore_2,
                      probit_108_Chile_2,
                      probit_108_Australia_2,
                      probit_108_Morocco_2)

# [16] Probits on individual votes (112th congress) -----------------------
fta_rollcalls_112 <- fta_rollcalls %>% 
  filter(congress==112) %>%
  filter(party_code==200 | party_code==100) %>%
  select(district,vname,vote,party_code,dim1) %>%
  mutate(party_code=as.numeric(party_code)) %>%
  spread(vname,vote) %>%
  inner_join(imports_108,by=c("district"="district108")) %>%
  mutate(party=ifelse(party_code==200,1,0)) %>%
  mutate(l_oth=log(d_imp_otch_lag_pd), 
         l_imp=log(d_imp_usch_pd)) %>% 
  rename(Colombia = RH1120777,
         Panama = RH1120778,
         Korea = RH1120779)

probit_112_Colombia <- glm(paste0("Colombia ~ l_oth + dim1 + party + l_oth*party +",
                                  labmkt_covars, " + ",
                                  region_covars, " + ",
                                  demographic_covars_new),
                           family = binomial(link = "probit"),
                           data = fta_rollcalls_112)

probit_112_Panama <- glm(paste0("Panama ~ l_oth + dim1 + party + l_oth*party +",
                                labmkt_covars, " + ",
                                region_covars, " + ",
                                demographic_covars_new),
                         family = binomial(link = "probit"),
                         data = fta_rollcalls_112)

probit_112_Korea <- glm(paste0("Korea ~ l_oth + dim1 + party + l_oth*party +",
                               labmkt_covars, " + ",
                               region_covars, " + ",
                               demographic_covars_new),
                        family = binomial(link = "probit"),
                        data = fta_rollcalls_112)

probits_112th <- list(probit_112_Colombia,
                      probit_112_Panama,
                      probit_112_Korea)

# [17] Probits on Trade Adjustment Assistance and GSP----------------------------------------

# RH1101018, Trade and Globalization Adjustment Assistance Act of 2007
#	RH1120780, Extending the GSP (112th congress)

#This is more surprising. There is no evidence of a significant effect.

tgaa <- voteview_search("trade",
                         congress = c(110), chamber = "House") %>%
                         filter(id=="RH1101018") %>% 
                         extract_votedata_summary() %>% 
                            filter(party_code==200 | party_code==100) %>%
                            select(district,vname,vote,party_code,dim1) %>%
                            mutate(party_code=as.numeric(party_code)) %>% 
                            inner_join(imports_108,
                                       by=c("district"="district108")) %>%
                            mutate(party=ifelse(party_code==200,1,0)) %>%
                            mutate(l_oth=log(d_imp_otch_lag_pd), 
                                   l_imp=log(d_imp_usch_pd))              

gsp <- voteview_search("trade",
                        congress = c(112), chamber = "House") %>%
                        filter(id=="RH1120780") %>% 
                        extract_votedata_summary() %>% 
                        filter(party_code==200 | party_code==100) %>%
                        select(district,vname,vote,party_code,dim1) %>%
                        mutate(party_code=as.numeric(party_code)) %>% 
                        inner_join(imports_108,
                                   by=c("district"="district108")) %>%
                        mutate(party=ifelse(party_code==200,1,0)) %>%
                        mutate(l_oth=log(d_imp_otch_lag_pd), 
                               l_imp=log(d_imp_usch_pd))      


probit_tgaa_nolab <- glm(paste0("vote ~ l_oth + dim1 + party + l_oth*party +",
                               #labmkt_covars, " + ",
                               region_covars, " + ",
                               demographic_covars_new),
                        family = binomial(link = "probit"),
                        data = tgaa)

probit_gsp_nolab <- glm(paste0("vote ~ l_oth + dim1 + party + l_oth*party +",
                               #labmkt_covars, " + ",
                               region_covars, " + ",
                               demographic_covars_new),
                        family = binomial(link = "probit"),
                        data = gsp)

probit_tgaa <- glm(paste0("vote ~ l_oth + dim1 + party + l_oth*party +",
                                labmkt_covars, " + ",
                                region_covars, " + ",
                                demographic_covars_new),
                         family = binomial(link = "probit"),
                         data = tgaa)

probit_gsp <- glm(paste0("vote ~ l_oth + dim1 + party + l_oth*party +",
                               labmkt_covars, " + ",
                               region_covars, " + ",
                               demographic_covars_new),
                        family = binomial(link = "probit"),
                        data = gsp)

probits_tgaa_gsp <- list(probit_tgaa_nolab, probit_gsp_nolab, 
                         probit_tgaa, probit_gsp)


# [18] Stargazer Regression Outputs ----------------------------------------

#simple function to help output regression results to latex
models_out <- function(number){
  
  if(number==1){
    fta_table <- res_freetrade %>% select(congress, date, yea, nay, 
                                          short_description, vote_result) %>% 
      rename(Congress = congress, Date = date, Yeas = yea, Nays = nay, 
             Description = short_description, Result = vote_result)
    fta_table[13,5] <- "UNITED STATES-MEXICO-CANADA AGREEMENT"
    fta_table$Description <- gsub(paste0("\\(PASS\\)",collapse = "|"),"", fta_table$Description)
    fta_table %>% knitr::kable(caption = "House FTA Votes, 108th - 116th Congresses")
    
  }  #table of FTA bills
  
  else if(number==2){
    stargazer(olsmodels_108,type="latex",keep=c("dim1_","l_oth","partyR_"),
              keep.stat=c("adj.rsq", "F"),
              covariate.labels = c("log\\_d\\_imp", "Dim1\\_108th", "Party (R)", 
                                   "log\\_d\\_imp x Party (R)"),
              se = serrors(olsmodels_108),
              df=FALSE,
              font.size = "footnotesize",
              dep.var.caption = "",
              dep.var.labels.include = FALSE,
              digits=2,
              header = FALSE,
              no.space = TRUE,
              column.sep.width = "1pt",
              title="Specifications for 108th Congress",
              add.lines=list(
                c("Regional Controls","No","No","No","No","Yes","No","No","Yes"),
                c("Labor Market Controls","No","No","No","No","No","Yes","No","Yes"),
                c("Demographic Controls","No","No","No","No","No","No","Yes","Yes")))
  } #OLS for 108th Congress, building up specifications
  
  else if(number==3){
      stargazer(ols_108_112, type="latex",keep=c("dim1_","l_oth","partyR_"),
                keep.stat=c("adj.rsq", "F"),
                covariate.labels = c("log\\_d\\_imp", "Dim1", "Party (R)", "log\\_d\\_imp x Party (R)"),
                df=FALSE,
                digits=2,
                font.size = "small",
                dep.var.labels = c("108th","109th","112th"),
                header = FALSE,
                no.space = TRUE,
                column.sep.width = "1pt",
                title="Outcomes across the 108th, 109th and 112th Congresses",
                add.lines=list(
                  c("Regional Controls","Yes","Yes","Yes"),
                  c("Labor Market Controls","Yes","Yes","Yes"),
                  c("Demographic Controls","Yes","Yes","Yes")))
  } #OLS for 108th - 112th Congresses, full controls
  
  else if(number==4){
    
    stargazer(probits_110_116th,
              se = serrors(probits_110_116th),
              coef = marginal_effects(probits_110_116th),
              p = pvals(probits_110_116th),
              font.size = "small",
              type="latex",keep=c("l_oth","dim1","party"),
              no.space = TRUE,
              header = FALSE,
              model.numbers = FALSE,
              dep.var.caption = "Marginal effects at Mean",
              dep.var.labels.include = FALSE,
              column.labels = c("Peru","USMCA"),
              keep.stat=c("adj.rsq", "F"),
              covariate.labels = c("log\\_d\\_imp", "Dim1", "Party (R)", 
                                   "log\\_d\\_imp x Party (R)"),
              df=FALSE,
              title="Probits for FTA bills in 110th and 116th Congress",
              add.lines=list(
                c("Regional Controls","Yes","Yes"),
                c("Labor Market Controls","Yes","Yes"),
                c("Demographic Controls","Yes","Yes")))
  } #probits for 110th and 116th Congresses
  
  else if(number==5){
    
    stargazer(probits_108th,
              type="latex",
              keep=c("l_oth","dim1","party"),
              se = serrors(probits_108th),
              p = pvals(probits_108th),
              no.space = TRUE,
              font.size = "small",
              header = FALSE,
              coef = marginal_effects(probits_108th),
              dep.var.caption = "Marginal effects at Mean",
              dep.var.labels.include = FALSE,
              model.numbers = FALSE,
              covariate.labels = c("log\\_d\\_imp", "Dim1", "Party (R)",
                                   "log\\_d\\_imp x Party (R)"),
              column.labels = c("Singapore","Chile","Australia","Morocco"),
              keep.stat=c("adj.rsq", "F"),
              df=FALSE,
              title="Probits for individual FTA bills in 108th Congress",
              add.lines=list(
                c("Labor Market Controls", "No", "No", "No", "No", "No", "No"),
                c("Regional Controls","Yes","Yes","Yes","Yes","Yes","Yes"),
                c("Demographic Controls","Yes","Yes","Yes","Yes","Yes","Yes")))
    
  } #probits for 108th congress, individual votes
  
  else if(number==6){
    
    stargazer(probits_108th_2,
              type="latex",
              keep=c("l_oth","dim1","party","l_shind"),
              se = serrors(probits_108th_2),
              p = pvals(probits_108th_2),
              coef = marginal_effects(probits_108th_2),
              dep.var.caption = "Marginal effects at Mean",
              dep.var.labels.include = FALSE,
              model.numbers = FALSE,
              font.size = "small",
              no.space = TRUE,
              header = FALSE,
              column.labels = c("Singapore","Chile","Australia","Morocco"),
              keep.stat=c("adj.rsq", "F"),
              df=FALSE,
              covariate.labels = c("log\\_d\\_imp", "Dim1", "Party (R)",
                                   "\\% Manufacturing", "\\% Routine Jobs", "Offshorability",
                                   "log\\_d\\_imp x Party (R)"),
              title="Probits on 108th Congress with full labor market controls",
              add.lines=list(
                c("Labor Market Controls","Yes","Yes","Yes","Yes"),
                c("Regional Controls","Yes","Yes","Yes","Yes"),
                c("Demographic Controls","Yes","Yes","Yes","Yes")))
  } #same as 5 but with full labor market controls
  
  else if(number==7){
    stargazer(probits_112th,
              type="latex",
              keep=c("l_oth","dim1","party","l_shind"),
              se = serrors(probits_112th),
              p = pvals(probits_112th),
              font.size = "small",
              coef = marginal_effects(probits_112th),
              dep.var.labels.include = FALSE,
              dep.var.caption = "Marginal effects at Mean",
              model.numbers = FALSE,
              covariate.labels = c("log\\_d\\_imp", "Dim1", "Party (R)",
                                   "\\% Manufacturing", "\\% Routine Jobs", "Offshorability",
                                   "log\\_d\\_imp x Party (R)"),
              no.space = TRUE,
              header = FALSE,
              column.labels = c("Colombia","Panama","Korea"),
              keep.stat=c("adj.rsq", "F"),
              df=FALSE,
              title="Probits on 112th Congress with full labor market controls",
              add.lines=list(
                c("Labor Market Controls","Yes","Yes","Yes"),
                c("Regional Controls","Yes","Yes","Yes"),
                c("Demographic Controls","Yes","Yes","Yes")))
  } #probits for 112th congress with full controls
  
  else if(number==8){
    
    #probits on TGAA and GSP
    #with and without labor market controls
    
    stargazer(probits_tgaa_gsp,
              type="latex",
              keep=c("l_oth","dim1","party","l_shind"),
              se = serrors(probits_tgaa_gsp),
              p = pvals(probits_tgaa_gsp),
              font.size = "small",
              coef = marginal_effects(probits_tgaa_gsp),
              dep.var.labels.include = FALSE,
              model.numbers = FALSE,
              dep.var.caption = "Marginal effects at Mean",
              no.space = TRUE,
              digits=2,
              covariate.labels = c("log\\_d\\_imp", "Dim1", "Party (R)",
                                   "\\% Manufacturing", "\\% Routine Jobs", "Offshorability",
                                   "log\\_d\\_imp x Party (R)"),
              header = FALSE,
              column.labels = c("TGAA","Extend GSP", "TGAA", "Extend GSP"),
              keep.stat=c("adj.rsq", "F"),
              df=FALSE,
              title="Probits on other relevant legislation",
              add.lines=list(
                c("Labor Market Controls","No","No", "Yes", "Yes"),
                c("Regional Controls","Yes","Yes", "Yes", "Yes"),
                c("Demographic Controls","Yes","Yes", "Yes", "Yes")))
  } #probits on TGAA and GSP votes
  
  else if(number==9){
    qqplot <- regclass::qq(full_108_112_reg$l_oth,
                           ax = "log_d_imp") + 
      title("QQ plot for import competition intensity")
  } #qqplot for import concentration
  
}
