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

##=================COLLECT AND CLEAN ROLL CALL DATA======================##

#generic free trade, trade agreements search from voteview api
res_freetrade <- voteview_search("'free trade'", 
                                 congress = c(108:117), chamber = "House") %>% 
                        filter(id!="RH1111537") %>% 
                        filter(bill_number!="HR644") %>% 
                        filter(question=="On Passage"|question=="Sustain the Ruling of the Chair")

res_extra <- voteview_search("trade", 
                              congress = c(108:117), chamber = "House") %>% 
  filter(id=="RH1101018"|id=="RH1120780"|id=="RH1140386"|id=="RH1121551"|id=="RH1091206"|id=="RH1140343")

#get detailed roll call data for the search results
rcall <- voteview_download(res_freetrade$id)

#convert roll call data from wide to long form
rc_long <- melt_rollcall(rcall,
                          legiscols = c("name", "state_abbrev","party_code", 
                                        "dim1", "dim2", "cqlabel"),
                          votecols = c("vname", "date", "congress", 
                                       "party_vote_counts.200.1", "party_vote_counts.200.6",
                                       "party_vote_counts.100.1", "party_vote_counts.100.6")) %>% 
  mutate(district=paste(state_abbrev,as.numeric(str_extract(cqlabel,"[:digit:]+")))) %>% 
  select(-cqlabel) %>% 
  mutate(vote=ifelse(vote==1,1, ifelse(vote==6,0,NA))) %>% #remove abstentions and recode yeas/nays as 1s and 0s
  mutate(party_code=replace(party_code, 
                            name == "SANDERS, Bernard", 100)) %>% #change Bernie Sanders from Independent to Democrat 
  mutate(R_yea=as.numeric(party_vote_counts.200.1), R_nay = as.numeric(party_vote_counts.200.6), #calculate party vote behaviour
         D_yea=as.numeric(party_vote_counts.100.1), D_nay = as.numeric(party_vote_counts.100.6)) %>% 
  select(-party_vote_counts.200.1,-party_vote_counts.200.6,
         -party_vote_counts.100.1,-party_vote_counts.100.6) %>% 
  mutate(R_yea = replace_na(R_yea, 0), R_nay = replace_na(R_nay, 0),
         D_yea = replace_na(D_yea, 0), D_nay = replace_na(D_nay, 0)) %>% 
  mutate(R_yea_vote = R_yea/(R_yea+R_nay), 
         D_yea_vote = D_yea/(D_yea+D_nay)) %>% 
  mutate(R_maj = ifelse(R_yea_vote>0.75, 1, ifelse(R_yea_vote<0.25,-1,0))) %>% #code party majority vote as >75% yeas (yea) or <25% yeas (nay)
  mutate(D_maj = ifelse(D_yea_vote>0.75, 1, ifelse(D_yea_vote<0.25,-1,0)))


##=============JOIN WITH TRADE AND DISTRICT x COUNTY DATA===================##

#import trade data and population shares in district x county cells
#then join with house voting and party data
trade_comp <- read_csv("house_2002_2016.csv") %>% 
  rename(fips=cty_fips) %>% 
  rename("district108"=congressionaldistrict) %>% 
  select(district108,fips,czone, #geographies
         starts_with("reg_"), #regional controls
         d_imp_usch_pd, d_imp_otch_lag_pd,  #import penetration intensity scores
         starts_with("l_sh"), #labor market composition controls
         sh_district_2002) %>%  #population share of district in 2002
  mutate(ctycd108 = 100*fips + #reformat district code for joins
           as.numeric(str_extract(district108,
                                    "[:digit:]+"))) %>% 
  rename(sh_district_108=sh_district_2002) %>% 
  relocate(32)

#validation
#Check that all county x district cell population shares sum to one for each district
trade_comp %>% 
  group_by(district108) %>% 
  summarise(pop_total=round(sum(sh_district_108),2)) %>% 
  select(pop_total) %>% table()
  

##==============MAKE ROLL CALL OBJECTS FOR EACH CONGRESS==============##

process_rc_congress <- function(rc_long_object, congress_number){
  rc_long_object %>% 
    filter(congress==congress_number) %>% 
    group_by(district) %>% 
    summarise(vote=mean(vote,na.rm=TRUE), 
              party_code = mean(as.numeric(party_code)),
              dim1=mean(dim1,na.rm=TRUE), dim2=mean(dim2,na.rm=TRUE),
              R_party_vote = mean(R_yea_vote, na.rm=TRUE),
              D_party_vote = mean(D_yea_vote, na.rm=TRUE)) %>% 
    mutate(diffparty = ifelse(party_code==200, vote-R_party_vote, vote-D_party_vote))
}

rc_108 <- rc_long %>% process_rc_congress(108)
rc_109 <- rc_long %>% process_rc_congress(109)
rc_110 <- rc_long %>% process_rc_congress(110)
rc_112 <- rc_long %>% process_rc_congress(112)
rc_116 <- rc_long %>% process_rc_congress(116)


##==============COLLAPSE TRADE AND CONTROL DATA BY DISTRICT==============##

#get mean of import penetration and control variables for each cell
#then aggregate to district level weighting by each cell's population share in the district

trade_comp_108 <- trade_comp %>% 
  group_by(district108) %>% 
  summarise(across(reg_midatl:l_sh_pop_hispanic, 
                   ~ stats::weighted.mean(.x, w=sh_district_108)))

##focus on the 113th congress redistricting, so bunch into two groups:
## 1 - 108th to 112th congress;
## 2 - 113th to 117th congress;

#join the roll-call data for the 1st group (108th to 112th congress)
full_108_112 <- trade_comp_108 %>%
  inner_join(rc_108, by=c("district108"="district")) %>% 
  inner_join(rc_109, by=c("district108"="district"),suffix=c("","_109")) %>% 
  inner_join(rc_110, by=c("district108"="district"),suffix=c("","_110")) %>% 
  inner_join(rc_112, by=c("district108"="district"),suffix=c("","_112"))

full_108_112 <- full_108_112 %>% 
  mutate(avg_vote = rowMeans(full_108_112[c(50,43,36,29)],na.rm=TRUE)) #avg vote across 108th-112th congresses
         
#make long-form full data set for 108th to 112th congresses
full_108_112_long <- mutate(rc_108, congress=108) %>% 
                        rbind(mutate(rc_109, congress=109)) %>% 
                        rbind(mutate(rc_110, congress=110)) %>% 
                        rbind(mutate(rc_112, congress=112)) %>% 
                     inner_join(trade_comp_108, by=c("district"="district108"))
                    
##==================REDISTRICTING FOR THE 113th CONGRESS======================##

#redistricted data for the 2nd group (113th to 117th congress)

#import district crosswalks with population share mappings by old district to new 
cwd113 <- read_csv("ctycd_crosswalks/ctycd113.csv") %>% 
  rename(district108 = congressionaldistrict108, district113 = congressionaldistrict113)

#calculate population shares of new (county x district) cells in new districts
cong_113 <<- trade_comp %>% 
          left_join(select(cwd113, -district108), by=c("ctycd108"="ctycd108")) %>% 
              rename(afact108_113 = afact_ctycd108_ctycd113) %>% 
              select(-cty_fips_2000, 
                     -cty_fips_2010,
                     -state_fips,
                     -afact_ctycd113_ctycd108) %>% 
              mutate(district113 = ifelse(is.na(district113), district108, district113),
                     ctycd113 = ifelse(is.na(ctycd113),ctycd108,ctycd113), 
                     afact108_113 = ifelse(is.na(afact108_113),1,afact108_113),
                     pop_113 = sh_district_108*afact108_113) %>% 
              group_by(district113) %>% 
              mutate(distpop_113 = sum(pop_113)) %>% 
              ungroup() %>%
              mutate(sh_district_113 = pop_113/distpop_113) %>% #variable of interest, sh_district_113
              select(-distpop_113, -fips, -czone, -pop_113, -district108)

#validation for population shares
#Check that all county x district cell population shares sum to one for each district
cong_113 %>% 
  group_by(district113) %>% 
  summarise(sum(sh_district_113)) %>%
  select(`sum(sh_district_113)`) %>% 
  table()

#collapse by district means, weighted by the product of 
#(share of old cell population mapping to new cell) x 
#(share of new cell population in new district population)
trade_comp_113 <- cong_113 %>% 
  group_by(district113) %>% 
  summarise(across(reg_midatl:l_sh_pop_hispanic, 
                   ~ stats::weighted.mean(.x, w=sh_district_113*afact108_113)))

#generate full dataset for 113th to 116th congresses (only one FTA, in the 116th congress, the USMCA)
full_113_116 <- trade_comp_113 %>%
  inner_join(rc_116, by=c("district113"="district")) %>% 
  mutate(congress=116) %>% 
  rename(district=district113)

##====================PRELIMINARY PLOTS (EDA)==========================##

full_108_112_long %>% 
  filter(party_code==200 | party_code==100) %>% 
  ggplot() +
  aes(y=vote, x=log(d_imp_usch_pd), col=as.factor(party_code)) +
    geom_point() +
    geom_smooth(method="lm", formula=y~x) +
       scale_color_manual(name = "Party", 
          labels = c("D", "R"), 
          values=c("blue", "red")) +
     ylab("Average vote") +
     xlab("District change in Chinese import penetration after 2001") +
     ggtitle("108th, 109th, 110th and 112th Congress", 
          subtitle = "Import intensity vs house voting outcomes on FTAs") +
     facet_wrap(.~congress) + 
     theme(strip.background =element_rect(fill="grey46")) + 
     theme(strip.text = element_text(colour = 'white',face="bold"))

full_113_116 %>% 
  filter(party_code==200 | party_code==100) %>% 
  ggplot() +
  aes(y=vote, x=log(d_imp_usch_pd), col=as.factor(party_code)) +
  geom_point() +
  geom_smooth(method="lm", formula=y~x) +
  scale_color_manual(name = "Party", 
                     labels = c("D", "R"), 
                     values=c("blue", "red")) +
  ylab("Average vote") +
  xlab("District change in Chinese import penetration after 2001") +
  ggtitle("116th Congress - USMCA", 
          subtitle = "Import intensity vs house voting outcome on USMCA")

##what about polarization? we can plot the average nominate scores over time.
all_congresses <- full_108_112_long %>% 
  rbind(full_113_116) %>% 
  group_by(district, congress) %>% 
  summarise(across(vote:diffparty, ~mean(.x, na.rm=TRUE)))

#polarization between parties
all_congresses %>% 
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
all_congresses %>% 
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

#does voting behaviour change by high vs low import penetration?
#see for 108th-112th congress, before redistricting
full_108_112_long %>% 
  mutate(imp_high = log(d_imp_usch_pd)>=median(log(d_imp_usch_pd))) %>% 
  group_by(imp_high, congress, party_code) %>% 
  filter(party_code==100 | party_code==200) %>%
  summarise(party_code=ifelse(mean(party_code)==100,"Democrat","Republican"),
            vote=mean(vote,na.rm=TRUE)) %>% 
  ggplot() + 
  aes(y=vote, x=congress, fill=imp_high) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(.~party_code) +
  scale_fill_manual(name = "Import Penetration in District", 
                     labels = c("Low", "High"), 
                     values=c("pink","deeppink3")) +
  ylab("Average vote") +
  xlab("Congress") +
  theme(legend.position="bottom") +
  ggtitle("Average FTA vote vs High/Low Import Penetration",
          subtitle = "108th, 109th, 110th and 112th Congresses")


##=================REGRESSION ANALYSIS (OVERALL)=======================## 

#import macro-level data on congresses 
macro_congress <- read_csv("congress_info.csv")

#inspect histogram of import data
#impose log transform
hist(full_108_112$d_imp_otch_lag_pd)
hist(full_108_112_reg$l_oth)


#recode variables for regression
full_108_112_reg <- full_108_112 %>% 
  filter(party_code==200 | party_code==100) %>% 
  mutate(partyR_108 = ifelse(party_code==200,1,0)) %>% 
  mutate(partyR_109 = ifelse(party_code_109==200,1,0)) %>% 
  mutate(partyR_110 = ifelse(party_code_110==200,1,0)) %>% 
  mutate(partyR_112 = ifelse(party_code_112==200,1,0)) %>% 
  rename(vote_108=vote,dim1_108=dim1, dim2_108=dim2, 
         R_party_vote_108=R_party_vote, D_party_vote_108=D_party_vote,
         diffparty_108=diffparty, avg_vote_108_112=avg_vote) %>% 
  mutate(l_imp = log(d_imp_usch_pd), l_oth = log(d_imp_otch_lag_pd)) %>% 
  select(-party_code, -party_code_109, -party_code_110, -party_code_112)

full_113_116_reg <- full_113_116 %>% 
  filter(party_code==200 | party_code==100) %>% 
  mutate(partyR_116 = ifelse(party_code==200,1,0)) %>% 
  rename(vote_116=vote,dim1_116=dim1, dim2_116=dim2, 
         R_party_vote_116=R_party_vote, D_party_vote_116=D_party_vote,
         diffparty_116=diffparty) %>%
  mutate(l_imp = log(d_imp_usch_pd), l_oth = log(d_imp_otch_lag_pd)) %>%
  select(-party_code)

#now we can analyse the results using a multiple regression framework
model1 <- lm(formula=vote_108 ~ l_oth,data=full_108_112_reg) 
summary(model1)
model2 <- lm(formula=vote_108 ~ l_oth +
               dim1_108, data=full_108_112_reg) 
summary(model2)
model3 <- lm(formula=vote_108 ~ l_oth +
               dim1_108 +
               partyR_108, data=full_108_112_reg) 
summary(model3)

model4 <- lm(formula=vote_108 ~ l_oth +
               dim1_108 +
               partyR_108 +
               l_oth*partyR_108, data=full_108_112_reg) 
summary(model4)
model5 <- lm(formula=vote_108 ~ l_oth +
               dim1_108 +
               partyR_108 +
               l_oth*partyR_108 +
               reg_midatl + reg_encen + reg_wncen + reg_satl + 
               reg_escen + reg_wscen + reg_mount + reg_pacif,
               data=full_108_112_reg) 
summary(model5)
model6 <- lm(formula=vote_108 ~ l_oth +
               dim1_108 +
               partyR_108 +
               l_oth*partyR_108 +
               l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
               l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
               l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
               l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
               l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic,
               data=full_108_112_reg) 
summary(model6)  
model7 <- lm(formula=vote_108 ~ l_oth +
               dim1_108 +
               partyR_108 +
               l_oth*partyR_108 +
               reg_midatl + reg_encen + reg_wncen + reg_satl + 
               reg_escen + reg_wscen + reg_mount + reg_pacif +
               l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
               l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
               l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
               l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
               l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic,
               data=full_108_112_reg) 
summary(model7)

#does this effect sustain through the 109th, 110th and 112th congresses?

model7_108th <-lm(formula=vote_108 ~ l_oth +
                    dim1_k +
                    partyR_k +
                    l_oth*partyR_k +
                    reg_midatl + reg_encen + reg_wncen + reg_satl + 
                    reg_escen + reg_wscen + reg_mount + reg_pacif +
                    l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
                    l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
                    l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
                    l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
                    l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic,
                  data=full_108_112_reg %>% 
                        rename(partyR_k = partyR_108) %>% 
                        rename(dim1_k = dim1_108))

summary(model7_108th) #same model as before, reformatted for better output

model8_109th <-lm(formula=vote_109 ~ l_oth +
                    dim1_k +
                    partyR_k +
                    l_oth*partyR_k +
                    reg_midatl + reg_encen + reg_wncen + reg_satl + 
                    reg_escen + reg_wscen + reg_mount + reg_pacif +
                    l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
                    l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
                    l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
                    l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
                    l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic,
                    data=full_108_112_reg %>% 
                            rename(partyR_k = partyR_109) %>% 
                            rename(dim1_k = dim1_109))

summary(model8_109th) #yes it does, but it is weaker. 

model10_112th <-lm(formula=vote_112 ~ l_oth +
                    dim1_k +
                    partyR_k +
                    l_oth*partyR_k +
                    reg_midatl + reg_encen + reg_wncen + reg_satl + 
                    reg_escen + reg_wscen + reg_mount + reg_pacif +
                    l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
                    l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
                    l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
                    l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
                    l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic,
                  data= full_108_112_reg %>% 
                          rename(partyR_k = partyR_112) %>% 
                          rename(dim1_k = dim1_112)) 

summary(model10_112th) #it is again statistically significant.

#ignore the 110th, 116th because there is only one vote.

#plot stargazer
stargazer(model1,
          model2,
          model3,
          model4,
          model5,
          model6,
          model7,type="latex",keep=c("dim1_","l_oth","partyR_"),
          keep.stat=c("adj.rsq", "F"),
          covariate.labels = c("log\\_d\\_imp", "Dim1\\_108th", "Party (R)", "log\\_d\\_imp x Party (R)"),
          df=FALSE,
          digits=2,
          title="Specifications for 108th Congress",
          add.lines=list(
             c("Regional Controls","No","No","No","No","Yes","No","Yes"),
             c("Labor Market Controls","No","No","No","No","No","Yes","Yes"),
             c("Demographic Controls","No","No","No","No","No","Yes", "Yes")))

stargazer(model7_108th,
          model8_109th,
          model10_112th,type="latex",keep=c("dim1_","l_oth","partyR_"),
          keep.stat=c("adj.rsq", "F"),
          covariate.labels = c("log\\_d\\_imp", "Dim1\\_kth", "Party (R)", "log\\_d\\_imp x Party (R)"),
          df=FALSE,
          digits=2,
          title="Outcomes across Congresses",
          add.lines=list(
            c("Regional Controls","Yes","Yes","Yes"),
            c("Labor Market Controls","Yes","Yes","Yes"),
            c("Demographic Controls","Yes","Yes","Yes")))
         

#Finally, test the model on the USMCA with a probit/logit.
#Can do the same thing with 110th congress.
#Look at the nominate cutpoints.
#Then have a look at TAARA.
#Robustness check
#Summary and policy implication.


#And see whether the shock has any influence on TAARA.

##================PROBIT ON INDIVIDUAL OUTCOMES (TAARA)/TAA+=================##
  
probit_110th_1 <- glm(vote_110 ~ l_oth, 
                  family = binomial(link = "probit"), 
                  data = full_108_112_reg)

coeftest(probit_110th_1, vcov. = vcovHC, type = "HC1")

probit_110th_2 <- glm(vote_110 ~ l_oth + dim1_110 + 
                        partyR_110,
                    family = binomial(link = "probit"), 
                    data = full_108_112_reg)

coeftest(probit_110th_2, vcov. = vcovHC, type = "HC1")

probit_110th_3 <- glm(vote_110 ~ l_oth + dim1_k + partyR_k +
                        l_oth*partyR_k + 
                        l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
                        l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
                        l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
                        l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
                        l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic,
                      family = binomial(link = "probit"), 
                      data = full_108_112_reg %>% 
                        rename(partyR_k = partyR_110) %>% 
                        rename(dim1_k = dim1_110))

cf110th <- coeftest(probit_110th_3, vcov. = vcovHC, type = "HC1")

probit_116th <- glm(vote_116 ~ l_oth + dim1_k + partyR_k +
                        l_oth*partyR_k + 
                        l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
                        l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
                        l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
                        l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
                        l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic,
                      family = binomial(link = "probit"), 
                      data = full_113_116_reg %>% 
                      rename(partyR_k = partyR_116) %>% 
                      rename(dim1_k = dim1_116))
cf116th <- coeftest(probit_116th, vcov. = vcovHC, type = "HC1")

stargazer(probit_110th_3,
          probit_116th,
          type="latex",keep=c("l_oth","dim1","party"),
          coef = list(cf110th[,1],cf116th[,1]),
          se = list(cf110th[,2],cf116th[,2]),
          p = list(cf110th[,4],cf116th[,4]),
          model.numbers = FALSE,
          column.labels = c("Peru","USMCA"),
          keep.stat=c("adj.rsq", "F"),
          covariate.labels = c("log\\_d\\_imp", "Dim1\\_kth", "Party (R)", "log\\_d\\_imp x Party (R)"),
          df=FALSE,
          title="Probits for FTA bills in 110th and 116th Congress",
          add.lines=list(
            c("Regional Controls","Yes","Yes"),
            c("Labor Market Controls","Yes","Yes"),
            c("Demographic Controls","Yes","Yes")))


##Try probit estimation on 108th votes individually? 
#use full controls in each specification
rc_108_all <- rc_long %>% filter(congress==108) %>% 
  select(district,vname,vote,party_code,dim1) %>%
  mutate(party_code=as.numeric(party_code)) %>% 
  filter(party_code==200 | party_code==100) %>% 
  spread(vname,vote) %>% 
  inner_join(trade_comp_108,by=c("district"="district108")) %>% 
  mutate(party=ifelse(party_code==200,1,0)) %>% 
  mutate(l_oth=log(d_imp_otch_lag_pd), l_imp=log(d_imp_usch_pd))

probit_108th_RH1080409 <- glm(RH1080409 ~ l_oth + dim1 + party +
                      l_oth*party + 
                        reg_midatl + reg_encen + reg_wncen + reg_satl + 
                        reg_escen + reg_wscen + reg_mount + reg_pacif +
                        l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
                        l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
                        l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
                        l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
                        l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic,
                    family = binomial(link = "probit"), 
                    data = rc_108_all)

cf1080409 <- coeftest(probit_108th_RH1080409, vcov. = vcovHC, type = "HC1") #significant

probit_108th_RH1080430 <- glm(RH1080430 ~ l_oth + dim1 + party +
                                l_oth*party + 
                                reg_midatl + reg_encen + reg_wncen + reg_satl + 
                                reg_escen + reg_wscen + reg_mount + reg_pacif +
                                l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
                                l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
                                l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
                                l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
                                l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic,
                              family = binomial(link = "probit"), 
                              data = rc_108_all)

cf1080430 <- coeftest(probit_108th_RH1080430, vcov. = vcovHC, type = "HC1") #significant 10%

probit_108th_RH1080434 <- glm(RH1080434 ~ l_oth + dim1 + party +
                                l_oth*party + 
                                reg_midatl + reg_encen + reg_wncen + reg_satl + 
                                reg_escen + reg_wscen + reg_mount + reg_pacif +
                                l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
                                l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
                                l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
                                l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
                                l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic,
                              family = binomial(link = "probit"), 
                              data = rc_108_all)

cf1080434 <- coeftest(probit_108th_RH1080434, vcov. = vcovHC, type = "HC1") #significant 5%

probit_108th_RH1081049 <- glm(RH1081049 ~ l_oth + dim1 + party +
                                l_oth*party + 
                                reg_midatl + reg_encen + reg_wncen + reg_satl + 
                                reg_escen + reg_wscen + reg_mount + reg_pacif +
                                l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
                                l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
                                l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
                                l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
                                l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic,
                              family = binomial(link = "probit"), 
                              data = rc_108_all)

cf1081049 <- coeftest(probit_108th_RH1081049, vcov. = vcovHC, type = "HC1") #significant 5%

probit_108th_RH1081087 <- glm(RH1081087 ~ l_oth + dim1 + party +
                                l_oth*party + 
                                reg_midatl + reg_encen + reg_wncen + reg_satl + 
                                reg_escen + reg_wscen + reg_mount + reg_pacif +
                                l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
                                l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
                                l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
                                l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
                                l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic,
                              family = binomial(link = "probit"), 
                              data = rc_108_all)

cf1081087 <- coeftest(probit_108th_RH1081087, vcov. = vcovHC, type = "HC1") #significant 5%

ses <- list(cf1080409[,2], cf1080430[,2], cf1080434[,2], cf1081049[,2], cf1081087[,2])
coefs <- list(cf1080409[,1], cf1080430[,1], cf1080434[,1], cf1081049[,1], cf1081087[,1])
ps <- list(cf1080409[,4], cf1080430[,4], cf1080434[,4], cf1081049[,4], cf1081087[,4])

stargazer(probit_108th_RH1080409,
          probit_108th_RH1080430,
          probit_108th_RH1080434,
          probit_108th_RH1081049,
          probit_108th_RH1081087,
          type="latex",keep=c("l_oth","dim1","party"),
          se = ses,
          p = ps,
          coef = coefs,
          model.numbers = FALSE,
          column.labels = c("CAFTA","Singapore","Chile","Australia","Morocco"),
          keep.stat=c("adj.rsq", "F"),
          covariate.labels = c("log\\_d\\_imp", "Dim1\\_108th", "Party (R)", "log\\_d\\_imp x Party (R)"),
          df=FALSE,
          title="Probits for individual FTA bills in 108th Congress",
          add.lines=list(
            c("Regional Controls","Yes","Yes","Yes","Yes","Yes","Yes"),
            c("Labor Market Controls","Yes","Yes","Yes","Yes","Yes","Yes"),
            c("Demographic Controls","Yes","Yes","Yes","Yes","Yes","Yes")))

#Marginal Effects#
#Repeat for each bill; pull dF/dx in the first column, ignore SEs
#dF/dx calculated at the mean by default
probitmfx(formula = RH1081087 ~ l_oth + dim1 + party +
            l_oth*party + 
            reg_midatl + reg_encen + reg_wncen + reg_satl + 
            reg_escen + reg_wscen + reg_mount + reg_pacif +
            l_sh_routine33 + l_sh_pop_f + l_sh_pop_edu_c + l_sh_fborn +
            l_sh_pop_age_1019 + l_sh_pop_age_2029 + l_sh_pop_age_3039 + 
            l_sh_pop_age_4049 + l_sh_pop_age_5059 + l_sh_pop_age_6069 + 
            l_sh_pop_age_7079 + l_sh_pop_age_8000 + 
            l_sh_pop_white + l_sh_pop_asian + l_sh_pop_black + l_sh_pop_hispanic, 
            data = rc_108_all)

marginal_effects_108th <- data.frame("Variable" = c("log_d_imp", "Dim1_108th", "Party (R)", "log_d_imp x Party (R)"),
                                     "RH1080409" = c(-2.4553e-08, 4.2612e-07, -7.2585e-09, 2.9320e-08),
                                     "RH1080430" = c(-0.1353802, 0.2136635, 0.4574885, -0.3143104),
                                     "RH1080434" = c(-0.1600904, 0.3511425, 0.3783547, -0.2718014),
                                     "RH1081049" = c(-0.112965, 0.282475, 0.156483, -0.026264),
                                     "RH1081087" = c(-0.0916708, 0.2338009, 0.1542683, -0.2495447))

marginal_effects_108th %>% knitr::kable(format="latex",
                                        digits=2)
                                                                     


