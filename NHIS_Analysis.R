### The data used in this analysis were extracted from the CDC through their FTP website using the NHIS_DownloadMatrix.R code. 
### The data were then processed using the NHIS_DataConsolidation.R code. 
### The data were analyzed in the NHIS_analysis_10Jan2022.R code. 

### Load Packages
library(tidymodels)
library(tidyverse)
library(gtsummary)
library(ggpubr)
library(haven)
library(survey)
library(car)

### Load Data
nhis <- read_rds("c:/users/atala/sync/research/projects/to publish/nhis time trends/data/samadult_combined.rdata") %>%
  mutate(
    "BMI2" = factor(BMI,levels=c("Normal","Underweight","Overweight","Obese","Morbidly Obese"), # Define Obese
                    labels = c("< 30","< 30","< 30",">=30",NA_character_)),
    "BMI3" = factor(BMI,levels=c("Normal","Underweight","Overweight","Obese","Morbidly Obese"), # Define morbid obesity
                    labels = c("< 40","< 40","< 40","< 40",">=40")),
    "Sleephr" = scale(13-Sleep/60),
    "Race2" = factor(if_else(Race2=="AIAN","Other",as.character(Race2)),
                     c("White","Black","Mexican Hispanic","Other Hispanic","Asian","Other")),
    "Hypertension1" = as.numeric(Hypertension)-1,
    "CHD1" = as.numeric(CHD)-1,
    "Diabetes1" = as.numeric(Diabetes)-1,
    "PreDiabetes1" = as.numeric(PreDiabetes)-1,
    "BMI21" = as.numeric(BMI2)-1,
    "BMI31" = as.numeric(BMI3)-1,
    "SleepDuration" = factor(case_when(Sleep/60 < 7 ~ "Short Sleep",
                                Sleep/60>=7 & Sleep/60<9 ~ "Recommended Sleep",
                                Sleep/60>=9 ~ "Long Sleep"),
                             levels=c("Recommended Sleep","Short Sleep","Long Sleep")),
    "SleepDuration2" = case_when(Sleep/60 < 7 ~ 1,Sleep/60>=7 & Sleep/60<9 ~ 0,Sleep/60>=9 ~ NA_real_))

### Generate survey design object
nhis.design <- svydesign(ids = ~PSU, strata=~Strata, weights = ~Weights, data=nhis, nest = T)

#### Trends over Time ####
### Build by-Year Models
# All Years
nhis.m1.unadj <- svyglm(Sleep ~ Year, nhis.design, family="gaussian")
nhis.m1.adj <- svyglm(Sleep ~ Year+Age+Sex+Race2+BMI+Employment+MaritalStatus, nhis.design, family="gaussian")
# 2005 to 2010
nhis.m2.unadj <- svyglm(Sleep ~ Year, subset(nhis.design,Year %in% 2005:2010), family="gaussian")
nhis.m2.adj <- svyglm(Sleep ~ Year+Age+Sex+Race2+BMI+Employment+MaritalStatus, subset(nhis.design,Year %in% 2005:2010), family="gaussian")
# 2010 to 2018
nhis.m3.unadj <- svyglm(Sleep ~ Year, subset(nhis.design,Year %in% 2010:2018), family="gaussian")
nhis.m3.adj <- svyglm(Sleep ~ Year+Age+Sex+Race2+BMI+Employment+MaritalStatus, subset(nhis.design,Year %in% 2010:2018), family="gaussian")
# Combine into a table
tbl_stack(list(
  tbl_merge(list(
    tbl_regression(nhis.m1.unadj,include = "Year"),
    tbl_regression(nhis.m2.unadj,include = "Year"),
    tbl_regression(nhis.m3.unadj,include = "Year")
  )),
  tbl_merge(list(
    tbl_regression(nhis.m1.adj,include = "Year"),
    tbl_regression(nhis.m2.adj,include = "Year"),
    tbl_regression(nhis.m3.adj,include = "Year")
  ))
))
  
### Build Age*Year Models
covariates = c("Year*Race2","Sex + Age + BMI + Employment + MaritalStatus + Year*Race2")
modelnames = c("Unadjusted","Adjusted")
timerange = list(2005:2018,2005:2010,2010:2018)
timerange1 = c("2005-2018","2005-2010","2010-2018")
finalmodels <- list()
for(idx in 1:3){
  templist <- list()
  for(jdx in 1:2){
    templist[[jdx]] <- svyglm(as.formula(paste("Sleep ~ ",covariates[jdx],sep="")),
                              subset(nhis.design,Year %in% timerange[[idx]]),
                              family="gaussian")
  }
  finalmodels[[idx]] <- templist
}
### Build Table 1
table1 <- tibble()
for(idx in 1:3){
  for(jdx in 1:2){
    temptable <- bind_rows(
      bind_cols(tibble("Range" = timerange1[idx],"Model" = modelnames[jdx],"Race" = "White"),as_tibble(deltaMethod(finalmodels[[idx]][[jdx]],"Year"))),
      bind_cols(tibble("Range" = timerange1[idx],"Model" = modelnames[jdx],"Race" = "Black"),as_tibble(deltaMethod(finalmodels[[idx]][[jdx]],"Year + `Year:Race2Black`"))),
      bind_cols(tibble("Range" = timerange1[idx],"Model" = modelnames[jdx],"Race" = "Mexican Hispanic"),as_tibble(deltaMethod(finalmodels[[idx]][[jdx]],"Year + `Year:Race2Mexican Hispanic`"))),
      bind_cols(tibble("Range" = timerange1[idx],"Model" = modelnames[jdx],"Race" = "Other Hispanic"),as_tibble(deltaMethod(finalmodels[[idx]][[jdx]],"Year + `Year:Race2Other Hispanic`"))),
      bind_cols(tibble("Range" = timerange1[idx],"Model" = modelnames[jdx],"Race" = "Asian"),as_tibble(deltaMethod(finalmodels[[idx]][[jdx]],"Year + `Year:Race2Asian`"))),
    )
    table1 <- bind_rows(table1, temptable)
  }
}
# Format Table 1
table1.1 <- table1 %>% mutate(
  "Race" = factor(Race,levels = c("White","Black","Mexican Hispanic","Other Hispanic","Asian")),
  "Estimate" = round(Estimate,2),
  "SE" = NULL,
  "Lower" = round(`2.5 %`,2),
  "Upper" = round(`97.5 %`,2),
  "95% CI" = paste("[",Lower,", ",Upper,"]",sep=""),
  `2.5 %` = NULL, 
  `97.5 %` = NULL)
# write_csv(table1.1,"c:/users/atala/sync/research/projects/to publish/nhis time trends/table1.csv")

### Interaction effects
## 2005 to 2018
anova(svyglm(Sleep ~ BMI + Employment + MaritalStatus + Age + Race2 + Year * Sex,   nhis.design, family="gaussian")) ### Year:Sex  interaction p=0.0538
anova(svyglm(Sleep ~ BMI + Employment + MaritalStatus + Sex + Age   + Year * Race2, nhis.design, family="gaussian")) ### Year:Race interaction p<0.0001
nhis.race.m1 <- svyglm(Sleep ~ BMI + Employment + MaritalStatus + Sex + Age   + Year * Race2, nhis.design, family="gaussian")

## 2005 to 2010
anova(svyglm(Sleep ~ BMI + Employment + MaritalStatus + Age + Race2 + Year * Sex,   subset(nhis.design, Year %in% 2005:2010), family="gaussian")) ### Year:Sex  interaction p=0.0944
anova(svyglm(Sleep ~ BMI + Employment + MaritalStatus + Sex + Age   + Year * Race2, subset(nhis.design, Year %in% 2005:2010), family="gaussian")) ### Year:Race interaction p=0.1272

## 2010 to 2018
anova(svyglm(Sleep ~ BMI + Employment + MaritalStatus + Age + Race2 + Year * Sex,   subset(nhis.design, Year %in% 2010:2018), family="gaussian")) ### Year:Sex  interaction p=0.8791
anova(svyglm(Sleep ~ BMI + Employment + MaritalStatus + Sex + Age   + Year * Race2, subset(nhis.design, Year %in% 2010:2018), family="gaussian")) ### Year:Race interaction p=0.0200
nhis.race.m2 <- svyglm(Sleep ~ BMI + Employment + MaritalStatus + Sex + Age + Race2 * Year, subset(nhis.design, Year %in% 2010:2018), family="gaussian")

### Generate Figure 1
sleep.table <- svyby(~Sleep, ~Year, nhis.design,svymean,vartype="ci",na.rm=T) %>%
  transmute("Year" = factor(Year),
            "Sleep" = round(Sleep,2),
            "Lower" = round(ci_l,2),
            "Upper" = round(ci_u,2))
sleeptime.p <- ggplot(sleep.table,aes(x=Year,y=Sleep,ymin=Lower,ymax=Upper,group="black"))+
  geom_line() + geom_point() + geom_ribbon(alpha=0.3) + theme_pubr() + xlab("")+
  ylab("Sleep per day (minutes)")

race.table <- svyby(~Sleep, ~Year+Race2, nhis.design,svymean, vartype="ci",na.rm=T) %>% filter(Race2 != "Other") %>%
  transmute("Year" = factor(Year),
            "Race" = factor(Race2,levels = c("White","Black","Mexican Hispanic","Other Hispanic","Asian")),
            "Sleep" = round(Sleep,2), 
            "Lower" = round(ci_l,2), 
            "Upper" = round(ci_u,2),
            "Sig" = factor(case_when(Race=="White"|Race=="Black"|Race=="Mexican Hispanic" ~ 1,
                                     Race=="Other Hispanic"|Race=="Asian" ~ 0),
                           levels = 0:1)
  ) %>% as_tibble()
racetime.p <- 
  ggplot(race.table,aes(x=Year,y=Sleep,ymin=Lower,ymax=Upper,group=Race))+ 
  geom_point(aes(color=Race,alpha=Race),size=2)+
  geom_line(aes(color=Race,alpha=Race),size=1)+
  geom_ribbon(aes(fill=Race,alpha=Sig))+
  scale_alpha_manual(values = c(0,.3,.3,1,1,.3,1),guide=NULL)+ #values = c(Signo,Sigyes,Asian,Black,Mexican Hispanic,Other Hispanic,White)
  theme_pubr()+xlab("")+ylab("Sleep per day (minutes)")+
  theme(legend.position = "none")

# nhis.race.m1

racemodel.p <- ggplot(filter(table1.1,Model=="Adjusted" & Range=="2005-2018"),aes(x=Race,y=Estimate))+
  geom_col(aes(fill=Race,alpha = Race),color="black")+
  geom_errorbar(aes(ymin=Lower,ymax=Upper,alpha=Race),size=1,width=0.5)+
  geom_bracket(xmin=c(1,1),xmax=c(2,3),y.position = c(0.1,0.2),
               label = c("p = 0.0001","p < 0.0001"))+
  scale_alpha_manual(values = c(1,1,1,0.3,0.3))+
  xlab("")+ylab("Difference per year (minutes)")+
  theme_pubr(legend = "none")
  
figure1 <- ggarrange(nrow=2,ncol=1,labels = c("A",""),
                     sleeptime.p,
                     ggarrange(nrow=1,ncol=2,labels = c("B","C"),
                               racetime.p,
                               racemodel.p),
                     heights = c(.66,1),legend = "none")

# tiff("c:/users/atala/sync/research/projects/to publish/nhis time trends/figure1.tiff",
#     res=300,width=5000,height=3000)
# figure1
# dev.off()

### Prevalence of Sleep Duration Categories
## Generate Figure 2
sleepcat.tbl <- svyby(~SleepDuration, ~Year+Race2,nhis.design,svymean,vartype="ci",na.rm=T) %>%
  as_tibble() %>%
  pivot_longer(cols=3:11,names_to = "Label", values_to = "Prevalence") %>%
  mutate(Label = str_remove(Label,"SleepDuration"),
         Label = str_replace(Label,"ci_l.","Lower"),
         Label = str_replace(Label, "ci_u.","Upper"),
         Label = str_replace(Label, "Recommended Sleep","_Recommended Sleep"),
         Label = str_replace(Label, "Short Sleep","_Short Sleep"),
         Label = str_replace(Label, "Long Sleep","_Long Sleep")) %>%
  separate(Label, into=c("Effect","SleepDuration"),sep="_") %>%
  mutate(Race2 = factor(Race2, levels = c("White","Black","Mexican Hispanic","Other Hispanic","Asian")),
         Effect = case_when(Effect == "" ~ "Estimate",
                            Effect == "Lower" ~ "Lower",
                            Effect == "Upper" ~ "Upper")) %>%
  filter(is.na(Race2)==F) %>%
  pivot_wider(id_cols = c(Year,Race2,SleepDuration),names_from = Effect,values_from = Prevalence) %>%
  mutate("Sig" = factor(case_when(Race2=="White"|Race2=="Black"|Race2=="Mexican Hispanic" ~ 1,
                                  Race2=="Other Hispanic"|Race2=="Asian" ~ 0),
                        levels = 0:1))

sleepcat.p <- ggplot(sleepcat.tbl,aes(x=Year,y=Estimate,ymin=Lower,ymax=Upper))+
  geom_point(aes(color=Race2,alpha=Race2),size=2)+
  geom_line(aes(color=Race2,alpha=Race2),size=1)+
  geom_ribbon(aes(fill=Race2,alpha=Sig))+
  scale_alpha_manual(values = c(0,.3,.3,1,1,.3,1),guide=NULL)+ #values = c(Signo,Sigyes,Asian,Black,Mexican Hispanic,Other Hispanic,White)
  facet_wrap(~SleepDuration,nrow=1,ncol=3,strip.position = "bottom",scales = "free")+
  scale_y_continuous(labels = scales::percent)+theme_pubr()+
  theme(strip.background.x = element_blank(),strip.placement = "outside",
        legend.title = element_blank())+
  xlab("")+ylab("Prevalence")

# png("c:/users/atala/sync/research/projects/to publish/nhis time trends/figure2.png",
#      res=300, width=4000,height=2000)
# sleepcat.p
# dev.off()

# Quick Models
summary(svyglm(SleepDuration2 ~ Year,nhis.design,family='quasipoisson'))
summary(svyglm(SleepDuration2 ~ Year+Age+Sex+Race2+BMI+Employment+MaritalStatus,nhis.design,family='quasipoisson'))
# By sex and race interaction ANOVAs
anova(svyglm(SleepDuration2 ~ Age+BMI+Employment+MaritalStatus+Race2+Year*Sex,nhis.design,family='quasipoisson')) ### Year:Sex interaction p = 0.031
anova(svyglm(SleepDuration2 ~ Age+BMI+Employment+MaritalStatus+Sex+Year*Race2,nhis.design,family='quasipoisson')) ### Year:Race2 interaction p < 0.0001
# Quick interaction model
summary(svyglm(SleepDuration2 ~ Age+BMI+Employment+MaritalStatus+Sex+Year*Race2,nhis.design,family='quasipoisson'))

### Prevalence Risk of Cardiometabolic Disease as predicted by Sleephr
## Overall Effect
### Build by-Year Models
outcomes <- c("Hypertension1","CHD1","Diabetes1","PreDiabetes1","BMI21","BMI31")
outcomenames <- c("Hypertension","Coronary Artery Disease","Diabetes","Pre-Diabetes","Obesity", "Morbid Obesity")
covariates  <-  c("~Sleephr","~Sex + Age + Employment + MaritalStatus + Year + Race2 + Sleephr")
modelnames <- c("Unadjusted","Adjusted")

outcomes.m <- list()
for(idx in 1:length(outcomes)){
  for(jdx in 1:length(covariates)){
    templist[[jdx]] <- svyglm(as.formula(paste(outcomes[[idx]],covariates[[jdx]],sep="")),
                              nhis.design,
                              family="quasipoisson")
  }
  outcomes.m[[idx]] <- templist
}
# Overall outcomes  
table2.1 <- tbl_stack(group_header = outcomenames, list(
  tbl_merge(tab_spanner = modelnames,list(
    tbl_regression(outcomes.m[[1]][[1]],include = "Sleephr",exp=T),
    tbl_regression(outcomes.m[[1]][[2]],include = "Sleephr",exp=T)
  )),
  tbl_merge(tab_spanner = modelnames,list(
    tbl_regression(outcomes.m[[2]][[1]],include = "Sleephr",exp=T),
    tbl_regression(outcomes.m[[2]][[2]],include = "Sleephr",exp=T)
  )),
  tbl_merge(tab_spanner = modelnames,list(
    tbl_regression(outcomes.m[[3]][[1]],include = "Sleephr",exp=T),
    tbl_regression(outcomes.m[[3]][[2]],include = "Sleephr",exp=T)
  )),
  tbl_merge(tab_spanner = modelnames,list(
    tbl_regression(outcomes.m[[4]][[1]],include = "Sleephr",exp=T),
    tbl_regression(outcomes.m[[4]][[2]],include = "Sleephr",exp=T)
  )),
  tbl_merge(tab_spanner = modelnames,list(
    tbl_regression(outcomes.m[[5]][[1]],include = "Sleephr",exp=T),
    tbl_regression(outcomes.m[[5]][[2]],include = "Sleephr",exp=T)
  )),
  tbl_merge(tab_spanner = modelnames,list(
    tbl_regression(outcomes.m[[6]][[1]],include = "Sleephr",exp=T),
    tbl_regression(outcomes.m[[6]][[2]],include = "Sleephr",exp=T)
  ))
))

### Prevalence Risks of cardiometabolic disease associated with sleep duration by sex/race/ethnicity.
outcomes <- c("Hypertension1","CHD1","Diabetes1","PreDiabetes1","BMI21","BMI31")
outcomenames <- c("Hypertension","Coronary Artery Disease","Diabetes","Pre-Diabetes","Obesity","Morbid Obesity")
covariates  <-  c("~Sleephr*Race2","~Sex + Age + Employment + MaritalStatus + Year + Sleephr*Race2")
modelnames <- c("Unadjusted","Adjusted")

outcomes.race.m <- list()
for(idx in 1:length(outcomes)){
  for(jdx in 1:length(covariates)){
    templist[[jdx]] <- svyglm(as.formula(paste(outcomes[[idx]],covariates[[jdx]],sep="")),
                              nhis.design,
                              family="quasipoisson")
  }
  outcomes.race.m[[idx]] <- templist
}

## Build table 2
table2 <- tibble()
for(idx in 1:length(outcomes)){
  for(jdx in 1:length(covariates)){
    temptable <- bind_rows(
      bind_cols(tibble("Outcome" = outcomenames[idx],"Model" = modelnames[jdx],"Race" = "White"),as_tibble(deltaMethod(outcomes.race.m[[idx]][[jdx]],"exp(Sleephr)"))),
      bind_cols(tibble("Outcome" = outcomenames[idx],"Model" = modelnames[jdx],"Race" = "Black"),as_tibble(deltaMethod(outcomes.race.m[[idx]][[jdx]],"exp(Sleephr + `Sleephr:Race2Black`)"))),
      bind_cols(tibble("Outcome" = outcomenames[idx],"Model" = modelnames[jdx],"Race" = "Mexican Hispanic"),as_tibble(deltaMethod(outcomes.race.m[[idx]][[jdx]],"exp(Sleephr + `Sleephr:Race2Mexican Hispanic`)"))),
      bind_cols(tibble("Outcome" = outcomenames[idx],"Model" = modelnames[jdx],"Race" = "Other Hispanic"),as_tibble(deltaMethod(outcomes.race.m[[idx]][[jdx]],"exp(Sleephr + `Sleephr:Race2Other Hispanic`)"))),
      bind_cols(tibble("Outcome" = outcomenames[idx],"Model" = modelnames[jdx],"Race" = "Asian"),as_tibble(deltaMethod(outcomes.race.m[[idx]][[jdx]],"exp(Sleephr + `Sleephr:Race2Asian`)"))),
    )
    table2 <- bind_rows(table2, temptable)
  }
}
table2.2 <- table2 %>% mutate(
  "Race" = factor(Race,levels = c("White","Black","Mexican Hispanic","Other Hispanic","Asian")),
  "PR" = round(Estimate,2),
  "SE" = NULL,
  "Lower" = round(`2.5 %`,2),
  "Upper" = round(`97.5 %`,2),
  "95% CI" = paste("[",Lower,", ",Upper,"]",sep=""),
  `2.5 %` = NULL, 
  `97.5 %` = NULL,
  Estimate = NULL)
# write_csv(table2.2, "c:/users/atala/sync/research/projects/to publish/nhis time trends/table22.csv")
