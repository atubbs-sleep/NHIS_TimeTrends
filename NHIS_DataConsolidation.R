# NHIS Data Consolidation - combine datafiles pulled by the DownloadMatrix
library(tidyverse)
library(haven)

# Load data
varsofinterest <- c("srvy_yr","wtfa_sa",
                    "strat","psu","sex",
                    "hispan_i","racerpi2","everwrk",
                    "age_p","r_maritl","par_stat","doinglwa","whynowka","all_sa",
                    "doinglw1","whynowk1",
                    "hyp","chl","chd","clhi",
                    "hrt","dib","dib","dibpre","wkdayr","bmi",
                    "sleep","asislpfl","asislpst","asislpmd",
                    "insomyr","fatigyr",
                    "asirest","dep","anx","fatyr","insyr")
samadult_combined <- read_dta("c:/users/atala/documents/nhis/data/2005/samadult.dta",col_select = contains(varsofinterest))
for (year in 2006:2018){
  fileaddress <- paste("c:/users/atala/documents/nhis/data/",year,"/samadult.dta",sep="")
  temp <- read_dta(fileaddress,col_select = contains(varsofinterest))
  samadult_combined <- full_join(samadult_combined,temp)
}
samadult_combined1 <- samadult_combined %>% transmute(
  Year = srvy_yr,
  Weights = wtfa_sa,
  Strata = case_when(is.na(stratum)==F ~ stratum,
                     is.na(stratum)==T & is.na(pstrat)==F ~ pstrat,
                     is.na(stratum)==T & is.na(pstrat)==T & is.na(strat_p)==F ~ strat_p),
  PSU = case_when(is.na(psu)==F ~ psu,
                  is.na(psu)==T & is.na(psu_p)==F ~ psu_p,
                  is.na(psu)==T & is.na(psu_p)==T & is.na(ppsu)==F ~ ppsu),
  Sleep = case_when(is.na(sleep)==F ~ sleep,
                    is.na(sleep)==T & is.na(asisleep)==F ~ asisleep),
  Sleep = if_else(Sleep>12,NA_real_,Sleep*60),
  Sex = factor(sex,1:2,c("Male","Female")),
  Age = factor(case_when(age_p<35 ~ "18-34",
                  age_p>34 & age_p<65 ~ "35-64",
                  age_p>64 ~ "65+"),levels = c("18-34","35-64","65+")),
  BMI = as.numeric(bmi),
  BMI = factor(case_when(
    BMI==1999.8 ~ NA_character_,
    BMI/2<185 ~ "Underweight",
    BMI/2>=185 & BMI/2<250 ~ "Normal",
    BMI/2>=250 & BMI/2<300 ~ "Overweight",
    BMI/2>=300 & BMI/2<400 ~ "Obese",
    BMI/2>=400 ~ "Morbidly Obese"),
    levels = c("Normal","Underweight","Overweight","Obese","Morbidly Obese")),
  Race = factor(racerpi2, 1:6, c("White","Black","AIAN","Asian","Other","Other")),
  Ethnicity = case_when(hispan_i==2|hispan_i==3~"Mexican Hispanic",
                        hispan_i==0~"Other Hispanic",hispan_i>3&hispan_i<12~"Other Hispanic",
                        hispan_i==12~"Non-Hispanic"),
  Race2 = factor(case_when(
    Ethnicity=="Mexican Hispanic"~"Mexican Hispanic",
    Ethnicity=="Other Hispanic"~"Other Hispanic",
    Race=="White" & Ethnicity=="Non-Hispanic"~"White",
    Race=="White" & is.na(Ethnicity)==T~"White",
    Race=="Black" & Ethnicity=="Non-Hispanic"~"Black",
    Race=="Black" & is.na(Ethnicity)==T~"Black",
    Race=="AIAN"  & Ethnicity=="Non-Hispanic"~"AIAN",
    Race=="AIAN"  & is.na(Ethnicity)==T~"AIAN",
    Race=="Asian" & Ethnicity=="Non-Hispanic"~"Asian",
    Race=="Asian" & is.na(Ethnicity)==T~"Asian",
    Race=="Other"~"Other"),
    levels = c("White","Black","Mexican Hispanic","Other Hispanic","AIAN","Asian","Other")),
  Employment = factor(case_when(doinglwa==1|doinglwa==4 ~ "Employed",
                         doinglwa==3 ~ "Unemployed",
                         doinglwa==2 | doinglwa==5 & whynowka==1 ~ "Homemaker",
                         doinglwa==2 | doinglwa==5 & whynowka==2 ~ "Student",
                         doinglwa==2 | doinglwa==5 & whynowka==3 ~ "Retired",
                         doinglwa==2 | doinglwa==5 & whynowka==4 ~ "Employed",
                         doinglwa==2 | doinglwa==5 & whynowka==5 ~ "Employed",
                         doinglwa==2 | doinglwa==5 & whynowka==6 ~ "Employed",
                         doinglwa==2 | doinglwa==5 & whynowka==7 ~ "Employed",
                         doinglwa==2 | doinglwa==5 & whynowka==8 ~ "Unemployed",
                         doinglwa==2 | doinglwa==5 & whynowka==9 ~ "unemployed",
                         doinglwa==2 | doinglwa==5 & whynowka==10 ~ "Unemployed",
                         doinglwa==2 | doinglwa==5 & whynowka==97 ~ "Unemployed",
                         doinglwa==2 | doinglwa==5 & whynowka==99 ~ "Unemployed",
                         doinglwa==7 | doinglwa==8|doinglwa==9 ~ NA_character_),
                      levels = c("Employed","Homemaker","Student","Retired","Unemployed")),
  MaritalStatus = factor(case_when(r_maritl==1 | r_maritl==2|r_maritl==3 ~ "Married",
                                   r_maritl==4 ~ "Widowed",
                                   r_maritl==5 | r_maritl== 6 ~ "Divorced/Separated",
                                   r_maritl==7 ~ "Never Married",
                                   r_maritl==8 ~ "Living with partner",
                                   r_maritl==9 ~ NA_character_),
                         levels = c("Never Married","Married","Widowed","Living with partner","Divorced/Separated")),
  Hypertension = factor(hypev,2:1,c("No","Yes")),
  CHD = factor(chdev,2:1,c("No","Yes")),
  CVD = factor(hrtev,2:1,c("No","Yes")),
  Diabetes = factor(case_when(dibev==2 | dibev1==2 ~ "No",
                              dibev==1 | dibev1==1 ~ "Yes"),
                              c("No","Yes")),
  PreDiabetes = factor(case_when(dibpre==1 | dibpre1==1 | dibpre2==1 ~ "Yes",
                                dibpre==2 | dibpre1==2 | dibpre2==2 ~ "No"),
                      levels = c("No","Yes")),
  Workloss = if_else(wkdayr>366,NA_real_,wkdayr)
)
write_rds(samadult_combined1,"c:/users/atala/sync/research/projects/to publish/nhis time trends/data/samadult_combined.rdata")

