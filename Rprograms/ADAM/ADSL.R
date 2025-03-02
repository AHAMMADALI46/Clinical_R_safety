#File name: ADSL
# Author: Ali
# Date: 19FEB2025
# Project/Study:
# Description: <To Develop The ADSL dataset>
# Input: SDTM DM,
# Output: AdaM ADSL datasets
# Macros used: <No Macro used>
#   
# Modification History:
# <DD-MON-YYYY>, <Firstname Lastname>
# <Description>

#---------------------------------------------------------------

rm(list=ls())
library(haven)
library(tidyverse)
library(dplyr)
library(lubridate)
library(admiral)

setwd("//Users//muhammed//R clinical ")
getwd()

path_raw <- "//Users//muhammed//R clinical //Doc//rawdatasets//"
path_sdtm <- "//Users//muhammed//R clinical //Doc//SDTM//"
vs <- read_sas(paste0(path_sdtm, "vs.sas7bdat"))
ex <- read_sas(paste0(path_sdtm, "ex.sas7bdat"))

#calling SDTM
dm <- read_sas(paste0(path_sdtm, "dm.sas7bdat"))
suppdm <- read_sas(paste0(path_sdtm, "suppdm.sas7bdat")) %>% 
pivot_wider(id_cols = "USUBJID",
            names_from = "QNAM",
            values_from = "QVAL")

dm1 <- left_join(dm, suppdm, by="USUBJID") 

#****HT WT BMI*****

ht <- vs %>% 
  filter(VSTESTCD =="HEIGHT") %>% 
  select(USUBJID, VSSTRESN, VSSTRESU)
wt <- vs %>% 
  filter(VSTESTCD=="WEIGHT") %>% 
  select(USUBJID, VSSTRESN, VSSTRESU)

bmi <- vs %>% 
  filter(VSTESTCD=="BMI") %>% 
  select(USUBJID, VSSTRESN, VSSTRESU)

ht1 <- ht %>% 
  mutate(
    HEIGHT = as.numeric(VSSTRESN),
    HEIGHTU = VSSTRESU
  ) %>% 
select(USUBJID, HEIGHT, HEIGHTU)

wt1 <- wt %>% 
  mutate(
    WEIGHT = as.numeric(VSSTRESN),
    WEIGHTU = VSSTRESU
  ) %>% 
  select(USUBJID, WEIGHT, WEIGHTU)

bmi1 <- bmi %>% 
  mutate(
    BMI = as.numeric(VSSTRESN),
    BMIU = VSSTRESU
  ) %>% 
  select(USUBJID, BMI, BMIU)

ht1<-ht1 %>%  arrange(USUBJID)
wt1<-wt1 %>%  arrange(USUBJID)
bmi1<-bmi1 %>%  arrange(USUBJID)

dm2 <- left_join(dm1, ht1, by="USUBJID")
dm3 <- left_join(dm2, wt1, by= "USUBJID")
dm4 <- left_join(dm3, bmi1, by= "USUBJID")

#Get the exposure dataset
ex1 <- ex %>% 
  group_by(USUBJID) %>% 
  slice_head(n=1) %>% 
  ungroup() %>% 
  select(USUBJID, EXTRT, EXDOSE, EXSTDTC, EXENDTC)

ex2 <- ex %>% 
  group_by(USUBJID) %>% 
  slice_tail(n=1) %>% 
  ungroup() %>% 
  select(USUBJID, EXTRT, EXDOSE, EXSTDTC, EXENDTC) %>% 
  rename(EXTRT2 = EXTRT, EXDOSE2 = EXDOSE, EXSTDTC2 = EXSTDTC, EXENDTC2 = EXENDTC)

#Merge dm4, ex1, ex2
dm5 <- dm4 %>% 
  left_join(ex1, by="USUBJID") %>% 
  left_join(ex2, by="USUBJID")

#Assign treatment groups
dm6 <- dm5 %>% 
  mutate(
    TRT01P = case_when(
      ARMCD == "T-R" ~ "Oxybutynin_TEST",
      ARMCD == "R-T" ~ "Oxybutynin_REFE",
      TRUE ~ NA_character_
    ),

    TRT01PN = case_when(
      ARMCD == "T-R" ~ 1,
      ARMCD == "R-T" ~ 2,
      TRUE ~ NA_real_
    ),
    TRT02P = case_when(
      ARMCD == "T-R" ~ "Oxybutynin_REFE",
      ARMCD == "R-T" ~ "Oxybutynin_TEST",
      TRUE ~ NA_character_
    ),
    TRT02PN = case_when(
      ARMCD == "T-R" ~ 1,
      ARMCD == "R-T" ~ 2,
      TRUE ~ NA_real_
    ),
    TRT01A = case_when(
      EXTRT== "TEST" ~ "Oxybutynin_TEST",
      EXTRT == "REF" ~ "Oxybutynin_REFE",
      TRUE ~ NA_character_
    ),
    TRT01AN = case_when(
      EXTRT == "TEST" ~ 1,
      EXTRT == "REF" ~ 2,
      TRUE ~ NA_real_
    ),
    TRT02A = case_when(
      EXTRT2== "REFE" ~ "Oxybutynin_REFE",
      EXTRT2 == "TEST" ~ "Oxybutynin_TEST",
      TRUE ~ NA_character_
    ),
    TRT02AN = case_when(
      EXTRT2 == "REFE" ~ 1,
      EXTRT2 == "TEST" ~ 2,
      TRUE ~ NA_real_
    ),
    TRT01SDTM = EXSTDTC,
    TRT02SDTM = EXSTDTC2
  )

#Final dataset DM6 with flags
dm7 <- dm6 %>% 
  mutate(
    SEXN= case_when(
      SEX=="M" ~ 1,
      SEX=="F" ~ 2,
      TRUE ~ NA_real_
    ),
    
    RACEN= case_when(
      RACE=="ASIAN" ~ 1,
      RACE == "OTHER" ~ 2,
      TRUE ~ NA_real_
    ),
    
    SAFFL = ifelse(!is.na(EXDOSE) & EXSTDTC !="", "Y", "N"),
    ITTFL = ifelse(ARMCD !="", "Y", "N"),
    ENRFL = ifelse(RFICDTC != "", "Y", "N")
  ) %>% 
  select(STUDYID,
         USUBJID,
         SUBJID,
         SITEID,
         COUNTRY,
         INVNAM,
         RFICDTC,
         #BRTHDTC,
         AGE,
         AGEU,
         SEX,
         SEXN,
         RACE,
         RACEN,
         RACEOTH,
         #ETHNIC,
         SAFFL,
         ITTFL,
         ARMCD,
         ARM,
         ENRFL,
         ACTARMCD ,
         ACTARM ,
         TRT01P,
         TRT01PN,
         TRT01A,
         TRT01AN,
         TRT02P,
         TRT02PN,
         TRT02A,
         TRT02AN,
         TRT01SDTM,
         TRT02SDTM,
         HEIGHT,
         WEIGHT,
         BMI,
         HEIGHTU,
         WEIGHTU,
         BMIU)

vars.label <- c(STUDYID='Study Identifier',
                USUBJID='Unique Subject Identifier',
                SUBJID='Subject Identifier for the Study',
                SITEID='Study Site Identifier',
                COUNTRY='Country',
                INVNAM='Investigator Name',
                RFICDTC='Date of Informed Consent',
                #BRTHDTC='Date/Time of Birth',
                AGE='Age',
                AGEU='Age Units',
                SEX='Sex',
                SEXN='Sex (N)',
                RACE='Race',
                RACEN='',
                RACEOTH='Race, Other',
                #ETHNIC='Ethnicity',
                SAFFL='Safety Population Flag',
                ITTFL='Randomized Population Flag',
                ARMCD='Planned Arm Code',
                ARM='Description of Planned Arm',
                ENRFL='Enrollment Population Flag',
                ACTARMCD ='Actual Arm Code ',
                ACTARM ='Description of Actual Arm ',
                TRT01P='Planned Treatment for Period 01',
                TRT01PN='Planned Treatment for Period 01 (N)',
                TRT01A='Actual Treatment for Period 01',
                TRT01AN='Actual Treatment for Period 01 (N)',
                TRT02P='Planned Treatment for Period 02',
                TRT02PN='Planned Treatment for Period 02 (N)',
                TRT02A='Actual Treatment for Period 02',
                TRT02AN='Actual Treatment for Period 02 (N)',
                TRT01SDTM='Treatment start date/time Period 01',
                TRT02SDTM='Treatment start date/time Period 02',
                HEIGHT='Height',
                WEIGHT='Weight',
                BMI='BMI',
                HEIGHTU='Height Unit',
                WEIGHTU='Weight Unit',
                BMIU='BMI Unit')

library(Hmisc)
ADSL <- Hmisc::upData(dm7, labels=vars.label)


#Save in RDS format

saveRDS(ADSL, file="ADSL.rds")        















































