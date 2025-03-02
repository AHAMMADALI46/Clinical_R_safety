#File name: ADLB
# Author: Ali
# Date: 22FEB2025
# Project/Study:
# Description: <To Develop The ADLB dataset>
# Input: SDTM LB, ADAM ADSL
# Output: AdaM ADLB datasets
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
path_adam <- "//Users//muhammed//R clinical //Doc//ADAM//"

lb <- read_sas(paste0(path_sdtm, "lb.sas7bdat"))
adsl <- read_sas(paste0(path_adam, "adsl.sas7bdat"))

lb <- lb %>%  arrange(USUBJID)
adsl <- adsl %>%  arrange(USUBJID) %>% 
  select(-STUDYID)

lb2 <- lb %>%  left_join(adsl, by="USUBJID")

lb3 <- lb2 %>% 
  mutate(
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    
    PARAM= case_when(LBSTRESU!="" ~ paste0(trimws(LBTEST),"(", trimws(LBSTRESU), ")"),
                     LBSTRESU =="" ~ trimws(LBTEST),
                     TRUE ~ ""),
    PARAMCD = LBTESTCD,
    PARCAT1 = LBCAT,
    PARCAT1N = case_when(PARCAT1=="BIOCHEMISTRY" ~ 1,
                         PARCAT1 =="HEAMATOLOGY" ~ 2,
                         PARCAT1 == "URINALYSIS" ~ 3))

param <- lb3 %>% 
  select(PARAMCD) %>% 
  distinct() %>% 
  mutate(PARAMN = row_number())

lb3 <- lb3 %>%  arrange(PARAMCD)  
param <- param %>%  arrange(PARAMCD)

lb4 <- lb3 %>%  left_join(param, by="PARAMCD")

lb4 <- lb4 %>%  arrange(USUBJID, PARCAT1N, PARCAT1, PARAMN, PARAM, AVISITN, AVISIT, LBDTC)

#compute BASE, SHIFT1, AVAL, CHG, PCHG and other variables
lb5 <- lb4 %>% 
  group_by(USUBJID, PARCAT1N, PARCAT1, PARAMN, PARAM) %>% 
  rename(PARCAT=PARCAT1) %>% 
  mutate(
    AVAL=LBSTRESN,
    AVALC=as.character(LBSTRESN),
    AVALU=LBSTRESU,
    ABLFL = ifelse(AVISITN == 0, "Y", NA_character_),
    BASE=ifelse(ABLFL=="Y", AVAL, NA),
    
    #"first"= row_number() == min(row_number()),
    #BASE = ifelse(first=="TRUE", NA, BASE),
    #BASEC = ifelse(first=="TRUE", "", as.character(AVAL)),
    SHIFT1 = ifelse(AVISITN == 0, LBNRIND, NA),
    
    CHG = ifelse(!is.na(BASE) & !is.na(AVAL), AVAL- BASE, NA_real_),
    PCHG = ifelse(!is.na(BASE) & !is.na(AVAL) & BASE !=0, (AVAL-BASE)/BASE * 100, NA_real_),
    
    #treatment assignments based on AVISIT
    TRTP = case_when(
      AVISIT == "PERIOD-1" ~ TRT01P,
      AVISIT == "PERIOD-2" ~ TRT02P,
      TRUE ~ NA_character_
    ),
    
    TRTPN = case_when(
      AVISIT == "PERIOD-1" ~ TRT01PN,
      AVISIT == "PERIOD-2" ~ TRT02PN,
      TRUE ~ NA_real_
    ),
    
    TRTA = case_when(
      AVISIT == "PERIOD-1" ~ TRT01A,
      AVISIT == "PERIOD-2" ~ TRT02A,
      TRUE ~ NA_character_
    ),
    
    TRTA = case_when(
      AVISIT == "PERIOD-1" ~ TRT01AN,
      AVISIT == "PERIOD-2" ~ TRT02AN,
      TRUE ~ NA_real_
    ),
    
    #Handling date conversions
    ADT = ifelse(nchar(LBDTC) >=10, as.Date(substr(LBDTC, 1, 10), "%Y-%M-%D"), NA),
    ADTM = LBDTC,
    
    #Flagging analysis records
    ANLO1FL = ifelse(!is.na(AVAL), "Y", NA_character_),
    
    #Assigning period categories
    APERIODC = case_when(
      AVISITN %in% c(0,1) ~ "Period 01",
      AVISITN >= 2 ~ "Period 02",
      TRUE ~ NA_character_
    ),
    
    APERIOD = case_when(
      AVISITN %in% c(0,1) ~ 1,
      AVISITN >= 2 ~ 2,
      TRUE ~ NA_real_
    ),
    # Assigning numeric values for ANRIND
    ANRIND = LBNRIND,
    ANRINDN = case_when(
      ANRIND == "LOW" ~ 1,
      ANRIND == "NORMAL" ~ 2,
      ANRIND == "HIGH" ~ 3,
      ANRIND == "ABNORMAL" ~ 4,
      TRUE ~ NA_real_
    )) %>% 
  ungroup() %>% 
  select (
    STUDYID,
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
    HEIGHT,
    WEIGHT,
    BMI,
    HEIGHTU,
    WEIGHTU,
    BMIU,
    AVISIT,
    AVISITN,
    #PARCAT,
    PARAM,
    PARAMCD,
    PARAMN,
    AVAL,
    AVALC,
    AVALU,
    BASE,
    CHG,
    PCHG,
    ABLFL,
    SHIFT1
  )
vars.label <- c(
  STUDYID='Study Identifier',
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
  HEIGHT='Height',
  WEIGHT='Weight',
  BMI='BMI',
  HEIGHTU='Height Unit',
  WEIGHTU='Weight Unit',
  BMIU='BMI Unit',
  AVISIT='Analysis Visit',
  AVISITN='Analysis Visit (N)',
  #PARCAT='Parameter catergory',
  PARAM='Parameter',
  PARAMCD='Parameter Code',
  PARAMN='Parameter (N)',
  AVAL='Analysis Value',
  AVALC='Analysis Value (C) ',
  AVALU='Analysis Value Unit',
  BASE='Baseline Value',
  CHG='Change from Baseline',
  PCHG='Percentage change from baseline',
  ABLFL='Baseline Record Flag',
  SHIFT1='Baseline result'
)    
    
library(Hmisc)
ADLB <- Hmisc::upData(lb5, labels=vars.label)    


#Save in RDS format

saveRDS(ADLB, file="ADLB.rds")            
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  










































