#File name: ADVS
# Author: Ali
# Date: 22FEB2025
# Project/Study:
# Description: <To Develop The ADVS dataset>
# Input: SDTM VS, ADAM ADSL
# Output: AdaM ADVS datasets
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

vs <- read_sas(paste0(path_sdtm, "vs.sas7bdat"))
adsl <- read_sas(paste0(path_adam, "adsl.sas7bdat"))

vs <- vs %>%  arrange(USUBJID)
adsl <- adsl %>%  arrange(USUBJID) %>% 
  select(-STUDYID)

vs2 <- vs %>%  left_join(adsl, by="USUBJID")

vs3 <- vs2 %>% 
  mutate(
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    ATPT = VSTPT,
    ATPTN = VISITNUM,
    
    PARAM= paste0(trimws(VSTEST),"(", trimws(VSSTRESU), ")"),
    PARAMCD = VSTESTCD,
   PARAMN = case_when(PARAMCD=="HEIGHT" ~ 1,
                      PARAMCD=="WEIGHT" ~ 2,
                      PARAMCD=="BMI" ~ 3,
                      PARAMCD=="SYSBP" ~ 4,
                      PARAMCD=="DIABP" ~ 5,
                      PARAMCD=="TEMP" ~ 6,
                      PARAMCD=="PULSE" ~ 7))
vs4 <- vs3 %>% 
  arrange(USUBJID, PARAMN, PARAM, AVISITN, AVISIT, ATPTN, ATPT, VSDTC) %>% 
  mutate(
    VSSTRESN1 = ifelse(VSSTRESN %in% c("NA", "Missing", "."), NA, VSSTRESN),
    AVAL=as.numeric(VSSTRESN1),
    AVALC=VSSTRESN,
    AVALU=VSSTRESU,
    ABLFL = ifelse(ATPT == 0, "Y", NA_character_),
    BASE=ifelse(ABLFL=="Y", AVAL, ),
    #BASE=as.numeric(BASE1),
    SHIFT1 = BASE,
  
    CHG = ifelse(!is.na(BASE) & !is.na(AVAL), AVAL-BASE, NA_real_),
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
ADT = ifelse(nchar(VSDTC) >=10, as.Date(substr(VSDTC, 1, 10), "%Y-%M-%D"), NA),
ADTM = VSDTC,

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
)) %>% 
  select(
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
    STUDYID,
    DOMAIN,
    USUBJID,
    VSSEQ,
    VSTESTCD,
    VSTEST,
    VSORRES,
    VSORRESU,
    VSSTRESC,
    VSSTRESN,
    VSSTRESU,
    VSBLFL,
    VISITNUM,
    VISIT,
    VSDTC,
    VSDY,
    VSTPT,
    VSTPTNUM,
    AVISIT,
    AVISITN,
    ATPT,
    ATPTN,
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
  STUDYID='Study Identifier',
  DOMAIN='Domain Abbreviation',
  USUBJID='Unique Subject Identifier',
  VSSEQ='Sequence Number',
  VSTESTCD='Vital Signs Test Short Name',
  VSTEST='Vital Signs Test Name',
  VSORRES='Result or Finding in Original Units',
  VSORRESU='Original Units',
  VSSTRESC='Character Result/Finding in Std Format',
  VSSTRESN='Numeric Result/Finding in Standard Units',
  VSSTRESU='Standard Units',
  VSBLFL='Baseline Flag',
  VISITNUM='Visit Number',
  VISIT='Visit Name',
  VSDTC='Date/Time of Measurements',
  VSDY='Study Day of Vital Signs',
  VSTPT='Planned Time Point Name',
  VSTPTNUM='Planned Time Point Number',
  AVISIT='Analysis Visit',
  AVISITN='Analysis Visit (N)',
  ATPT='Analysis Timepoint',
  ATPTN='Analysis Timepoint (N)',
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
  SHIFT1='Baseline result')
library(Hmisc)
ADVS <- Hmisc::upData(vs4, labels=vars.label)    
    

#Save in RDS format

saveRDS(ADVS, file="ADVS.rds")           
    
    
    
    
    
    
    
    
    
    
    
    
  
















