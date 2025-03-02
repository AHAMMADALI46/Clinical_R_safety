#File name: ADAE
# Author: Ali
# Date: 23FEB2025
# Project/Study:
# Description: <To Develop The ADAE dataset>
# Input: SDTM AE, ADAM ADSL
# Output: AdaM ADAE datasets
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

ae <- read_sas(paste0(path_sdtm, "ae.sas7bdat"))
adsl <- read_sas(paste0(path_adam, "adsl.sas7bdat"))

ae <- ae %>%  arrange(USUBJID)
adsl <- adsl %>%  arrange(USUBJID) %>% 
  select(-STUDYID)

ae2 <- ae %>%  left_join(adsl, by="USUBJID") %>% 
  mutate(
    TRTSDT = str_sub(TRT01SDTM, 1, 10),
    TRTEDT = str_sub(TRT02SDTM, 1, 10),
#extract AESTDTC components
AEST_YY = if_else(!is.na(AESTDTC), str_sub(AESTDTC,1,4), NA_character_),
AEST_MM = if_else(str_length(AESTDTC) >= 7, str_sub(AESTDTC,6,2), NA_character_),
AEST_DD = if_else(str_length(AESTDTC) >= 10, str_sub(AESTDTC,9,2), NA_character_),

#extract AEENDTC components
AEEN_YY = if_else(!is.na(AEENDTC), str_sub(AEENDTC,1,4), NA_character_),
AEEN_MM = if_else(str_length(AEENDTC) >= 7, str_sub(AEENDTC,6,2), NA_character_),
AEEN_DD = if_else(str_length(AEENDTC) >= 10, str_sub(AEENDTC,9,2), NA_character_),

#extract TRTSDTC components
TRST_YY = if_else(!is.na(TRTSDT), str_sub(TRTSDT,1,4), NA_character_),
TRST_MM = if_else(str_length(TRTSDT) >= 7, str_sub(TRTSDT,6,2), NA_character_),
TRST_DD = if_else(str_length(TRTSDT) >= 10, str_sub(TRTSDT,9,2), NA_character_),


#extract TRTEDTC components
TREN_YY = if_else(!is.na(TRTEDT), str_sub(TRTEDT,1,4), NA_character_),
TREN_MM = if_else(str_length(TRTEDT) >= 7, str_sub(TRTEDT,6,2), NA_character_),
TREN_DD = if_else(str_length(TRTEDT) >= 10, str_sub(TRTEDT,9,2), NA_character_)) %>% 

#Handle misssing date imputation
mutate(
  AEST_DD = case_when(
    is.na(AEST_DD) & !is.na(AEST_MM) & !is.na(AEST_YY) ~ if_else(AEST_MM == TRST_MM, TRST_DD, "01"),
                                                                TRUE ~ AEST_DD),
    ASTDTF = case_when(
      is.na(AEST_DD) & !is.na(AEST_MM) & !is.na(AEST_YY) ~ "D",
      is.na(AEST_MM) & !is.na(AEST_YY) ~ "M",
      is.na(AEST_DD) & is.na(AEST_MM) & is.na(AEST_YY) ~ "Y",
      TRUE ~ NA_character_
    ),
  AEST_MM = case_when(
    is.na(AEST_MM) & !is.na(AEST_YY) ~ if_else(AEST_YY == TRST_YY, TRST_MM, "12"),
    TRUE ~ AEST_MM
  ),
  
  AEST_DD = case_when(
    is.na(AEST_DD) & !is.na(AEST_MM) & !is.na(AEST_YY) ~ if_else(AEST_YY == TRST_YY, TRST_DD, "31"),
    TRUE ~ AEST_DD
  ),
  
  #Create final date variables
  ASTDTC = as.Date(AESTDTC),
  #ASTDTC = if_else(is.na(AEST_YY), NA_character_, ASTDTC),
  ASTDT = ASTDTC,
  
  AENDTC = str_sub(AEENDTC, 1, 10),
  AENDT = ymd(AENDTC))

ae3<- ae2 %>% 
  mutate(
    TRTMFL= case_when(!is.na(ASTDT) & ASTDT >= TRTSDT ~ "Y",
              TRUE ~ "N"),
    APERIODC = case_when(
      ASTDT >= TRTSDT & ASTDT <= TRTEDT ~ "PERIOD-01",
      ASTDT > TRTEDT ~ "PERIOD-2",
      TRUE ~ NA_character_
    ),
    APERIOD = case_when(
      APERIODC =="PERIOD-01" ~ 1,
      APERIODC == "PERIOD-2" ~ 2,
      TRUE ~ NA_real_
    ),
    TRTA = case_when(
      APERIOD ==1 ~ TRT01A,
      APERIOD ==2 ~ TRT02A,
      TRUE ~ NA_character_
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
    #TEAEFL,
    APERIOD,
    TRTA,
    ASTDT,
    AENDT
  )
vars.label<- c(
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
  #TEAEFL='Treatment Emergent flag',
  APERIOD='Analysis period',
  TRTA='Actual Treatment For AE',
  ASTDT='ANALYSIS START DATE',
  AENDT='ANALYSIS END DATE'
)
library(Hmisc)
ADAE <- Hmisc::upData(ae3, labels=vars.label)  


#Save in RDS format

saveRDS(ADAE, file="ADAE.rds")          





























