#File name: VS
# Author: Ali
# Date: 16FEB2025
# Project/Study:
# Description: <To Develop The vs dataset>
# Input: vital_raw
# Output: SDTM VS dataset
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
library(stringr)

setwd("//Users//muhammed//R clinical ")
getwd()

path_raw <- "//Users//muhammed//R clinical //Doc//rawdatasets//"
path_sdtm <- "//Users//muhammed//R clinical //DOC//SDTM//"

vs <- read_sas(paste0(path_raw, "vital_raw.sas7bdat"))
dm <- read_sas(paste0(path_sdtm, "dm.sas7bdat"))

vs1 <- vs %>% 
  mutate(STUDYID =trimws(STUDYID),
         DOMAIN='VS',
         SUBJID= trimws(SUBJID),
         SITEID=trimws(SITE),
         USUBJID=paste0(STUDYID, '-', SITEID, '-', SUBJID),
         VSDTC = paste0(as.character(RECDT), "T", as.character(RECDTM)),
         VSTPT = trimws(TP)) %>% 
         mutate(VSTPTNUM = case_when(
           VSTPT == "Check-in" ~ 1,
           VSTPT == "0" ~ 2,
           VSTPT == "2" ~ 3,
           VSTPT == "8" ~ 4,
           VSTPT == "Check-out" ~ 5,
           TRUE ~ NA)) %>% 
         mutate(VISITNUM = case_when(
           VISIT == "SCREENING" ~ 0,
           VISIT == "PERIOD-1" ~ 1,
           VISIT == "PERIOD-2" ~ 2,
           TRUE ~ NA))

vs2 <- vs1 %>% 
  arrange(STUDYID,DOMAIN,USUBJID,VISITNUM,VISIT,VSTPTNUM,VSTPT,VSDTC) %>% 
pivot_longer(
  cols = c(PULSE, TEMP, SYS, DIA),
  names_to ="VSTESTCD",
  values_to = "VSSTRESN"
)

vs3 <- vs2 %>% 
  mutate(
    VSTESTCD=case_when(VSTESTCD =="PULSE" ~ "PULSE",
                       VSTESTCD =="TEMP" ~ "TEMP",
                       VSTESTCD =="SYS" ~ "SYSBP",
                       VSTESTCD =="DIA" ~ "DIABP"),
    VSTEST=case_when(VSTESTCD =="PULSE" ~ "PULSE RATE",
                     VSTESTCD =="TEMP" ~ "TEMPERATURE",
                     VSTESTCD =="SYSBP" ~ "SYSTOLIC BLOOD PRESSURE",
                     VSTESTCD =="DIABP" ~ "DIASTOLIC BLOOD PRESSURE"),
    VSORRES=as.character(trimws(VSSTRESN)))
vs4 <- vs1 %>% 
  arrange(STUDYID,DOMAIN,USUBJID,VISITNUM,VISIT,VSTPTNUM,VSTPT,VSDTC) %>% 
  pivot_longer(
    cols = c(PULSER, TEMPR, SYSR, DIAR),
    names_to ="VSTESTCD1",
    values_to = "VSSTRESN1"
  )

vs5 <- vs4 %>% 
  mutate(
    VSTESTCD=case_when(VSTESTCD1 =="PULSER" ~ "PULSE",
                       VSTESTCD1 =="TEMPR" ~ "TEMP",
                       VSTESTCD1 =="SYSR" ~ "SYSBP",
                       VSTESTCD1 =="DIAR" ~ "DIABP"),
    VSSTRESC=trimws(VSSTRESN1)) %>%
  mutate(
    VSBLFL=case_when(VSTPT=="0" ~ "Y",
                     TRUE ~ ""),
    VSDTN= as.Date(VSDTC)) %>% 
  select(-c(VSTESTCD1,VSSTRESN1 ))


vs6 <- vs1 %>% 
  arrange(STUDYID,DOMAIN,USUBJID,VISITNUM,VISIT,VSTPTNUM,VSTPT,VSDTC) %>% 
  pivot_longer(
    cols = c(PULSEU, TEMPU, SYSU, DIAU),
    names_to ="VSTESTCD2",
    values_to = "VSSTRESN2"
  )

vs7 <- vs6 %>% 
  mutate(
    VSTESTCD=case_when(VSTESTCD2 =="PULSEU" ~ "PULSE",
                       VSTESTCD2 =="TEMPU" ~ "TEMP",
                       VSTESTCD2 =="SYSU" ~ "SYSBP",
                       VSTESTCD2 =="DIAU" ~ "DIABP"),
    VSORRESU=case_when(VSSTRESN2=="per min" ~ "beats/min",
                       VSSTRESN2=="mm of Hg" ~ "mmHg",
                       VSSTRESN2=="F" ~ "F",
                       VSSTRESN2=="" ~ "" ))%>% 
    mutate(VSSTRESU=VSORRESU) %>%
  select(-c(VSTESTCD2,VSSTRESN2 ))


vs3 <-vs3 %>%  arrange(STUDYID,DOMAIN,USUBJID,VISITNUM,VISIT,VSTPTNUM,VSTPT,VSDTC,VSTESTCD) 
vs5 <-vs5 %>%  arrange(STUDYID,DOMAIN,USUBJID,VISITNUM,VISIT,VSTPTNUM,VSTPT,VSDTC,VSTESTCD) 
vs7 <-vs7 %>%  arrange(STUDYID,DOMAIN,USUBJID,VISITNUM,VISIT,VSTPTNUM,VSTPT,VSDTC,VSTESTCD) 

vs8 <- merge(vs3, vs5, by=c("STUDYID","DOMAIN","USUBJID","VISITNUM",
"VISIT","VSTPTNUM","VSTPT","VSDTC","VSTESTCD"),
             all=TRUE)

vs9 <- merge(vs7, vs8, by=c("STUDYID","DOMAIN","USUBJID","VISITNUM",
                            "VISIT","VSTPTNUM","VSTPT","VSDTC","VSTESTCD"),
             all=TRUE)

#variables deriving from DM dataset

dm1 <- dm %>% 
  mutate(
    RFSTDTN = convert_dtc_to_dt(RFSTDTC)) %>% 
  select(USUBJID, RFSTDTN, RFSTDTC)

dm1 <-dm1 %>%  arrange(USUBJID) 
vs9 <-vs9 %>%  arrange(USUBJID) 

#Merge two datasets dm1 and ae2
vs10 <- inner_join(dm1, vs9, by="USUBJID") %>% 
  mutate(VSDY = ifelse(!is.na(VSDTN) < !is.na(RFSTDTN),
                         VSDTN-RFSTDTN,
                         VSDTN-RFSTDTN+1)) %>% 
  group_by(USUBJID) %>% 
  mutate(VSSEQ = 1:n())

final <- vs10 %>% 
  select(
    VSTESTCD,
    VSTEST,
    VSORRES,
    VSDTC,
    VSTPT,
    VSTPTNUM,
    STUDYID,
    DOMAIN,
    USUBJID,
    VSORRESU,
    VSSTRESC,
    VSSTRESN,
    VSSTRESU,
    VSBLFL,
    VISITNUM,
    VISIT,
    VSSEQ,
    VSDY
  )
vars.label <- c(
  VSTESTCD='Vital Signs Test Short Name',
  VSTEST='Vital Signs Test Name',
  VSORRES='Result or Finding in Original Units',
  VSDTC='Date/Time of Measurements',
  VSTPT='Planned Time Point Name',
  VSTPTNUM='Planned Time Point Number',
  STUDYID='Study Identifier',
  DOMAIN='Domain Abbreviation',
  USUBJID='Unique Subject Identifier',
  VSORRESU='Original Units',
  VSSTRESC='Character Result/Finding in Std Format',
  VSSTRESN='Numeric Result/Finding in Standard Units',
  VSSTRESU='Standard Units',
  VSBLFL='Baseline Flag',
  VISITNUM='Visit Number',
  VISIT='Visit Name',
  VSSEQ='Sequence Number',
  VSDY='Study Day of Vital Signs')

library(Hmisc)
VS <- Hmisc::upData(final, labels=vars.label)  

#Save in RDS format

saveRDS(VS, file="VS.rds") 










