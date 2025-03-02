#File name: EX
# Author: Ali
# Date: 16FEB2025
# Project/Study:
# Description: <To Develop The EX dataset>
# Input: demog_raw, exposure_raw
# Output: SDTM EX dataset
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

ex <- read_sas(paste0(path_raw, "exposure_raw.sas7bdat"))
dm <- read_sas(paste0(path_sdtm, "dm.sas7bdat"))

ex1 <- ex %>% 
  mutate(STUDYID =trimws(STUDY),
         DOMAIN='EX',
         SUBJID= trimws(SUBJID),
         SITEID=trimws(SITE),
         USUBJID=paste0(STUDYID, '-', SITEID, '-', SUBJID),
         EXTRT = trimws(TRT),
         EXDOSE = as.numeric(str_sub(DOSE, 1, 2)),
         EXDOSETXT = trimws(DOSEN),
         EXDOSU = str_to_lower(str_sub(DOSE,3)),
         EXDOSFRM = str_to_upper(trimws(DOSTP)),
         EXDOSFRQ = str_to_upper(trimws(freq)),
         EXROUTE = str_to_upper(trimws(route)),
         EXFAST = str_sub(str_to_upper(trimws(fast)),1,1),
         EPOCH = "TREATMENT")
ex2 <- ex1 %>% 
  mutate(
    DSDTN = as.Date(DSDT, format="%m/%d/%Y"),
    EXSTDTC=paste0(as.character(DSDTN), "T", as.character(DSDTM)),
    EXENDTC=paste0(as.character(DSDTN), "T", as.character(DSDTM)),
    
    EXSTDTN=as.Date(EXSTDTC),
    EXENDTN=as.Date(EXENDTC))

#variables deriving from DM dataset

dm1 <- dm %>% 
  mutate(
    RFSTDTN = convert_dtc_to_dt(RFSTDTC)) %>% 
  select(USUBJID, RFSTDTN, RFSTDTC)

dm1 <-dm1 %>%  arrange(USUBJID) 
ex2 <-ex2 %>%  arrange(USUBJID) 

#Merge two datasets dm1 and ae2
ex3 <- inner_join(dm1, ex2, by="USUBJID") %>% 
  mutate(EXSTDY = ifelse(!is.na(EXSTDTN) < !is.na(RFSTDTN),
                         EXSTDTN-RFSTDTN,
                         EXSTDTN-RFSTDTN+1)) %>% 
  mutate(EXENDY = ifelse(!is.na(EXENDTN) < !is.na(RFSTDTN),
                         EXENDTN-RFSTDTN,
                         EXENDTN-RFSTDTN+1),
         EXDUR1 = EXENDTN-EXSTDTN +1,
         EXDUR = paste0("P", EXDUR1, "D")) %>% 
        group_by(USUBJID) %>% 
         mutate(EXSEQ = 1:n()) %>% 
  select(STUDYID,
         DOMAIN,
         USUBJID,
         EXSEQ,
         #EXGRPID,
         #EXREFID,
         #EXSPID,
         #EXLNKID,
         #EXLNKGRP,
         EXTRT,
         #EXCAT,
         #EXSCAT,
         EXDOSE,
         #EXDOSTXT,
         EXDOSU,
         EXDOSFRM,
         EXDOSFRQ,
         #EXDOSRGM,
         EXROUTE,
         #EXLOT,
         #EXLOC,
         #EXLAT,
         #EXDIR,
         #EXFAST,
         #EXADJ,
         EPOCH,
         EXSTDTC,
         EXENDTC,
         EXSTDY,
         EXENDY,
         EXDUR)

vars.label <- c(
  STUDYID='Study Identifier',
  DOMAIN='Domain Abbreviation',
  USUBJID='Unique Subject Identifier',
  EXSEQ='Sequence Number',
  #EXGRPID='Group ID',
  #EXREFID='Reference ID',
  #EXSPID='Sponsor-Defined Identifier',
  #EXLNKID='Link ID',
  #EXLNKGRP='Link Group ID',
  EXTRT='Name of Treatment',
  #EXCAT='Category of Treatment',
  #EXSCAT='Subcategory of Treatment',
  EXDOSE='Dose',
  #EXDOSTXT='Dose Description',
  EXDOSU='Dose Units',
  EXDOSFRM='Dose Form',
  EXDOSFRQ='Dosing Frequency per Interval',
  #EXDOSRGM='Intended Dose Regimen',
  EXROUTE='Route of Administration',
  #EXLOT='Lot Number',
  #EXLOC='Location of Dose Administration',
  #EXLAT='Laterality',
  #EXDIR='Directionality',
  #EXFAST='Fasting Status',
  #EXADJ='Reason for Dose Adjustment',
  EPOCH='Epoch',
  EXSTDTC='Start Date/Time of Treatment',
  EXENDTC='End Date/Time of Treatment',
  EXSTDY='Study Day of Start of Treatment',
  EXENDY='Study Day of End of Treatment',
  EXDUR='Duration of Treatment'
)

#install.packages("Hmisc")
library(Hmisc)
EX <- Hmisc::upData(ex3, labels=vars.label)  

#Save in RDS format

saveRDS(EX, file="EX.rds") 
