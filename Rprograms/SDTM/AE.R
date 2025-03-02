#File name: AE
# Author: Ali
# Date: 16FEB2025
# Project/Study:
# Description: <To Develop The AE dataset>
# Input: AE_raw, 
# Output: SDTM AE dataset
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

ae <- read_sas(paste0(path_raw, "ae_raw.sas7bdat"))
dm <- read_sas(paste0(path_sdtm, "dm.sas7bdat"))

ae1 <- ae %>% 
  mutate(STUDYID =trimws(STUDY),
         DOMAIN='AE',
         SUBJID= trimws(SUBJID),
         SITEID=trimws(SITE),
         USUBJID=paste0(STUDYID, '-', SITEID, '-', SUBJID),
         AETERM = trimws(EVENT),
         AELLT = trimws(llt),
         AELLTCD = as.numeric(lltcd),
         AEDECOD = trimws(DECOD),
         AEPTCD = as.numeric(ptcd),
         AEHLT = trimws(hlt),
         AEHLTCD = as.numeric(hltcd),
         AEHLGT = trimws(HLGT),
         AEHLGTCD = as.numeric(HLGTcd),
         AEBODSYS = trimws(BODSYS),
         AEBDSYCD = BDSYCD,
         AESOC = trimws(soc),
         AESOCCD = as.numeric(soccd),
         AESEV = str_to_upper(SEV),
         AESER = str_sub(str_to_upper(SER), 1, 1),
         
         AEACN = str_to_upper(trimws(ACTT)),
         AEACNOTH = trimws(COCN),
         
         AEREL = trimws(CAU),
         AEOUT = str_to_upper(trimws(OUTCOME)),
         AECONTRT = str_sub(str_to_upper(trimws(COCN)),1,1))

#variables deriving from DM dataset

dm1 <- dm %>% 
  mutate(
    RFSTDTN = convert_dtc_to_dt(RFSTDTC)) %>% 
  select(USUBJID, RFSTDTN, RFSTDTC)
  
#Creating AESTDTC, AEENDTC
ae2 <- ae1 %>% 
  mutate(
    AESTDTC=paste0(as.character(STDT), "T", as.character(STTM)),
    AEENDTC=paste0(as.character(STOPDT), "T", as.character(STOPTM)),
    
    AESTDTN=as.Date(AESTDTC),
    AEENDTN=as.Date(AEENDTC)) 
#select (AESTDTC, AEENDTC, AESTDTN, AEENDTN)

#Merge two datasets dm1 and ae2
ae3 <- inner_join(dm1, ae2, by="USUBJID") %>% 
  mutate(ASTDT = AESTDTN,
         AENDT = AEENDTN) %>% 
  mutate(AESTDY = ifelse(!is.na(ASTDT) < !is.na(RFSTDTN),
                         ASTDT-RFSTDTN,
                         ASTDT-RFSTDTN+1)) %>% 
  mutate(AEENDY = ifelse(!is.na(AENDT) < !is.na(RFSTDTN),
                         AENDT-RFSTDTN,
                         AENDT-RFSTDTN+1),
         AEDUR1 = AENDT-ASTDT +1,
         AEDUR = paste0("P", AEDUR1, "D"),
         AESEQ = row_number()) 
  # select(ASTDT, AENDT, AESTDY, AEENDY, AEDUR)

ae3 <-ae3 %>%  arrange(STUDYID,USUBJID,AEDECOD,AESTDTC) %>% 
  select(
    STUDYID,
    DOMAIN,
    USUBJID,
    AESEQ,
    #AEGRPID,
    #AEREFID,
    #AESPID,
    AETERM,
    #AEMODIFY,
    AELLT,
    AELLTCD,
    AEDECOD,
    AEPTCD,
    AEHLT,
    AEHLTCD,
    AEHLGT,
    AEHLGTCD,
    #AECAT,
    #AESCAT,
    #AEPRESP,
    AEBODSYS,
    AEBDSYCD,
    AESOC,
    AESOCCD,
    #AELOC,
    AESEV,
    AESER,
    AEACN,
    AEACNOTH,
    AEREL,
    #AERELNST,
    #AEPATT,
    AEOUT,
    #AESCAN,
    #AESCONG,
    #AESDISAB,
    #AESDTH,
    #AESHOSP,
    #AESLIFE,
    #AESOD,
    #AESMIE,
    AECONTRT,
    #AETOXGR,
    AESTDTC,
    AEENDTC,
    AESTDY,
    AEENDY,
    AEDUR
  )

vars.label <- c(
  STUDYID='Study Identifier',
  DOMAIN='Domain Abbreviation',
  USUBJID='Unique Subject Identifier',
  AESEQ='Sequence Number',
  #AEGRPID='Group ID',
  #AEREFID='Reference ID',
  #AESPID='Sponsor-Defined Identifier',
  AETERM='Reported Term for the Adverse Event',
  #AEMODIFY='Modified Reported Term',
  AELLT='Lowest Level Term',
  AELLTCD='Lowest Level Term Code',
  AEDECOD='Dictionary-Derived Term',
  AEPTCD='Preferred Term Code',
  AEHLT='High Level Term',
  AEHLTCD='High Level Term Code',
  AEHLGT='High Level Group Term',
  AEHLGTCD='High Level Group Term Code',
  #AECAT='Category for Adverse Event',
  #AESCAT='Subcategory for Adverse Event',
  #AEPRESP='Pre-Specified Adverse Event',
  AEBODSYS='Body System or Organ Class',
  AEBDSYCD='Body System or Organ Class Code',
  AESOC='Primary System Organ Class',
  AESOCCD='Primary System Organ Class Code',
  #AELOC='Location of Event',
  AESEV='Severity/Intensity',
  AESER='Serious Event',
  AEACN='Action Taken with Study Treatment',
  AEACNOTH='Other Action Taken',
  AEREL='Causality',
  #AERELNST='Relationship to Non-Study Treatment',
  #AEPATT='Pattern of Adverse Event',
  AEOUT='Outcome of Adverse Event',
  #AESCAN='Involves Cancer',
  #AESCONG='Congenital Anomaly or Birth Defect',
  #AESDISAB='Persist or Signif Disability/Incapacity',
  #AESDTH='Results in Death',
  #AESHOSP='Requires or Prolongs Hospitalization',
  #AESLIFE='Is Life Threatening',
  #AESOD='Occurred with Overdose',
  #AESMIE='Other Medically Important Serious Event',
  AECONTRT='Concomitant or Additional Trtmnt Given',
  #AETOXGR='Standard Toxicity Grade',
  AESTDTC='Start Date/Time of Adverse Event',
  AEENDTC='End Date/Time of Adverse Event',
  AESTDY='Study Day of Start of Adverse Event',
  AEENDY='Study Day of End of Adverse Event',
  AEDUR='Duration of Adverse Event')
  #AEENRF='End Relative to Reference Period'

#install.packages("Hmisc")
library(Hmisc)
AE <- Hmisc::upData(ae3, labels=vars.label)   

#Save in RDS format

saveRDS(AE, file="AE.rds")   


         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         