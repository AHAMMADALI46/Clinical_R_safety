#File name: DM
# Author: Ali
# Date: 15FEB2025
# Project/Study:
# Description: <To Develop The DM dataset>
# Input: Demog_raw, exposure_raw,
# Output: SDTM DM datasets
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

dm <- read_sas(paste0(path_raw, "demog_raw.sas7bdat"))
ex<- read_sas(paste0(path_raw, "exposure_raw.sas7bdat"))
rnd<- read_sas(paste0(path_raw, "rnd.sas7bdat"))

#variables creation for DM dataset
dm1 <- dm %>% 
  mutate(STUDYID =trimws(STUDY),
         DOMAIN='DM',
         SUBJID= trimws(SUBJID),
         SITEID=trimws(SITE),
         USUBJID=paste0(STUDYID, '-', SITEID, '-', SUBJID),
         AGEU ='YEARS',
         AGE=trimws(AGEUN),
         
        RACE=str_to_upper(ETH),
        RFSTDTC=paste0(as.character(ENRDT), "T", as.character(ENRTM)),
        RFENDTC=paste0(as.character(CMPDT_), "T", as.character(CMPTM_)),
        
        RFICDTC=paste0(as.character(infdt), "T", as.character(inftm)),
        RFPENDTC=paste0(as.character(CMPDT_), "T", as.character(CMPTM_)),
        
        DTHDTC="",
        DTHFL=" ",
        
        INVNAM = inv,
        INVID= INVID1) %>% 
  
  mutate(SEX= case_when(str_to_upper(GEN) =="FEMALE" ~ "F",
                        str_to_upper(GEN) =="MALE" ~ "M",
                         TRUE ~ "")) %>% 
        
  select(STUDYID, DOMAIN, USUBJID, SUBJID, SITEID, AGE, AGEU, RACE, RFSTDTC, RFENDTC,
         RFICDTC, RFPENDTC, DTHDTC, DTHFL, INVNAM, INVID, SEX, ETHOT)

#variables creation for EX dataset
ex1 <- ex %>% 
  mutate(STUDYID =trimws(STUDY),
         SUBJID= trimws(SUBJID),
         SITEID=trimws(SITE),
         USUBJID=paste0(STUDYID, '-', SITEID, '-', SUBJID),
         DSDTN=mdy(DSDT),
         RFXSTDTC= paste(format(DSDTN, "%y-%m-%d"), format(DSDTM, "%H:%M:%S"), sep="T")) %>% 
  filter(VISIT=="Period-1") %>% 
  select(USUBJID, RFXSTDTC, TRT)

ex2 <- ex %>% 
  mutate(STUDYID =trimws(STUDY),
         SUBJID= trimws(SUBJID),
         SITEID=trimws(SITE),
         USUBJID=paste0(STUDYID, '-', SITEID, '-', SUBJID),
         DSDTN=mdy(DSDT),
         RFXENDTC= paste(format(DSDTN, "%y-%m-%d"), format(DSDTM, "%H:%M:%S"), sep="T"),
         TRT1= TRT) %>% 
  filter(VISIT=="Period-2") %>% 
  select(USUBJID, RFXENDTC, TRT1)
  
#Merge Ex1 and Ex2
ex3 <- full_join(ex1, ex2, by="USUBJID") %>% 
  mutate(RFXENDTC = ifelse(is.na(RFXENDTC), RFXSTDTC, RFXENDTC))

#Merge Ex3 with Dm1
dm2 <- left_join(dm1, ex3, by="USUBJID") 

#Merge DM2 with RND
 rnd <- rnd %>%  arrange(SUBJID)
dm2 <-dm2 %>%  arrange(SUBJID)

dm3 <- left_join(dm2, rnd, by="SUBJID")

#Final Dataset with ARm Logic
dm4 <- dm3 %>% 
  mutate(
    ARMCD = ARMDP,
    ARM = ARMP,
    ACTARMCD = case_when(
      TRT == "REF" & TRT1 =="TEST" ~ "R-T",
      TRT == "TEST" & TRT1 == "REF" ~ "T-R",
      TRUE ~ NA_character_
    ),
    
    ACTARM = case_when(
      TRT == "REF" & TRT1 =="TEST" ~ "REFF-TEST",
      TRT == "TEST" & TRT1 == "REF" ~ "TEST-REFF",
      TRUE ~ NA_character_
    ),
    
    COUNTRY="IND") %>% 
  select(STUDYID, DOMAIN, USUBJID, SUBJID, SITEID, RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC,
         RFICDTC,RFPENDTC, DTHDTC, DTHFL, INVNAM, INVID, AGE, AGEU, SEX, RACE, ARMCD, ARM,
         ACTARMCD, ACTARM, COUNTRY, ETHOT, INVID)
  
 vars.label <- c(
   STUDYID='Study Identifier',
   DOMAIN='Domain Abbreviation',
   USUBJID='Unique Subject Identifier',
   SUBJID='Subject Identifier for the Study',
   SITEID='Study Site Identifier',
   RFSTDTC='Subject Reference Start Date/Time',
   RFENDTC='Subject Reference End Date/Time',
   RFXSTDTC='Date/Time of First Study Treatment',
   RFXENDTC='Date/Time of Last Study Treatment',
   RFICDTC='Date/Time of Informed Consent',
   RFPENDTC='Date/Time of End of Participation',
   DTHDTC='Date/Time of Death',
   DTHFL='Subject Death Flag',
   INVNAM='Investigator Name',
   AGE='Age',
   AGEU='Age Units',
   SEX='Sex',
   RACE='Race',
   #ETHNIC='Ethnicity',
   ARMCD='Planned Arm Code',
   ARM='Description of Planned Arm',
   ACTARMCD='Actual Arm Code',
   ACTARM='Description of Actual Arm',
   COUNTRY='Country',
   #DMDTC='Date/Time of Collection',
   #DMDY='Study Day of Collection',
   INVID='Investigator'
   #BRTHDTC='Date/Time of Birth'
 )

 #install.packages("Hmisc")
 library(Hmisc)
 DM <- Hmisc::upData(dm4, labels=vars.label)    

 #Save in RDS format
 
 saveRDS(DM, file="DM.rds")        
    
    
    
    
    
    
    
    
    
    

















