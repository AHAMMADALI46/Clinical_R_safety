#File name: SUPPDM
# Author: Ali
# Date: 16FEB2025
# Project/Study:
# Description: <To Develop The SUPPDM dataset>
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

suppdm <- dm %>% 
  filter(ETHOT!='') %>% 
  mutate(STUDYID =trimws(STUDY),
         DOMAIN='DM',
         SUBJID= trimws(SUBJID),
         SITEID=trimws(SITE),
         USUBJID=paste0(STUDYID, '-', SITEID, '-', SUBJID),
    RDOMAIN="DM",
    IDVARVAL="",
    IDVAR="",
    QNAM="RACEOTH",
    QLABEL="RACE, OTHER",
    QVAL=trimws(ETHOT),
    QORIG="CRF",
    QEVAL=inv) %>% 
  select(STUDYID,
         RDOMAIN,
         USUBJID,
         IDVAR,
         IDVARVAL,
         QNAM,
         QLABEL,
         QVAL,
         QORIG,
         QEVAL)

vars.label <- c(
  STUDYID='Study Identifier',
  RDOMAIN='Related Domain Abbreviation',
  USUBJID='Unique Subject Identifier',
  IDVAR='Identifying Variable',
  IDVARVAL='Identifying Variable Value',
  QNAM='Qualifier Variable Name',
  QLABEL='Qualifier Variable Label',
  QVAL='Data Value',
  QORIG='Origin',
  QEVAL='Evaluator'
)
library(Hmisc)
SUPPDM <- Hmisc::upData(suppdm, labels=vars.label)      

#Save in RDS format

saveRDS(SUPPDM, file="SUPPDM.rds")     
    
    
    
    
    
    
    
    
    
  