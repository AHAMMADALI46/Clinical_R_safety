#File name: rlae1
# Author: Ali
# Date: 24FEB2025
# Project/Study:
# Description: <To Develop The Listing 16.2.1.1 Adverse Events>
# Input: Adam.ADSL
# Output: Listing 16.2.1.1 Adverse Events
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

#install.packages("r2rtf")
library(r2rtf)

setwd("//Users//muhammed//R clinical ")
getwd()

path_raw <- "//Users//muhammed//R clinical //Doc//rawdatasets//"
path_sdtm <- "//Users//muhammed//R clinical //Doc//SDTM//"
path_adam <- "//Users//muhammed//R clinical //Doc//ADAM//"

adsl<- read_sas(paste0(path_adam, "ADSL.sas7bdat"))
adae<- read_sas(paste0(path_adam, "ADAE.sas7bdat"))

ae1 <- adae %>% 
  mutate(
    TRT1=case_when(str_detect(TRTA, "TEST") ~ "TEST",
                   str_detect(TRTA, "REFE") ~ "REFERENCE",
                   TRUE ~ NA_character_),
    
    AESEQC = as.character(AESEQ),
    SPA = paste0(str_trim(AETERM), "/", str_trim(AEBODSYS), "/", str_trim(AEDECOD))
  ) %>% 
  filter(!is.na(TRT1)) %>% 
  select(TRT1, USUBJID, AESEQC, SPA, AESTDTC, AENDTC, AESEV, AESER, AEACN, AEREL, AEOUT)

#output location
output <- "//Users//muhammed//R clinical //Output//"

ae1 %>% 
  rtf_page(orientation = "landscape") %>% 
  rtf_title("Listing 16.2.1.1 Adverse Events",
            text_justification ="l",
            text_format ="b") %>% 
  rtf_colheader("Treatment | Subj.\nNo. |AE\nNO.|Adverse Event/Primary System Organ Class/\nPreferred term
                |Start\nDate/Time|End\nDate/Time|Severity|Serious\nEvent|Action Taken|
                Relationship to\nStudy Drug|Outcome",
                col_rel_width = c(1.5,2,1,4.5,2.0,2.0,2.0,1.5,2.0,2.5,2.0)) %>% 
  rtf_body(col_rel_width = c(1.5,2,1,4.5,2.0,2.0,2.0,1.5,2.0,2.5,2.0)) %>% 
  rtf_footnote("Data source: Test=Oxybutynin, Reference=CARE(Oxybutynin Succinate)") %>% 
  rtf_encode() %>% 
  write_rtf(paste0(output, "Listing 16.2.1.1.rtf"))  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  