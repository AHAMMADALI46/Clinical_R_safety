#File name: rldemo
# Author: Ali
# Date: 23FEB2025
# Project/Study:
# Description: <To Develop The Listing 16.1.1.1 Subject Demographics>
# Input: Adam.ADSL
# Output: Listing 16.1.1.1 Subject Demographics
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

adsl1 <- adsl %>% 
  mutate(
    SUBJN= str_sub(USUBJID,1,13),
  ) %>% 
  select(SUBJN, ACTARM, AGE, SEX, HEIGHT, WEIGHT, BMI)
adsl1 <- adsl1 %>% arrange(SUBJN)

#output location
output <- "//Users//muhammed//R clinical //Output//"

adsl1 %>% 
  rtf_page(orientation = "landscape") %>% 
  rtf_title("Patient Demographics and Treatment Information",
            text_justification ="l",
            text_format ="b") %>% 
  rtf_colheader("Subject Number| Treatment Sequence |Age*(years)|Sex|Height(cm)|Weight(kg)|BMI(kg/m2)",
col_rel_width = c(1.5,2,1,1,1.5,1.5,1.5)) %>% 
  rtf_body(col_rel_width = c(1.5,2,1,1,1.5,1.5,1.5)) %>% 
  rtf_footnote("*Age at the time of screening;BMI:Body Mass Index.
               Data source: T=Test, R=Reference, Test=Oxybutynin, Reference=CARE(Oxybutynin Succinate)") %>% 
  rtf_encode() %>% 
  write_rtf(paste0(output, "Listing 16.1.1.1.rtf"))
  








