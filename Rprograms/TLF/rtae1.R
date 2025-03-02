#File name: rtae1
# Author: Ali
# Date: 26FEB2025
# Project/Study:
# Description: <To Develop The Table 14.2.1 Treatment Emergent Adverse Events by Treatment,soc and PT>
# Input: Adam.ADAE
# Output: Table 14.2.1 Treatment Emergent Adverse Events by Treatment, System Organ Class and Preferred Term(Safety Population)
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


#install.packages("sqldf")
library(sqldf)

#install.packages("r2rtf")
library(r2rtf)

setwd("//Users//muhammed//R clinical ")
getwd()

path_raw <- "//Users//muhammed//R clinical //Doc//rawdatasets//"
path_sdtm <- "//Users//muhammed//R clinical //Doc//SDTM//"
path_adam <- "//Users//muhammed//R clinical //Doc//ADAM//"

adsl<- read_sas(paste0(path_adam, "ADSL.sas7bdat"))
adae<- read_sas(paste0(path_adam, "ADAE.sas7bdat"))

#filter subject in safety population
demog <- adsl %>% 
  filter(SAFFL == "Y")

#process treatment Assignment for TRT01A
demog_1 <- demog %>% 
  mutate(
    TRT = case_when(
      str_detect(TRT01A, "TEST") ~ "TES",
      str_detect(TRT01A, "REFE") ~ "REF"
    ),
    ORD = case_when(TRT =="TES" ~ 1, TRT=="REF" ~ 2)) %>% 
      select(USUBJID, AGE, TRT, ORD, SEX, RACE, SEXN, RACEN) %>% 
      filter(!is.na(ORD))
#process treatment assignment for TRT02A
demog_2 <- demog %>% 
  mutate(
    TRT = case_when(
      str_detect(TRT02A, "TEST") ~ "TES",
      str_detect(TRT02A, "REFE") ~ "REF"
    ),
    ORD = case_when(TRT =="TES" ~ 1, TRT=="REF" ~ 2)) %>% 
  select(USUBJID, AGE, TRT, ORD, SEX, RACE, SEXN, RACEN) %>% 
  filter(!is.na(ORD))

#combine demographic data
demog_final <- bind_rows(demog_1, demog_2) %>% 
  distinct(USUBJID, TRT, .keep_all=TRUE)

#count unique subjects per treatment
n_counts <- demog_final %>% 
  group_by(ORD) %>% 
  summarise(N= n_distinct(USUBJID)) %>% 
  arrange(ORD)

n1 <- n_counts$N[1]
n2 <- n_counts$N[2]

cat("N1:", n1, "N2:", n2, "\n")

#filter and process adverse events data
adae1 <- adae %>% 
  filter(TRTMFL == "Y", SAFFL =="Y") %>% 
  mutate(
    TRT = case_when(
      str_detect(TRTA, "TEST") ~ "TES",
      str_detect(TRTA, "REFE") ~ "REF"
    ),
    ORD = case_when(TRT == "TES" ~ 1, TRT == "REF" ~ 2)
  ) %>% 
  select(USUBJID, AEBODSYS, AEDECOD, TRT, ORD)

#summarise AE counts
any1 <- adae1 %>% 
  group_by(TRT) %>% 
  summarise(N=n_distinct(USUBJID)) %>% 
  mutate(AEBODSYS = "Number of Subjects with TETAs",
         AEDECOD ="")

soc <- adae1 %>% 
  group_by(TRT, AEBODSYS) %>% 
  summarise(N=n_distinct(USUBJID)) %>% 
  mutate(AEDECOD="")

pt <- adae1 %>% 
  group_by(TRT, AEBODSYS, AEDECOD) %>% 
  summarise(N=n_distinct(USUBJID)) 
#Combine AE Data
all_data <- bind_rows(any1, soc, pt)

#Pivot data
final <- all_data %>% 
  pivot_wider(id_cols=c("AEBODSYS","AEDECOD"),
              names_from = TRT, values_from = N) %>% 
  arrange(AEBODSYS, AEDECOD)

final1 <- final %>% 
  mutate(
    test = ifelse(is.na(TES), "0", paste0(TES, "(", round(TES/n1 * 100, 1), ")")),
    #refe = ifelse(is.na(REF), "0", paste0(REF, "(", round(REF/n2 * 100, 1), ")"))
  ) %>% 
  select(AEBODSYS, AEDECOD, test) #refe)

final2 <- final1 %>% 
  arrange(AEBODSYS, AEDECOD) %>% 
mutate(
  AEBODSYS=case_when(AEDECOD!="" ~ " ",
                      TRUE ~ paste0(AEBODSYS, "", AEDECOD)),
         cc=paste0(AEBODSYS, "   ", AEDECOD),
  Refe ="0") %>% 
  select(cc, test, Refe)
  
      
#output location
output <- "//Users//muhammed//R clinical //Output//"

final2 %>% 
  rtf_page(orientation ='landscape') %>% 
  rtf_title(title = "Table 14.2.1 Treatment Emergent Adverse Events by Treatment, System Organ Class and Preferred Term(Safety Population)",
            text_justification ="l",
            text_format ="b") %>% 
  rtf_colheader(paste0("MedDRA® System Organ Class \nMedDRA® Preferred Term
  |Test \n(N=",n1,"),|Reference \n(N=", n2,")"),
                col_rel_width = c(11,6,6),
                text_justification = c("l","c","c")) %>% 
  rtf_body(col_rel_width = c(11,6,6),
           text_justification = c("l","c","c")) %>% 
  rtf_footnote(paste0("Test=Oxybutynin; Reference=CARE(Oxybutynin Succinate
  MedDRA®: Medical Dictionary for Regulatory Activities Version 18.0; TEAEs: Treatment-emergent adverse events
  Each subject could only contribute once to each of the incidence rates, regardless of the number of occurrences.
  E: Number of events; n(%): Number and percent of subjects
  Data source: Listing 16.2.1")) %>% 
  rtf_encode() %>% 
  write_rtf(paste0(output, "Table 14.2.1.rtf"))





















  

























