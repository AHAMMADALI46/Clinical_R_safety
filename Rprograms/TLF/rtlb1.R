#File name: rtlb1
# Author: Ali
# Date: 01MAR2025
# Project/Study:
# Description: <To Develop The Table 14.4.1 Shift Table from Baseline to Period 01 end(Safety Population)
# Input: Adam.ADAE
# Output: Table 14.4.1 Shift Table from Baseline to Period 01 end(Safety Population)
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
adlb<- read_sas(paste0(path_adam, "ADLB.sas7bdat"))

#filter dataset
adsl <- adsl %>% 
  filter(SAFFL=="Y")

demog_1 <- adsl %>% 
  mutate(TRT01A ="ALL") %>% 
  bind_rows(adsl)
demog_2 <- demog_1 %>% 
  mutate(TRT=case_when(
    grepl("TEST", TRT01A) ~ "TES",
    grepl("REFE", TRT01A) ~ "REF",
    grepl("ALL", TRT01A) ~ "ALL"
  ),
  ord = case_when(
    TRT=="TES" ~ 1,
    TRT=="REF" ~ 2,
    TRT=="ALL" ~ 3
  )) %>% 
  filter(!is.na(ord)) %>% 
  select(USUBJID, AGE, TRT, ord)

#create TRT02A

demog_11 <- adsl %>% 
  mutate(TRT02A ="ALL") %>% 
  bind_rows(adsl)
demog_22 <- demog_11 %>% 
  mutate(TRT=case_when(
    grepl("TEST", TRT02A) ~ "TES",
    grepl("REFE", TRT02A) ~ "REF",
    grepl("ALL", TRT02A) ~ "ALL"
  ),
  ord = case_when(
    TRT=="TES" ~ 1,
    TRT=="REF" ~ 2,
    TRT=="ALL" ~ 3
  )) %>% 
  filter(!is.na(ord)) %>% 
  select(USUBJID, AGE, TRT, ord)

#Merge datasets
demog_3 <- bind_rows(demog_2, demog_22)

#count distinct subjects by ord
n_counts <- demog_3 %>% 
  group_by(ord) %>% 
  summarise(n=n_distinct(USUBJID))

n3 <- ifelse(nrow(n_counts) >=3, n_counts$n[3],0)
cat(n3, "\n")

#create lb1 dataset
lb1 <- adlb %>% 
  filter(SAFFL=="Y") %>% 
  transmute(
    USUBJID, PARCAT1, PARAMN, PARAM, BASECAT1 = SHIFT1,
    AVALCAT1 = LBNRIND, AVISIT, AVISITN, ABLFL
  )

#sort lb1 dataset
lb2 <- lb1 %>% 
  arrange(USUBJID, PARAMN, PARAM) %>% 
  group_by(USUBJID, PARAMN, PARAM) %>% 
  filter(row_number()==n()) %>% 
  ungroup() %>% 
  filter(PARCAT1 %in% c("BIOCHEMISTRY", "HEAMATOLOGY"))
#Frequency count
count <- lb2 %>% 
  count(PARCAT1, PARAMN, PARAM, BASECAT1, AVALCAT1)

#Final Dataset
final <- count %>% 
  mutate(
    var = ifelse(is.na(n), "0",
                 ifelse(n==n3, paste0(n, "(100%)"),
                        paste0(n, " (", round(n/n3*100,1),")"))))

#Reshape final dataset
final1 <- final %>%
  pivot_wider(names_from = AVALCAT1,
              values_from = var,
              values_fill= " 0") %>% 
  select(PARCAT1, PARAM, BASECAT1, LOW, NORMAL, HIGH)

final2 <- final1 %>% 
  group_by(PARCAT1) %>% 
  mutate(PARCAT1=ifelse(row_number()==1, PARCAT1, ""))

final3 <- final2 %>% 
  group_by(PARAM) %>% 
mutate(PARAM=ifelse(row_number()==1, PARAM, ""))

#create RTF table
#output location
output <- "//Users//muhammed//R clinical //Output//"

final3 %>% 
  rtf_page(orientation ='landscape') %>% 
  rtf_title(title = "Table 14.4.1 Shift Table from Baseline to Period 01 end(Safety Population)",
            text_justification ="l",
            text_format ="b") %>% 
  rtf_colheader(paste0("Parametr Category| Parameter(Unit)| Baseline | LOW | NORMAL | HIGH"),
                col_rel_width = c(2,4,2,2,2,2),
                text_justification = c("l","l","c" ,"c" ,"c" ,"c")) %>% 
  rtf_body(col_rel_width = c(2,4,2,2,2,2),
           text_justification = c("l","l","c" ,"c" ,"c" ,"c")) %>% 
  rtf_footnote(paste0("Test=Oxybutynin; Reference=CARE(Oxybutynin Succinate
  Data source: Basline is defined as the last observation prior to first drug administration")) %>% 
  rtf_encode() %>% 
  write_rtf(paste0(output, "Table 14.4.1.rtf"))


















































































