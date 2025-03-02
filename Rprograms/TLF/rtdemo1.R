#File name: rldemo1
# Author: Ali
# Date: 25FEB2025
# Project/Study:
# Description: <To Develop The Table 14.1.1 Subject Demographics-Age(Safety Population)>
# Input: Adam.ADSL
# Output: Table 14.1.1 Subject Demographics
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

#Filter the safety Population
adsl1 <- adsl %>%  filter(SAFFL=="Y")

#Create Treatment Groups
demog_1 <- adsl1 %>% 
  mutate(TRT01A = "ALL") %>% 
  bind_rows(adsl1)

demog_2 <- demog_1 %>% 
  mutate(
    trt = case_when(
      grepl("TEST", TRT01A) ~ "TES",
      grepl("REFE", TRT01A) ~ "REF",
      TRT01A == "ALL" ~ "ALL"
    ),
    ord = case_when(
      trt =="TES" ~ 1,
      trt =="REF" ~ 2,
      trt =="ALL" ~ 3
    )) %>% 
  select(USUBJID, AGE, SEX, RACE, trt, ord) %>% 
  filter(!is.na(ord))

demog_12 <- adsl1 %>% 
  mutate(TRT02A = "ALL") %>% 
  bind_rows(adsl1)

demog_22 <- demog_12 %>% 
  mutate(
    trt = case_when(
      grepl("TEST", TRT02A) ~ "TES",
      grepl("REFE", TRT02A) ~ "REF",
      TRT02A == "ALL" ~ "ALL"
    ),
    ord = case_when(
      trt =="TES" ~ 1,
      trt =="REF" ~ 2,
      trt =="ALL" ~ 3
    )) %>% 
  select(USUBJID, AGE, SEX, RACE, trt, ord) %>% 
  filter(!is.na(ord))
#Merge datasets
demog_3 <- bind_rows(demog_2, demog_22) %>% 
  arrange(ord) %>% 
  distinct(USUBJID, trt, .keep_all = TRUE)

#get counts for treatment groups
n_counts <- demog_3 %>% 
  group_by(ord) %>% 
  summarise(n=n_distinct(USUBJID)) %>% 
  arrange(ord) %>% 
  pull(n)

n1 <- n_counts[1]
n2 <- n_counts[2]
n3 <- n_counts[3]

#Frequency table for SEX
gender <- demog_3 %>% 
  count(SEX, trt) %>% 
  mutate(
    cat="Gender",
    stat = ifelse(SEX=="M", "Male", "Female"),
    sort = ifelse(SEX=="M", 1, 2)
  ) %>% 
  select(cat, sort, stat, trt, n)

gender_ <- gender %>% 
  pivot_wider(names_from=trt,
              values_from = n,
              values_fill = 0)

#Frequency table for RACE
race <- demog_3 %>% 
  count(RACE, trt) %>% 
  mutate(
    cat = "Race",
    stat= case_when(
      RACE == "ASIAN" ~ "Asian",
      RACE == "OTHER" ~ "Other"
    ),
    sort= case_when(
      RACE =="ASIAN" ~ 1,
      RACE =="OTHER" ~ 2
    )
  ) %>% 
  select(cat, sort, stat, trt, n)
  
race_ <- race %>% 
  pivot_wider(names_from=trt,
              values_from = n,
              values_fill = 0)
#Combine Tables
final <- bind_rows(gender_, race_)%>% 
  mutate(
    test = ifelse(is.na(TES), "0", paste0(TES, "(", round(TES/n1 * 100, 1), ")")),
    refe = ifelse(is.na(REF), "0", paste0(REF, "(", round(REF/n2 * 100, 1), ")")),
    allp = ifelse(is.na(ALL), "0", paste0(ALL, "(", round(ALL/n3 * 100, 1), ")"))
  )

final1 <- final %>% 
  arrange(cat,sort) %>% 
  group_by(cat) %>% 
  mutate(cat=ifelse(row_number()==1, cat, "")) %>% 
  #ungroup() %>% 
  select(cat,stat,test,refe, allp)

#output location
output <- "//Users//muhammed//R clinical //Output//"

final1 %>% 
  rtf_page(orientation ='landscape') %>% 
  rtf_title(title = "Table 14.1.1 Subject Demographics-Age(Safety Population)",
            text_justification ="l",
            text_format ="b") %>% 
  rtf_colheader(paste0("Category|Statistics|Test \n(N=",n1,"),|Reference \n(N=", n2,") | 
  Overall\n(N=",n3,")"),
                col_rel_width = c(11,6,6,6,6),
                text_justification = c("l","c","c","c","c")) %>% 
  rtf_body(col_rel_width = c(11,6,6,6,6),
           text_justification = c("l","c","c","c","c")) %>% 
  rtf_footnote(paste0("Test=Oxybutynin; Reference=CARE(Oxybutynin Succinate
                      n(%): Number and percent of subjects
                      Data source: Listing 16.2.4.1")) %>% 
  rtf_encode() %>% 
  write_rtf(paste0(output, "Table 14.1.2.rtf"))

                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



























  
  
  
  
  
  
  
  
  
  
  
  
  
