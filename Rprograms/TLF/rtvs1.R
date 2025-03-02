#File name: rtvs1
# Author: Ali
# Date: 27FEB2025
# Project/Study:
# Description: <To Develop The Table 14.3.1 Summary of Changes in Vital Signs from Baseline to Final Visit(Safety Population)
# Input: Adam.ADAE
# Output: Table 14.3.1 Summary of Changes in Vital Signs from Baseline to Final Visit(Safety Population)
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
advs<- read_sas(paste0(path_adam, "ADVS.sas7bdat"))

#filter subject in safety population
demog <- adsl %>% 
  filter(SAFFL == "Y")

#process treatment Assignment for TRT01A
demog_1 <- demog %>% 
  mutate(TRT01A = "ALL") %>% 
  bind_rows(demog)
   
#process treatment assignment for TRT01A
demog_2 <- demog_1 %>% 
  mutate(
    TRT = case_when(
      str_detect(TRT01A, "TEST") ~ "TES",
      str_detect(TRT01A, "REFE") ~ "REF",
      str_detect(TRT01A, "ALL") ~ "ALL"
    ),
    ORD = case_when(TRT =="TES" ~ 1, TRT=="REF" ~ 2, TRT=="ALL" ~ 3)) %>% 
  select(USUBJID, AGE, TRT, ORD) %>% 
  filter(!is.na(ORD))

#process treatment Assignment for TRT02A
demog_11 <- demog %>% 
  mutate(TRT02A = "ALL") %>% 
  bind_rows(demog)

#process treatment assignment for TRT02A
demog_22 <- demog_11 %>% 
  mutate(
    TRT = case_when(
      str_detect(TRT02A, "TEST") ~ "TES",
      str_detect(TRT02A, "REFE") ~ "REF",
      str_detect(TRT02A, "ALL") ~ "ALL"
    ),
    ORD = case_when(TRT =="TES" ~ 1, TRT=="REF" ~ 2, TRT=="ALL"~3)) %>% 
  select(USUBJID, AGE, TRT, ORD) %>% 
  filter(!is.na(ORD))

#combine demographic data
demog_final <- bind_rows(demog_2, demog_22) %>% 
  distinct(USUBJID, TRT, .keep_all=TRUE)

#count unique subjects per treatment
n_counts <- demog_final %>% 
  group_by(ORD) %>% 
  summarise(N= n_distinct(USUBJID)) %>% 
  arrange(ORD)

n1 <- n_counts$N[1]
n2 <- n_counts$N[2]

cat("N1:", n1, "N2:", n2, "\n")

#filter and process vital signs data
advs1 <- advs %>% 
  filter(SAFFL =="Y" , AVAL!= ".", paramn %in% c(7), AVISIT!="SCREENING") %>% 
  mutate(
    TRT = case_when(
      str_detect(TRTA, "TEST") ~ "TES",
      str_detect(TRTA, "REFE") ~ "REF"
    ),
    ORD = case_when(TRT == "TES" ~ 1, TRT == "REF" ~ 2)
  ) %>% 
  select(USUBJID, PARAM, paramn, AVISIT, AVISITN, TRT, ORD, AVAL, ATPT, ATPTN, CHG)

advs2<- advs1 %>% 
  mutate(
    AVAL = as.numeric(AVAL))

#Summary statistics calculation
vs <- advs2 %>% 
  group_by(paramn, PARAM, AVISITN, AVISIT, ATPTN, ATPT, TRT) %>% 
  summarise(
    n=n(),
    mean= mean(AVAL, na.rm=TRUE),
    median = median(AVAL, na.rm=TRUE),
    std = sd(AVAL, na.rm=TRUE),
    min = min(AVAL, na.rm=TRUE),
    max = max(AVAL, na.rm=TRUE)
  ) %>% 
  ungroup()
  
#convert numeric values to character format (like SAS put/compress)
vs2 <- vs %>% 
  mutate(
    n= as.character(n),
    mean = formatC(mean, format = "f", digits =1),
    median = formatC(median, format = "f", digits =1),
    std = formatC(std, format = "f", digits =2),
    min = as.character(min),
    max = as.character(max)
  )

#Transpose data
vs3 <- vs2 %>% 
  pivot_longer(cols=c("n","mean","median","std","min", "max"),
               names_to ="des",
               values_to = "val") %>% 
pivot_wider(id_cols =c("PARAM","paramn","AVISIT","AVISITN","ATPTN", "ATPT","des"),
              names_from ="TRT",
              values_from = "val",
              values_fn = list(val=first))

vs4 <- vs3 %>% 
  mutate(
    od= case_when(des=="n" ~ 1,
                  des=="mean" ~ 2,
                  des=="median" ~ 3,
                  des=="std" ~ 4,
                  des=="min" ~ 5,
                  des=="max" ~ 6)
  )

#Summary statistics calculation CHG
vs_ <- advs2 %>% 
  group_by(paramn, PARAM, AVISITN, AVISIT, ATPTN, ATPT, TRT) %>% 
  summarise(
    n=n(),
    mean= mean(CHG, na.rm=TRUE),
    median = median(CHG, na.rm=TRUE),
    std = sd(CHG, na.rm=TRUE),
    min = min(CHG, na.rm=TRUE),
    max = max(CHG, na.rm=TRUE)
  ) %>% 
  ungroup()

#convert numeric values to character format (like SAS put/compress)
vs2_ <- vs_ %>% 
  mutate(
    n= as.character(n),
    mean = formatC(mean, format = "f", digits =1),
    median = formatC(median, format = "f", digits =1),
    std = formatC(std, format = "f", digits =2),
    min = as.character(min),
    max = as.character(max)
  )

#Transpose data
vs3_ <- vs2_ %>% 
  pivot_longer(cols=c("n","mean","median","std","min", "max"),
               names_to ="des",
               values_to = "val") %>% 
  pivot_wider(id_cols =c("PARAM","paramn","AVISIT","AVISITN","ATPTN", "ATPT","des"),
              names_from ="TRT",
              values_from = "val",
              values_fn = list(val=first))

vs4_ <- vs3_ %>% 
  mutate(
    od= case_when(des=="n" ~ 1,
                  des=="mean" ~ 2,
                  des=="median" ~ 3,
                  des=="std" ~ 4,
                  des=="min" ~ 5,
                  des=="max" ~ 6)
  )


#sorting
vs4 <- vs4 %>%
  arrange(paramn, PARAM, AVISITN, AVISIT, ATPTN, ATPT,od)
vs4_ <- vs4_ %>%
  arrange(paramn, PARAM, AVISITN, AVISIT, ATPTN, ATPT, od)

final <- merge(vs4, vs4_, by = c("paramn","PARAM","AVISITN","AVISIT","ATPTN","ATPT","od"), all=TRUE)

final <-final %>% 
  mutate(
    ATPT = case_when(
      ATPT =="0" ~ "Baseline",
      ATPT == "2" ~ "2H",
      ATPT == "8" ~ "8H",
      TRUE ~ ATPT
    )) %>% 
  arrange(paramn, PARAM, AVISITN, AVISIT, ATPTN, ATPT,od) %>% 
  select(PARAM, AVISIT, ATPT, des.x, TES.x, TES.y, REF.x, REF.y)


#output location
output <- "//Users//muhammed//R clinical //Output//"

#final <- as.data.frame(final)
final1 <- final %>% 
  group_by(PARAM) %>% 
  mutate(PARAM=ifelse(row_number()==1, PARAM, ""))

final2 <- final1 %>% 
  group_by(AVISIT) %>% 
  mutate(AVISIT=ifelse(row_number()==1, AVISIT, ""))

final3 <- final2 %>% 
  group_by(ATPT) %>% 
  mutate(ATPT=ifelse(row_number()==1, ATPT, ""))


final3%>% 
  rtf_page(orientation ='landscape') %>% 
  rtf_title(title = "Table 14.3.1 Summary of Changes in Vital Signs from Baseline to Final Visit(Safety Population)",
            text_justification ="l",
            text_format ="b") %>% 
  rtf_colheader(paste0("Parameter(Unit)|Visit|Time Point|Statistic|
                       Observed value(Tes)|Change from Baseline(Test)|
                       Observed value(Reference)|Change from Baseline(Reference)"),
                col_rel_width = c(3,3,2,3,3,3,3,4),
                text_justification = c("l","l","c","c","c","c","c","c")) %>% 
  rtf_body(col_rel_width = c(3,3,2,3,3,3,3,4),
           text_justification = c("l","l","c","c","c","c","c","c")) %>% 
  rtf_footnote(paste0("Test=Oxybutynin; Reference=CARE(Oxybutynin Succinate
 SD: Standard Deviation
 Baseline is defined as the last observation prior to first drug administration
  Data source: Table 14.3.1")) %>% 
  rtf_encode() %>% 
  write_rtf(paste0(output, "Table 14.3.1.rtf"))















































