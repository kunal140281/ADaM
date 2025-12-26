# Creating :- Listing 16.2.3 Assignment to Analysis Populations
getwd()
rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(stringr)
library(lubridate)
library(tidyr)




path <- "C:\\GOT\\Rprogramming\\"
adsl <- read_sas(paste0(path,"adsl.sas7bdat"))
output <- "C:\\GOT\\Rprogramming\\OUTPUT\\"

analysis_pop <- adsl %>%
  mutate(RANDFL1 = case_when(RANDFL=="Y" ~ "Yes",
                             TRUE ~ "No"),
         SAFFL1 = case_when(SAFFL=="Y" ~ "Yes",
                            TRUE ~ "No"),
         ITTFL1 = case_when(ITTFL=="Y" ~ "Yes",
                            TRUE ~ "No"),
         PPROTFL1 = case_when(PPROTFL=="Y" ~ "Yes",
                              TRUE ~ "No")) %>%
  select(USUBJID,RANDFL1,SAFFL1,ITTFL1,PPROTFL1)
#output location

analysis_pop %>%
  rtf_title("Listing 16.2.3 Assignment to Analysis Populations",
            text_justification = "l",
            text_format = "b") %>%
  rtf_colheader(" | Included in ",
                col_rel_width = c(6,24)) %>%
  rtf_colheader("Subject Number | Safety Population | Randomized population |
ITT Population | PP Population",
                col_rel_width = c(6,6,6,6,6)) %>%
  rtf_body(col_rel_width = c(6,6,6,6,6)) %>%
  rtf_footnote("IIT : Intent to treat,PP : Per Protocol ") %>%
  rtf_encode() %>%
  write_rtf(paste0(output, "Listing 16.2.3.rtf"))

#Creating :- Listing 16.2.4.1 Subject Demographics
rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(stringr)
#path <- "D:/R Training/Learning Materials/Data creation/Projects/ADAM/"
path <- "C:\\GOT\\Rprogramming\\"
adsl <- read_sas(paste0(path, "adsl.sas7bdat"))
demo_pop <- adsl %>%
  mutate(SEX1 = case_when(SEX=="M" ~ "Male",
                          SEX=="F" ~ "Female",
                          TRUE ~ " ") ,
         ETHNIC = str_to_sentence(ETHNIC),
         RACE = str_to_sentence(RACE),
         BHGHTSI1 = as.character(round(BHGHTSI, 2)),
         BWGHTSI1 = as.character(round(BWGHTSI, 2)),
         BBMISI1 = as.character(round(BBMISI, 2))) %>%
  select(USUBJID,AGE , SEX1, ETHNIC, RACE, BHGHTSI1, BWGHTSI1, BBMISI1)
#output location
output <- "C:\\GOT\\Rprogramming\\OUTPUT\\"
demo_pop %>%
  rtf_page(orientation = "landscape") %>%
  rtf_title("Listing 16.2.4.1 Subject Demographics",
            text_justification = "l",
            text_format = "b") %>%
  rtf_colheader("Subject Number | Age(years) | Gender | Ethnic Origin | Race |
Height (cm) | Weight(kg) | BMI (kg/ m^2)",
                col_rel_width = c(11,6,6,6,6,6,6,6)) %>%
  rtf_body(col_rel_width = c(11,6,6,6,6,6,6,6)) %>%
  rtf_footnote("BMI: Body Mass Index.") %>%
  rtf_encode() %>%
  write_rtf(paste0(output, "Listing 16.2.4.1.rtf"))
