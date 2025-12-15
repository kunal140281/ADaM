# to create ADAE PROGRAM
getwd()
rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(stringr)
library(lubridate)
library(tidyr)
library(sqldf)
library(admiral)
OUTPUT <- "C:\\GOT\\Rprogramming\\OUTPUT"
path <- "C:\\GOT\\Rprogramming\\"
path1 <- "C:\\GOT\\Rprogramming\\sdtm\\"
adsl <- read_sas(paste0(path,"adsl.sas7bdat"))
ae <- read_sas(paste0(path1,"ae.sas7bdat")) %>% select(-c("ARM", "ACTARM", "STUDYID"))
suppae1<- read_sas(paste0(path1,"suppae.sas7bdat"))%>% select(-c("ARM", "ACTARM", "STUDYID")) %>% 
  mutate(AESEQ = as.numeric(IDVARVAL)) %>% 
  pivot_wider(id_cols = c("USUBJID",'AESEQ'),
              names_from = "QNAM",
              values_from = "QVAL")
 ae1 <-left_join(ae,suppae1,by = c("USUBJID", "AESEQ"))             
 ae2 <-left_join(ae1,adsl,by = 'USUBJID')
ae3<- ae2 %>% 
                     

  mutate(
    ASEV = case_when(
      AESEV == 'MILD' ~ 'Mild',
      AESEV == 'MODERATE' ~ 'Moderate',
      AESEV == 'SEVERE' ~ 'Severe',
      AESEV == 'LIFE-THREATENING' ~ 'Life-threatening',
      TRUE ~ NA_character_
    ),
    ASEVN = case_when(
      AESEV == 'MILD' ~ 1,
      AESEV == 'MODERATE' ~ 2,
      AESEV == 'SEVERE' ~ 3,
      AESEV == 'LIFE-THREATENING' ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    AREL = case_when(
      AEREL == 'DEFINITELY RELATED' ~ 'Definitely related',
      AEREL == 'PROBABLY RELATED' ~ 'Probably related',
      AEREL == 'POSSIBLY RELATED' ~ 'Possibly related',
      AEREL == 'UNLIKELY RELATED' ~ 'Unlikely related',
      AEREL == 'NOT RELATED' ~ 'Not related',
      TRUE ~ NA_character_
    ),
    ARELN = case_when(
      AEREL == 'DEFINITELY RELATED' ~ 1,
      AEREL == 'PROBABLY RELATED' ~ 2,
      AEREL == 'UNLIKELY RELATED' ~ 3,
      AEREL == 'NOT RELATED' ~ 4,
      TRUE ~ NA_real_
    )
  )%>%
  mutate(
    relgr1 = case_when(
      ARELN %in% c(1, 2, 3) ~ "Related",
      TRUE ~ "Not Related"
    ) ) %>% mutate(AACN = str_to_title(AEACN))  %>%
  derive_vars_dt(
    dtc = DTHDTC,
    new_vars_prefix = "DTH"
  )




ae5 <- ae3 %>%
  # Derive start datetime
  derive_vars_dtm(
    dtc = AESTDTC,
    highest_imputation = "M",
    new_vars_prefix = "AST",
    date_imputation = "first",
    time_imputation = "00:00:00",
    min_dates = vars(TRTSDT)
  ) %>%
  # Derive end datetime  
  derive_vars_dtm(
    dtc = AEENDTC,
    highest_imputation = "M",
    new_vars_prefix = "AEN", 
    date_imputation = "last",
    time_imputation = "23:59:59",
    min_dates = vars(DTHDT,EOSDT)
  ) %>% 
  derive_vars_dtm_to_dt(source_vars = exprs(ASTDTM) ) %>% 
  derive_vars_dtm_to_dt(
    source_vars = exprs(AENDTM))%>%
  mutate(ASTDY = ifelse(!is.na(ASTDT) < !is.na(TRTSDT) , ASTDT-TRTSDT, ASTDT-TRTSDT+1),
         AENDY = ifelse(!is.na(AENDT) < !is.na(TRTSDT) , AENDT-TRTSDT, AENDT-TRTSDT+1),
         TRTEMFL = case_when(AETRTEM=="Y" ~"Y"),
         TRTEMFL = ifelse((!is.na(ASTDT) >= !is.na(TRTSDT) | !is.na(ASTDT) <= !is.na(RFPENDTC)),
                          "Y", ""))
