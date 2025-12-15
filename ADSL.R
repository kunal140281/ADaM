
# to create ADSL PROGRAM
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
path_sdtm <- "C:\\GOT\\Rprogramming\\sdtm\\"

sv <- read_sas(paste0(path_sdtm,"sv.sas7bdat"))
face <- read_sas(paste0(path_sdtm,"face.sas7bdat"))
vs <- read_sas(paste0(path_sdtm,"vs.sas7bdat"))
ds <- read_sas(paste0(path_sdtm,"ds.sas7bdat"))
dm<- read_sas(paste0(path_sdtm,"dm.sas7bdat"))
suppdm <- read_sas(paste0(path_sdtm,"suppdm.sas7bdat")) %>%
  pivot_wider(id_cols = "USUBJID",
              names_from = "QNAM",
              values_from = "QVAL")
dm1 <- left_join(dm, suppdm, by = "USUBJID") %>%
  mutate(AGEGR1 = case_when(AGE < 40 ~ "< 40 years old",
                            AGE >= 40 ~ ">= 40 years old"),
         SEXN = case_when(SEX =="F" ~ 2 ,
                          SEX =="M" ~ 1),
         ETHNICN = case_when(ETHNIC =="HISPANIC OR LATINO" ~ 1,
                             ETHNIC =="NOT HISPANIC OR LATINO" ~ 2,
                             ETHNIC =="UNKNOWN" ~ 3,
                             ETHNIC =="NOT REPORTED" ~4),
         RACEN = case_when(RACE =="AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
                           RACE =="ASIAN" ~ 2,
                           RACE =="BLACK OR AFRICAN AMERICAN" ~ 3,
                           RACE =="NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 4,
                           RACE == "WHITE" ~ 5,
                           RACE =="OTHER" ~ 6,
                           RACE =="NOT REPORTED" ~ 7,
                           RACE =="" ~ 8)) %>%
  #Derived variables
  mutate(TRT01P = case_when(ARMCD=="TQ" ~ "Tafenoquine",
                            ARMCD=="PLACEBO" ~ "Placebo"),
         TRT01PN = case_when(ARMCD=="TQ" ~ 1,
                             ARMCD=="PLACEBO" ~ 2),
         TRT01A = case_when(ACTARMCD== "TQ" ~ "Tafenoquine",
                            ACTARMCD=="PLACEBO" ~ "Placebo"),
         TRT01AN = case_when(ACTARMCD=="TQ" ~ 1,
                             ACTARMCD=="PLACEBO" ~ 2),
         #creation of TRTSDTM and other time variables
         TRTSDTM = case_when(RFXSTDTC!=" " ~ ymd_hm(RFXSTDTC),
                             TRUE ~ NA),
         TRTSDT = as.Date(RFXSTDTC),
         TRTSDTM = ymd_hm(RFXSTDTC),
         TRTSDT = as.Date(RFXSTDTC),
         TRTEDTM = ymd_hm(RFXENDTC),
         TRTEDT = as.Date(RFXENDTC),
         TRTDURD = (TRTEDT - TRTSDT)+1,
         #Creation of flag variables
         RFICDTC1 = as.Date(RFICDTC),
         #Screened pop flag
         SCRNFL = case_when(!is.na(RFICDTC1) ~"Y"),
         #Safety pop flag
         RFXSTDTC1 = as.Date(RFXSTDTC),
         SAFFL = case_when(!is.na(RFXSTDTC1) ~"Y")) #%>%
#select(TRT01P,ARMCD, TRT01PN,TRT01A,ACTARMCD, TRT01AN)
# Adding the baseline records for height, weight and BMI
vs1 <- vs %>%
  filter(VISIT=="Screening/Day -4 to -1" & VSTESTCD %in% c('HEIGHT','WEIGHT')) %>%
  pivot_wider(id_cols = USUBJID,
              names_from = VSTESTCD,
              values_from = VSSTRESN) %>%
  mutate(BBMISI = WEIGHT/(HEIGHT**2)*10000) %>%
  rename(BHGHTSI = HEIGHT,
         BWGHTSI = WEIGHT)
dm2 <- dplyr::left_join(dm1, vs1 , by = "USUBJID")
'%notin%' <- Negate('%in%')
ds1 <- ds %>%
  mutate(
    sel = (DSCAT=="DISPOSITION EVENT" & DSSCAT == "END OF STUDY/EARLY TERMINATION"),
    EOSSTT = case_when(sel == TRUE & DSDECOD=="COMPLETED" ~"Completed",
                       sel == TRUE & DSDECOD!="COMPLETED" ~"Discontinued"),
    DCSREAS = case_when(sel == TRUE & DSDECOD!="COMPLETED" ~ DSDECOD),
    DCSREASP = case_when(sel == TRUE & DSDECOD=="OTHER" ~ DSTERM)) %>%
  mutate(
    sel2 = (DSCAT== "DISPOSITION EVENT"),
    EOSDT = as.Date(DSSTDTC)) %>%
  filter(EOSSTT!= " ")%>%
  select(USUBJID,EOSDT,DCSREASP,EOSSTT,DCSREAS )
dm3 <- dplyr::left_join(dm2, ds1 , by= "USUBJID")
rand <- ds %>%
  mutate(
    sel1 = (DSDECOD== "RANDOMIZED" & DSSTDTC!=""),
    RANDDT = case_when(sel1 ~ as.Date(DSSTDTC)),
    RANDFL = case_when(sel1 ~ "Y"),
    ITTFL = case_when(sel1 ~ "Y")) %>%
  filter(!is.na(RANDDT)) %>%
  select(USUBJID,RANDDT,RANDFL,ITTFL)
dm4 <- dplyr::left_join(dm3, rand , by= "USUBJID")
#DAY 14
day14 <- sv %>%
  filter(VISITNUM==15) %>%
  select(USUBJID,VISITNUM)
dm5 <- dplyr::left_join(dm4, day14 , by= "USUBJID") %>%
  mutate(VISITNUM = replace_na(VISITNUM,99)) %>%
  mutate(
    PPROTFL = ifelse((VISITNUM==15 & ITTFL=="Y"), "Y","N")) #%>%
# select(USUBJID,PPROTFL,VISITNUM,ITTFL)
face1 <- face %>%
  mutate(
    #derivation of COVD14FL
    sel1 = ((FAOBJ) %in% c("Cough","Shortness of breath (difficulty breathing)") & VISITNUM <=15),
    COVD14FL = case_when(sel1 ~"Y")) %>%
  mutate(
    #derivation of COVD28FL
    sel1 = ((FAOBJ) %in% c("Cough","Shortness of breath (difficulty breathing)") & VISITNUM <=28),
    COVD28FL = case_when(sel1 ~"Y")) %>%
  filter(FAOBJ %in% c("Cough","Shortness of breath (difficulty breathing)")) %>%
  distinct(USUBJID, .keep_all = T) %>%
  select(COVD14FL,COVD28FL, USUBJID)
dm6 <- dplyr::left_join(dm5, face1 , by= "USUBJID") %>%
  rename(HOSPCOFL = hospcofl) %>%
  mutate(EOSSTT = case_when(EOSSTT==" " & (!is.na(TRTSDT)) ~"Ongoing")) %>%
  select(AGEGR1,
         SEXN,
         RACEN,
         ETHNICN,
         RANDFL,
         RANDDT,
         SCRNFL,
         SAFFL,
         ITTFL,
         PPROTFL,
         TRT01P,
         TRT01PN,
         TRT01A,
         TRT01AN,
         TRTSDTM,
         TRTSDT,
         TRTEDTM,
         TRTEDT,
         TRTDURD,
         EOSSTT,
         EOSDT,
         DCSREAS,
         DCSREASP,
         BBMISI,
         BHGHTSI,
         BWGHTSI,
         MFUV,
         VCYN,
         VCNUM,
         COVD14FL,
         COVD28FL,
         HOSPCOFL,
         STUDYID,
         USUBJID,
         SUBJID,
         RFSTDTC,
         RFENDTC,
         RFXSTDTC,
         RFXENDTC,
         RFICDTC,
         RFPENDTC,
         DTHDTC,
         DTHFL,
         SITEID,
         BRTHDTC,
         AGE,
         AGEU,
         SEX,
         RACE,
         ETHNIC,
         ARMCD,
         ARM,
         ACTARMCD,
         ACTARM,COUNTRY)

