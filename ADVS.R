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
vs <- read_sas(paste0(path1,"vs.sas7bdat"))%>% select(-c("ARM", "ACTARM", "STUDYID"))
#suppvs  <- read_sas(paste0(path1,"suppvs.sas7bdat")) %>% select(-c("ARM", "ACTARM", "STUDYID"))

# MERGE VS WITH ADSL
vs1 <- left_join(vs, adsl, by = 'USUBJID') %>% 
  mutate(AVISIT = VISIT,
         AVISITN = VISITNUM,
         PARAM = paste0(VSTEST,"(",VSSTRESU,")"),
         PARAMCD = VSTESTCD,
         PARAMN = case_when( PARAM == 'Systolic Blood Pressure(mmHg)' ~1,
                             PARAM == 'Temperature(C)' ~2,
                             PARAM == 'Heart Rate(beats/min)' ~3,
                             PARAM == 'Diastolic Blood Pressure(mmHg)' ~4,
                             PARAM == 'Respiratory Rate(breaths/min)' ~5,
                             PARAM == 'Oxygen Saturation(%)' ~6,
                             PARAM == 'Weight(kg)' ~7,
                             PARAM == 'Height(cm)' ~8,
                             TRUE ~NA),
         AVAL = VSSTRESN,
         AVALC = VSSTRESC) %>% 
  separate(VSDTC,
           into = c("DT","TIME"),
           sep = "T",
           remove = F)%>% 
  mutate(ADT = as.Date(DT),
         ATM = hm(TIME),
         ADY = ifelse(!is.na(ADT) < !is.na(TRTSDT) , ADT-TRTSDT, ADT-TRTSDT+1)
         ) 
# Create Baseline records
vs2 <- vs1 %>%  filter(!is.na(ADT) & !is.na(TRTSDT)) %>% 
  filter(ADT <= TRTSDT & !is.na(AVAL) ) %>% 
  arrange(USUBJID,desc(AVISITN), AVAL, PARAMN) %>% 
  distinct(USUBJID, PARAMN, .keep_all = T) %>% 
  mutate(ABLFL = "Y") %>% 
  arrange(USUBJID, PARAMN, AVISITN,ABLFL) %>% 
  select(USUBJID, PARAMN, AVISITN,ABLFL)

# 
base <- vs2 %>% right_join(vs1, by = c("USUBJID", "PARAMN", "AVISITN")) %>% 
  mutate (BASE= ifelse(ABLFL == "Y", AVAL, NA),
         BASEC= ifelse(ABLFL == "Y", as.character(AVAL), " ")) %>% 
  select(USUBJID, PARAMN,ABLFL,BASE,BASEC) %>% filter(BASE!="")

vs3 <- dplyr::left_join(vs1,base,by = c("USUBJID", "PARAMN")) %>% 
  arrange(USUBJID, PARAMN, AVISITN) %>% 
  group_by(USUBJID, PARAMN) %>% 
  mutate(CHG = case_when(!is.na(BASE) & !is.na(AVAL) ~ AVAL-BASE),
         PCHG = case_when(!is.na(BASE) & !is.na(AVAL) ~ (CHG/BASE)*100)) %>% 
# Creation of flags
   mutate(
    ONTRTFL = ifelse(!is.na(ADT) & !is.na(TRTSDT) & !is.na(TRTEDT) & 
                     ADT >= TRTSDT & ADT <= TRTEDT, "Y", "N"),
    ANL01FL = case_when(
      AVISIT != "UNSCHEDULED" ~ "Y",
      TRUE ~ ""
    )
  ) %>% 
  mutate(
    CRIT1 = case_when(
      # Systolic Blood Pressure
      PARAMCD == "SYSBP" & !is.na(AVAL) & (AVAL < 90 | AVAL > 120) ~ 
        "Systolic Blood Pressure (mmHg) <90 or Systolic Blood Pressure (mmHg) >120",
      
      # Diastolic Blood Pressure
      PARAMCD == "DIABP" & !is.na(AVAL) & (AVAL < 60 | AVAL > 80) ~ 
        "Diastolic Blood Pressure (mmHg) <60 or Diastolic Blood Pressure (mmHg) >80",
      
      # Respiratory Rate
      PARAMCD == "RESP" & !is.na(AVAL) & (AVAL < 12 | AVAL > 18) ~ 
        "Respiratory Rate (breaths/min) <12 or Respiratory Rate (breaths/min) >18",
      
      # Heart Rate
      PARAMCD == "HR" & !is.na(AVAL) & (AVAL < 60 | AVAL > 90) ~ 
        "Heart Rate (beats/min) <60 or Heart Rate (beats/min) >90",
      
      # Temperature
      PARAMCD == "TEMP" & !is.na(AVAL) & (AVAL < 36.5 | AVAL > 37.3) ~ 
        "Temperature (C)<36.5 or Temperature (C) > 37.3",
      
      # Oxygen Saturation
      PARAMCD == "OXYSAT" & !is.na(AVAL) & (AVAL < 95 | AVAL > 100) ~ 
        "Oxygen Saturation (%) <95 or Oxygen Saturation (%) >100",
      
      # If none of the above conditions are met
      TRUE ~ ""
    )
  ) %>% 
  mutate(
    CRIT1FL = ifelse(!is.na(CRIT1) & CRIT1 != "", "Y", "")
  )%>% 
  mutate(
    CRIT2 = case_when(
      # Systolic Blood Pressure
      PARAMCD == "SYSBP" & !is.na(AVAL) & (AVAL < 90 | AVAL > 140) ~ 
        "Systolic Blood Pressure (mmHg) <90 or Systolic Blood Pressure (mmHg) >140",
      
      # Diastolic Blood Pressure
      PARAMCD == "DIABP" & !is.na(AVAL) & (AVAL < 50 | AVAL > 90) ~ 
        "Diastolic Blood Pressure (mmHg) <50 or Diastolic Blood Pressure (mmHg) >90",
      
      # Respiratory Rate
      PARAMCD == "RESP" & !is.na(AVAL) & (AVAL < 10 | AVAL > 30) ~ 
        "Respiratory Rate (breaths/min) <10 or Respiratory Rate (breaths/min) >30",
      
      # Heart Rate
      PARAMCD == "HR" & !is.na(AVAL) & (AVAL < 40 | AVAL > 100) ~ 
        "Heart Rate (beats/min) <40 or Heart Rate (beats/min) >100",
      
      # Temperature
      PARAMCD == "TEMP" & !is.na(AVAL) & (AVAL < 34.9 | AVAL > 39.5) ~ 
        "Temperature (C)<34.9 or Temperature (C) > 39.5",
      
      # Oxygen Saturation
      PARAMCD == "OXYSAT" & !is.na(AVAL) & (AVAL < 92 | AVAL > 100) ~ 
        "Oxygen Saturation (%) <92 or Oxygen Saturation (%) >100",
      
      # If none of the above conditions are met
      TRUE ~ ""
    )
  ) %>% 
  mutate(
    CRIT2FL = ifelse(!is.na(CRIT2) & CRIT2 != "", "Y", "")
  ) %>% 
  select(STUDYID,
         USUBJID,
         SUBJID,
         SITEID,
         AGE,
         AGEU,
         SEX,
         RACE,
         ETHNIC,
         COUNTRY,
         SAFFL,
         ITTFL,
         PPROTFL,
         RANDFL,
         TRT01P,
         TRT01PN,
         TRT01A,
         TRT01AN,
         TRTSDT,
         TRTEDT,
         VSSEQ,
         VISIT,
         VISITNUM,
         VSDTC,
         AVISIT,
         AVISITN,
         PARAM,
         PARAMN,
         PARAMCD,
         AVALC,
         AVAL,
         ADT,
         ADTM,
         ADY ,
         ABLFL,
         BASE,
         BASEC,
         CHG,
         PCHG,
         ANL01FL,
         ONTRTFL,
         CRIT1,
         CRITFL,
         CRIT2,
         CRIT2FL
  )

colnames(adsl)
     



