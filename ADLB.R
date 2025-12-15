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
lb <- read_sas(paste0(path1,"lb.sas7bdat"))%>% select(-c("ARM", "ACTARM", "STUDYID"))
supplb  <- read_sas(paste0(path1,"supplb.sas7bdat")) %>% select(-c("ARM", "ACTARM", "STUDYID"))
# merge lb and supplb
lb1 <- left_join(lb, supplb, by = 'USUBJID')
# MERGE LB1 WITH ADSL
lb2 <- left_join(lb1, adsl, by = 'USUBJID') %>% 
  mutate(PARAM = case_when(
    !is.na(LBTEST) & LBSTRESU != "" ~ paste0(LBTEST,"(",LBSTRESU,")"),
    TRUE ~ LBTEST
  )) %>%
  rename(
    
    AVAL = LBSTRESN,
    AVALC = LBSTRESC,
    PARCAT1 = LBCAT
  ) %>% 
  mutate(AVISIT = case_when(VISIT != "UNSCHEDULED" ~VISIT,
                            TRUE ~""),
         AVISIT1 = case_when(VISIT != "UNSCHEDULED" ~ as.character(VISITNUM),
                            TRUE ~ as.character(NA)),
         
         AVISITN = as.numeric(AVISIT1),
         
         ANL01FL =  case_when(AVISIT != "UNSCHEDULED" ~"Y",
                              TRUE ~""),
         PARAMN1 = match(LBTESTCD, sort(unique(LBTESTCD))),
         PARAMN = as.numeric(factor(LBTESTCD, levels = sort(unique(LBTESTCD)))),
         ADTM = as.numeric(as.POSIXct(LBDTC, format = "%Y-%m-%dT%H:%M")),
         PARAMCD = LBTESTCD,
         ANRLO = LBSTNRLO,
         ANRHI = LBSTNRHI,
         ANRIND = LBNRIND,
         TRTP = TRT01P,
         TRTPN = TRT01PN,
         TRTA = TRT01A,
         TRTAN = TRT01AN
         ) %>% 
  separate(LBDTC, into = c("DATE","TIME"), sep = "T",
           remove = F)%>% 
  mutate(ADT = as.Date(DATE),
         ATM = hm(TIME),
         ADY = ifelse(!is.na(ADT) < !is.na(TRTSDT) , ADT-TRTSDT, ADT-TRTSDT+1)
         
         ) 

# DERIVE PARAMN
PARAMN <- lb1 %>% 
  select(LBTESTCD) %>% distinct() %>%    mutate(PARAMN = row_number())
#          colnames(lb2)
#          
#  lb3<- left_join(lb2,PARAMN, by = "LBTESTCD") 
         
 #data set prepartion for the baseline
 lb3 <- lb2 %>% filter((!is.na(ADT)) & (!is.na(TRTSDT))) %>%
   filter((ADT <= TRTSDT ) & (!is.na(AVAL))) %>%
   arrange(USUBJID, desc(AVISITN), AVAL, PARAMN) %>%
   distinct(USUBJID,PARAMN, .keep_all = T) %>%
   mutate(ABLFL="Y") %>%
   arrange(USUBJID,PARAMN, AVISITN,ABLFL) %>%
   select(USUBJID,PARAMN, AVISITN,ABLFL)
 #baseline
 base <- lb3 %>% dplyr::right_join(lb2, by=c("USUBJID","PARAMN","AVISITN")) %>%
   mutate(BASE = ifelse(ABLFL=="Y", AVAL, NA),
          BASEC = ifelse(ABLFL=="Y", as.character(AVAL), " "),
          BNRIND =ifelse(ABLFL=="Y", LBNRIND,"")) %>%
   select(USUBJID,PARAMN, ABLFL, BASE, BASEC,BNRIND) %>%
   filter(BASE!="")       
         
lb4 <- dplyr::left_join(lb2, base , by = c("USUBJID","PARAMN")) %>%
   arrange(USUBJID,PARAMN, AVISITN) %>%
   group_by(USUBJID,PARAMN) %>%
   mutate(
     "First" = row_number()== min(row_number()),
     BASE = ifelse(First =="TRUE",NA, BASE),
     BASEC = ifelse(First =="TRUE"," ", as.character(AVAL)),
     CHG = case_when(!is.na(BASE) & !is.na(AVAL) ~ AVAL-BASE),
     PCHG = case_when(!is.na(BASE) & !is.na(AVAL) ~ ((AVAL-BASE)/BASE)*100)) %>% 
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
         TRTP,
         TRTPN,
         TRTA,
         TRTAN,
         TRTSDTM,
         TRTSDT,
         TRTEDTM,
         TRTEDT,
         ADTM ,
         ADT ,
         ATM ,
         ADY ,
         PARCAT1 ,
         PARAM ,
         PARAMN,
         PARAMCD ,
         AVAL,
         AVALC ,
         ABLFL ,
         BASE ,
         BASEC ,
         CHG ,
         PCHG ,
         ANRLO,
         ANRHI,
         ANRIND,
         BNRIND,
         VISITNUM ,
         VISIT ,
         AVISIT ,
         AVISITN ,
         ANL01FL
  )
         