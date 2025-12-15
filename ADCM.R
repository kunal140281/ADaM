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
cm <- read_sas(paste0(path1,"cm.sas7bdat"))%>% select(-c("ARM", "ACTARM", "STUDYID"))
suppcm  <- read_sas(paste0(path1,"suppcm.sas7bdat")) %>% select(-c("ARM", "ACTARM", "STUDYID"))
# transposing in the suppcm datasets

suppcm1 <- suppcm %>% mutate(CMSEQ=as.numeric(IDVARVAL)) %>% 
  pivot_wider(id_cols = c(USUBJID,CMSEQ),
              names_from = QNAM,
              values_from = QVAL)
# merge cm and suppcm
cm1 <- left_join(cm,suppcm1, by = c("USUBJID","CMSEQ")) 
# join cm1 and adsl
cm_ <- cm1 %>% inner_join(adsl,by = "USUBJID") %>% 
# date imputation 
mutate(LENS= nchar(CMSTDTC),
       LEN_EDT = nchar(CMENDTC)) %>% 
#SEPARATE CMSTDTC to date and time
  separate(CMSTDTC,
           into = c("SDT","STIME"),
           sep="T",
           remove = F) %>% 
#SEPARATE CMENDTC to date and time
  separate(CMENDTC,
           into = c("EDT","ETIME"),
           sep="T",
           remove = F) #%>% 
  #select(LENS, CMSTDTC, SDT,STIME,CMENDTC,EDT,ETIME)
cm2 <- cm_ %>%  mutate(
  ATC4 = CODE4,
  ATC4TXT = TEXT4
  ) %>% 
  mutate(CMSTDTC_C = case_when( LENS == 4 ~ paste0(SDT,"-01-01"),
                                LENS == 7 ~ paste0(SDT,"-01"),
                                LENS == 10 ~ SDT,
                                LENS == 0 ~ NA_character_ )) %>% 
 mutate(ASTDTF = case_when( LENS == 4 ~ "M",
                                LENS == 7 ~ "D",
                                LENS == 10 ~ " ",
                                LENS == 0 ~ NA_character_ )) %>% 
  mutate(CMENDTC_C = case_when(  LEN_EDT == 4 ~ paste0(EDT,"-12-31"),
                                 LEN_EDT == 7 ~ paste0(EDT,"-30"),
                                 LEN_EDT == 10 ~ EDT,
                                 LEN_EDT == 0 ~ NA_character_ )) %>% 
  mutate(AENDTF = case_when(  LEN_EDT == 4 ~ "M",
                              LEN_EDT == 7 ~ "D",
                              LEN_EDT == 10 ~ " ",
                              LEN_EDT == 0 ~ NA_character_ )) %>% 
  
  mutate(
    ASTDT = as.Date(CMSTDTC_C),  # Convert to proper Date format
    AENDT = as.Date(CMENDTC_C),  # Convert to proper Date format
    ASTDY = ifelse(ASTDT < TRTSDT, 
                   ASTDT - TRTSDT,      # If ASTDT is before TRTSDT
                   ASTDT - TRTSDT + 1), # If ASTDT is on/after TRTSDT
    AENDY = ifelse(AENDT < TRTSDT,
                   AENDT - TRTSDT,      # If AENDT is before TRTSDT  
                   AENDT - TRTSDT + 1),  # If AENDT is on/after TRTSDT
    
      ONTRTFL= case_when(
        ASTDT >= TRTSDT ~ "Y",
        ASTDT < TRTSDT & CMENRF == "ONGOING" ~ "Y",
        TRUE ~ NA_character_
      )
    
  ) %>% 
  select(LENS, CMSTDTC, SDT,STIME,CMSTDTC_C,ASTDTF,CMENDTC,EDT,ETIME,CMENDTC_C,AENDTF, ASTDT, AENDT, ASTDY, AENDY,ONTRTFL)

    
  

