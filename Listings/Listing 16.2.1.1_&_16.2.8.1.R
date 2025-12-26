#Creating :- Listing 16.2.1.1 Withdrawals from the Study
rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(stringr)
path <- "C:\\GOT\\Rprogramming\\"
output <- "C:\\GOT\\Rprogramming\\OUTPUT\\"
adsl <- read_sas(paste0(path,"adsl.sas7bdat"))


adsl <- read_sas(paste0(path, "adsl.sas7bdat"))
withdrawal_list <- adsl %>%
  filter(EOSSTT=="Discontinued") %>%
  mutate(DCSREAS=str_to_sentence(DCSREAS)) %>%
  select(USUBJID,RFPENDTC, DCSREAS,TRTEDT)
#output location
#output <- "D:/R Training/Learning Materials/Data creation/Projects/Output/"
withdrawal_list %>%
  rtf_page(orientation = "landscape") %>%
  rtf_title(title = "Section 16.2.1: Disposition",
            subtitle = "Listing 16.2.1.1 Withdrawals from the Study",
            text_justification = c("l","l"),
            text_format = c("b", "i")) %>%
  rtf_colheader("Subject Number | Date of Withdrawal | Primary Reason for Withdrawal |
Date of Last Drug Administration",
                col_rel_width = c(11,6,6,6),
                text_justification = c('l',"c","c","c")) %>%
  rtf_body(col_rel_width = c(11,6,6,6),
           text_justification = c('l',"c","c","c")) %>%
  # rtf_footnote("BMI: Body Mass Index.") %>%
  rtf_encode() %>%
  write_rtf(paste0(output, "Listing 16.2.1.1.rtf"))
#Creating :- Listing 16.2.8.1 Biochemistry
rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(stringr)
path <- "C:\\GOT\\Rprogramming\\"
adsl <- read_sas(paste0(path,"adsl.sas7bdat"))
output <- "C:\\GOT\\Rprogramming\\OUTPUT\\"

adlb <- read_sas(paste0(path, "adlb.sas7bdat"))
'%notin%' <-Negate('%in%')
bio_chem <- adlb %>%
  filter(PARCAT1=="CHEMISTRY") %>%
  # 1 way for filter high and low
  # filter(ANRIND %in% c("HIGH", "LOW")) %>%
  #2nd way of filtering
  filter(ANRIND %notin% c("NORMAL", "")) %>%
  mutate(L_H=paste0(as.character(round(ANRLO,2)),"-", as.character(round(ANRHI,2))),
         CHG1 = as.character(CHG),
         ANRIND = str_to_sentence(ANRIND)) %>%
  arrange(USUBJID,PARAMN, PARAM, AVISITN, AVISIT) %>%
  select(USUBJID,PARAM,AVISIT, ADTM, AVALC, CHG1, L_H, ANRIND)
#output location
#output <- "D:/R Training/Learning Materials/Data creation/Projects/Output/"
bio_chem %>%
  rtf_page(orientation = "landscape") %>%
  rtf_title(title = "Listing 16.2.8.1 Biochemistry",
            text_justification = "l",
            text_format = "b") %>%
  rtf_colheader("Subject Number | Test (Unit) | Time Point | DateTime of Measurement | Result |
Change from Baseline | Out of Range | Investigator Interpretation ",
                col_rel_width = c(4,4,4,4,4,4,4,4)) %>%
  rtf_body(col_rel_width = c(4,4,4,4,4,4,4,4)) %>%
  rtf_footnote("H: high; L: low; CS: clinically significant; NCS: not clinically significant.
Baseline is defined as the last observation prior to first drug administration.") %>%
  rtf_encode() %>%
  write_rtf(paste0(output, "Listing 16.2.8.1.rtf"))
