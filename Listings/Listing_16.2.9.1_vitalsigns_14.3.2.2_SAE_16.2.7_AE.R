#Creating :- Listing 16.2.9.1 Vital Signs
rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(stringr)
path <- "C:\\GOT\\Rprogramming\\"
output <- "C:\\GOT\\Rprogramming\\OUTPUT\\"
advs <- read_sas(paste0(path, "advs.sas7bdat"))
'%notin%' <-Negate('%in%')
vital_listing <- advs %>%
  # 1 way for filter high and low
  # filter(AVALCAT1 %in% c("Abnormal and Not Clinically Significant", "Abnormal and Clinically Significant")) %>%
  #2nd way of filtering
  filter(AVALCAT1 %notin% c("Normal range", "")) %>%
  mutate(
    #L_H=paste0(as.character(round(ANRLO,2)),"-", as.character(round(ANRHI,2))),
    CHG1 = as.character(round(CHG,2)),
    AVALCAT1 = str_to_sentence(AVALCAT1),
    AVALC1 = as.character(round(AVAL,2))) %>%
  arrange(USUBJID,PARAMN, PARAM, AVISITN, AVISIT) %>%
  select(USUBJID,PARAM,AVISIT, ADTM, AVALC1, CHG1, AVALCAT1)
#output location
#output <- "D:/R Training/Learning Materials/Data creation/Projects/Output/"
vital_listing %>%
  rtf_page(orientation = "landscape") %>%
  rtf_title(title = "Listing 16.2.9.1 Vital Signs",
            text_justification = "l",
            text_format = "b") %>%
  rtf_colheader("Subject Number | Test (Unit) | Time Point | Date /Time of Measurement | Result |
Change from Baseline |Investigator Interpretation ",
                col_rel_width = c(4,4,4,4,4,4,4)) %>%
  rtf_body(col_rel_width = c(4,4,4,4,4,4,4)) %>%
  rtf_footnote("H: high; L: low; CS: clinically significant; NCS: not clinically significant; SBP: systolic blood pressure; DBP: diastolic blood pressure.
Baseline is defined as the last observation prior to first drug administration.") %>%
  rtf_encode() %>%
  write_rtf(paste0(output, "Listing 16.2.9.1.rtf"))


#Creating :- Listing 14.3.2.2 Serious Adverse Events
rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(stringr)
path <- "C:\\GOT\\Rprogramming\\"
output <- "C:\\GOT\\Rprogramming\\OUTPUT\\"
adae <- read_sas(paste0(path, "adae.sas7bdat"))
serious_ae_list <- adae %>%
  filter(AESER=="Y") %>%
  mutate(ONG = case_when(AEENDTC != "" ~ "No",
                         TRUE ~ "Yes")) %>%
  select(USUBJID,AESEQ, AETERM, AEDECOD ,AEBODSYS, ASTDTM, AENDTM,ONG)
#output location
#output <- "D:/R Training/Learning Materials/Data creation/Projects/Output/"
serious_ae_list %>%
  rtf_page(orientation = "landscape") %>%
  rtf_title(title = "Listing 14.3.2.2 Serious Adverse Events",
            text_justification = "l",
            text_format = "b") %>%
  rtf_colheader("Subject Number | AE Number | AE Description |MedDRA Preferred Term | MedDRA System Organ Class |
Start Date/Time | End Date/Time | Ongoing ",
                col_rel_width = c(4,4,4,4,4,4,4,4)) %>%
  rtf_body(col_rel_width = c(4,4,4,4,4,4,4,4)) %>%
  rtf_footnote("AE: adverse event; MedDRA: Medical Dictionary for Regulatory Activities; SAE: serious adverse event; TEAE: treatment emergent adverse event.
All adverse events are coded using MedDRA version 25.1.") %>%
  rtf_encode() %>%
  write_rtf(paste0(output, "Listing 14.3.2.2.rtf"))


#Creating:- Listing 16.2.7 Adverse Events
rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(stringr)
#path <- "D:/R Training/Learning Materials/Data creation/Projects/ADAM/"
path <- "C:\\GOT\\Rprogramming\\"
output <- "C:\\GOT\\Rprogramming\\OUTPUT\\"
adae <- read_sas(paste0(path, "adae.sas7bdat"))
adverse_event_list <- adae %>%
  # filter(AESER=="Y") %>%
  mutate(ONG = case_when(AEENDTC != "" ~ "No",
                         TRUE ~ "Yes")) %>%
  select(USUBJID,AESEQ, AETERM, AEDECOD ,AEBODSYS, ASTDTM, AENDTM,ONG)
#output location
#output <- "D:/R Training/Learning Materials/Data creation/Projects/Output/"
adverse_event_list %>%
  rtf_page(orientation = "landscape") %>%
  rtf_title(title = "Listing 16.2.7 Adverse Events",
            text_justification = "l",
            text_format = "b") %>%
  rtf_colheader("Subject Number | AE Number | AE Description |MedDRA Preferred Term | MedDRA System Organ Class |
Start Date/Time | End Date/Time | Ongoing ",
                col_rel_width = c(4,4,4,4,4,4,4,4)) %>%
  rtf_body(col_rel_width = c(4,4,4,4,4,4,4,4)) %>%
  rtf_footnote("AE: adverse event; MedDRA: Medical Dictionary for Regulatory Activities; SAE: serious adverse event; TEAE: treatment emergent adverse event.
All adverse events are coded using MedDRA version 25.1.") %>%
  rtf_encode() %>%
  write_rtf(paste0(output, "Listing 16.2.7.rtf"))