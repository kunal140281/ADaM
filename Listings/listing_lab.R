#Creating :- Listing 16.2.1.1 Withdrawals from the Study
getwd()
rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(stringr)
library(lubridate)

path <- "C:\\GOT\\Rprogramming\\"
adlb <- read_sas(paste0(path,"adlb.sas7bdat"))
OUTPUT <- "C:\\GOT\\Rprogramming\\OUTPUT"
unique(adlb$PARCAT1)

bio_chem <- adlb %>% filter(PARCAT1 == "CHEMISTRY" & toupper(ANRIND) %in% c("LOW","HIGH"))%>% 
mutate(anlo = as.character(round(ANRLO,2)),
       anhi = as.character(round(ANRHI,2)),
       REF_RANGE = paste0(anlo,"-",anhi),
       INVST = if_else(is.na(ANRIND), "NCS", "CS"),
       CHG = as.character(CHG)
  ) %>% 
  arrange(USUBJID,PARAMN, PARAM, AVISITN, AVISIT, ADT) %>% 
   select(USUBJID, PARAM,REF_RANGE,AVISIT,  ADTM, AVALC,CHG, ANRIND,INVST )
unique(bio_chem$ANRIND)
require(r2rtf)
foot_notes <- c("H: high; L: low; CS: clinically significant; NCS: not clinically significant." ,
                "Baseline is defined as the last observation prior to first drug administration." )
bio_chem  %>% 
  rtf_page(orientation = "landscape") %>% 
  
  
  
  rtf_title (title = "Section 16.2.8: Laboratory Assessments",
             subtitle = "Listing 16.2.8.1 Biochemistry",
             text_justification = c("l","l"),
             text_font = c(3,3),
             # text_font_size = 10,
             text_format = c("b","" ) ) %>%
  
  rtf_colheader("SubjectNumber|Test(Unit)|Reference Range| Time Point | Date/Time of Measurement | Result|Change from Baseline| Out of Range|Investigator Interpretation",
                
                
                col_rel_width = c(15,20,8,15, 10, 6, 6, 5,5),
                 text_justification = c("l","l","c", "c","c","c", "c","c","c")
                ) %>% 
  rtf_body(col_rel_width = c(15,20,8,15, 10, 6, 6, 5,5),
           text_justification = c("l","l","c", "c","c","c", "c","c","c")
          ) %>% 
  rtf_page_footer(foot_notes,
                  text_justification  = "l" ) %>% 
  rtf_footnote(
    foot_notes,
    text_justification  = "l"              
  ) %>%
  rtf_encode() %>% 
  write_rtf("Listing 16.2.8.1.rtf")
# Listing 14.3.2.1 SERIOUS ADVERSE EVENT LEADING TO DEATH
adae <- read_sas(paste0(path,"adae.sas7bdat"))

colnames(adae)
str(adae)
unique(adae$AENDTM)
ae <- adae %>% 
   filter(AESER == "Y")%>%
  mutate(ONGO = case_when(AEENDTC != "" ~ "No",
                         TRUE ~ "Yes" ))%>% 
  select(USUBJID, AESEQ, AETERM, AEDECOD, AEBODSYS, ASTDTM, AENDTM, ONGO)