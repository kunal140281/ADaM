#Creating :- Listing 16.2.1.1 Withdrawals from the Study
getwd()
rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(stringr)
library(lubridate)

path <- "C:\\GOT\\Rprogramming\\"
adsl <- read_sas(paste0(path,"adsl.sas7bdat"))
OUTPUT <- "C:\\GOT\\Rprogramming\\OUTPUT"
adsl1 <- adsl %>% select(USUBJID,RANDFL, SAFFL, ITTFL, PPROTFL)
# FORMAT CREATION
format_FL <- function(fmt_var) {
  case_when(
    fmt_var == "Y" ~ "Yes",
    fmt_var == "N" ~ "No",
    TRUE ~ NA_character_
  )
}

final <- adsl1 %>% mutate(RANDFL = format_FL(RANDFL),
                          SAFFL = format_FL(SAFFL),
                          ITTFL = format_FL(ITTFL),
                          PPROTFL = format_FL(PPROTFL))


# LISTING 16.2.3 INFORMED CONSCENT DATE
IN <- adsl %>% select(USUBJID, RFICDTC) %>% 
  mutate(RFICDTC1 = format(as.Date(RFICDTC), "%d-%m-%Y"),
         RFICDTC2 =format(ymd(RFICDTC), "%d%B%Y"),
         RFICDTC3 =format(ymd(RFICDTC), "%d%b%Y"))

# listing 16.2.4

dem <-  adsl %>% select(USUBJID,AGE, SEX, ETHNIC, RACE, ends_with("SI"))
# CHARACTER FORMAT
format_SX <- function(fmt_var) {
  case_when(
    fmt_var == "M" ~ "MALE",
    fmt_var == "F" ~ "FEMALE",
    TRUE ~ NA_character_
  )
}

dem <-  adsl %>% mutate(GENDER = format_SX(SEX),
                        ETHNIC = str_to_sentence(ETHNIC),
                        RACE = str_to_sentence(RACE),
                        BMI = as.character(round(BBMISI,2)),
                        HEIGHT = as.character(round(BHGHTSI,2)),
                        WEIGHT =  as.character(round(BWGHTSI,2))) %>% 
  
  select(USUBJID,AGE, GENDER, ETHNIC, RACE, HEIGHT, WEIGHT,BMI) 
require(r2rtf)
dem %>% 
  rtf_page(orientation = "landscape",
           margin = c(1,0.25,1,0.25,0.25,3
                      )
           
           
           ) %>% 
  rtf_title (c("Section 16.2.4: Demographics and Baseline Characteristics","Listing 16.2.4.1 Subject Demographics"),
             text_justification = c('l','l'),
             text_font = 3,
             text_font_size = c(15,10),
             text_format = c("b","")
             
             ) %>%
  
  rtf_colheader("Subject Number | Age (years)|Gender|	Ethnic Origin |
                 Race|	Height (cm)	|Weight (kg)|	BMI (kg/m2)",
                col_rel_width = c(10,4,6,15,10,6,6,6)) %>% 
  rtf_body(col_rel_width = c(10,4,6,15,10,6,6,6)) %>% 
                                        
  rtf_footnote( ("BMI: Body Mass Index."),
             text_justification = 'l',
             text_font = 3,
             text_font_size = 10,
             #text_format = c("b","")
             
  ) %>% 
  
  # rtf_page_footer( ("BMI: Body Mass Index."),
  #                   text_justification = 'l',
  #                   text_font = 3,
  #                   text_font_size = 10,
  #                   #text_format = c("b","")
  #                   ) %>% 
  
       
        rtf_encode() %>% 
        write_rtf("Listing 16.2.4.1.rtf")

# Listing:16.2.1.1 Withdrawals from study


adsl2<-  adsl %>% filter(EOSSTT =="Discontinued") %>%
  mutate(DCSREAS = str_to_sentence(DCSREAS),
         RFPENDTC =format(ymd(RFPENDTC), "%d%b%Y"), 
        TRTEDT =format(ymd(as.character(TRTEDT)), "%d%b%Y")) %>% 
  
  
  select(USUBJID,RFPENDTC, DCSREAS,TRTEDT )



require(r2rtf)
foot_notes <- c("Programmer Note:" ,
                "Please concatenate any reasons specified in the CRF to the result being listed as shown in the shell." )
adsl2 %>% 
  rtf_page(orientation = "landscape") %>% 
           
           
  
  rtf_title (title = "Section 16.2.1: Disposition",
             subtitle = "Listing 16.2.1.1 Withdrawals from the Study",
             text_justification = c("l","l"),
              text_font = c(3,3),
             # text_font_size = 10,
             text_format = c("b","" ) ) %>%
             
    rtf_colheader("Subject Number | Date of Withdrawal| Primary Reason for Withdrawal| Date of Last Drug Administration ",
                
                col_rel_width = c(10,10,15,10),
                text_justification = c("l","c","c", "c","c") ) %>% 
  rtf_body(col_rel_width = c(10,10,15,10),
           text_justification = c("l","c","c", "c","c")) %>% 
 
   rtf_footnote(
    foot_notes,                       
    text_justification  = "l"              
  ) %>%
  rtf_encode() %>% 
  write_rtf("Listing 16.2.1.1.rtf")
  
  
  # rtf_footnote ("Programmer Note:" ,
  #               text_justification = "l",
  #               text_font = 3,
  #               text_font_size = 10,
  #             text_format = "b") %>% 
  # rtf_footnote ("Please concatenate any reasons specified in the CRF to the result being listed as shown in the shell." ,
  #               text_justification = "l",
  #               text_font = 3,
  #               text_font_size = 10,
  #               text_format = "b") %>% 
  # 
  # rtf_footnote(footnote = c("Section 16.2.4: Demographics and Baseline Characteristics",
  #                "Listing 16.2.4.1 Subject Demographics"),
  #              text_justification = c('l','l'),
  #              text_font = 3,
  #              text_font_size = c(10,10),
  #              text_format = c("b","b")) %>% 
               
               
      