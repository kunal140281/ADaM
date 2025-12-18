# to create Table 14.3.1.2 Treatment Emergent Adverse Events by Treatment, System Organ Class and Preferred Term (Safety Population)


getwd()
rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(stringr)
library(lubridate)
library(tidyr)
library(sqldf)



path <- "C:\\GOT\\Rprogramming\\"
adsl <- read_sas(paste0(path,"adsl.sas7bdat"))
adae <- read_sas(paste0(path,"adae.sas7bdat"))
OUTPUT <- "C:\\GOT\\Rprogramming\\OUTPUT"

# calling adsl dataset
adsl <- adsl %>% filter(SAFFL =='Y')
adsl_overall <- adsl %>% mutate(TRT01A = 'Overall')
master_adsl <-rbind(adsl,adsl_overall)

# calling adae dataset
adae1 <- adae %>% filter(SAFFL =='Y') %>% 
  select(USUBJID,TRT01A, TRTEMFL,AETERM,AEDECOD, AEBODSYS )
adae_overall <- adae1 %>% mutate(TRT01A = 'Overall')
master_adae <-rbind(adae1,adae_overall) %>% filter(AEBODSYS != '')

# denom count
denom_cnt <- master_adsl %>% 
  group_by(TRT01A) %>% 
  summarize(N = n())

# SOC count
soc_count <- sqldf("select TRT01A,AEBODSYS,
                   count(distinct USUBJID) as sub_count,
                   1 as soc_ord,
                   '' as AEDECOD,
                   count(usubjid) as evt_cnt
                   from master_adae
                   where TRTEMFL = 'Y'
                   group by TRT01A, AEBODSYS ")
                   
# PT count
pt_count <- sqldf("select TRT01A,AEBODSYS,AEDECOD,
                   count(distinct USUBJID) as sub_count,
                   2 as soc_ord,
                   count(usubjid) as evt_cnt
                   from master_adae
                   where TRTEMFL = 'Y'
                   group by TRT01A, AEBODSYS,AEDECOD ")


# setting both dataset
 test <- rbind(soc_count,pt_count)                  
 final <- left_join(denom_cnt,test, by="TRT01A" ) %>% 
   mutate(pct = (sub_count/N) * 100,
          pct1 = round(pct,2),
          total = paste0(sub_count,"(",pct1,")","[",evt_cnt,"]"),
          stat ="n (%) E") %>% select(AEBODSYS,AEDECOD, stat, total,soc_ord,TRT01A)
 
last<- final %>% 
  pivot_wider(id_cols = c("AEBODSYS","soc_ord","AEDECOD"),
              names_from = "TRT01A",
              values_from = "total") %>% 
  arrange(AEBODSYS,AEDECOD) # sort according th AEBODSYS AEDECOD

  last1 <- last %>%  mutate(SOCPT = case_when(AEDECOD == "" ~ AEBODSYS,
                                              AEDECOD != "" ~ paste("    ",AEDECOD)),
                            Stat = "n (%) E")
  df <- last1 %>%
    replace_na(list(
     Placebo = "0(0.00)[0]",
      Tquine = "0(0.00)[0]"
    )) %>% 
    select(SOCPT, Stat, Tquine, Placebo, Overall )
  
  # header count
  total <-as.vector(denom_cnt$N)[1]
  drug <- as.vector(denom_cnt$N)[2]
  placebo <- as.vector(denom_cnt$N)[3]
  
  foot_notes <- paste0("E: Number of events; N: Number of subjects dosed with each treatment (or any treatment as applicable); n: Number of subjects with adverse event; %: Calculated using the number of subjects treated with each treatment (or any treatment as applicable) as the denominator (n/N*100).
All adverse events are coded using MedDRA version 25.2" )
  df %>% 
    rtf_page(orientation = "landscape") %>% 
    
    
    rtf_title (title = "Section 14.3.1: Adverse Events",
               subtitle = "Table 14.3.1.2 Treatment Emergent Adverse Events by Treatment, System Organ Class and Preferred Term (Safety Population)",
               text_justification = c("l","l"),
               text_font = c(3,3),
               # text_font_size = 10,
               text_format = c("b","" ) ) %>%
    
    rtf_colheader ( paste0("System Organ Class\n Preferred Term |Statistics| Test\n (N=", drug,")| Reference\n (N=", placebo,")| Any Treatment\n (N=", total,")"),
                    
                    col_rel_width = c(20,10,10,10,10),
                    text_justification = c("l","c","c","c","c") ) %>% 
    rtf_body(col_rel_width = c(20,10,10,10,10),
             text_justification = c("l","c","c","c","c"))%>% 
    
    
    rtf_footnote(
      foot_notes,                       
      text_justification  = "l"              
    ) %>%
    rtf_encode() %>% 
    write_rtf("Table 14.3.1.2 TEAE by trt, soc,pt(Safety Pop).rtf")     
  
  
  
  
  
