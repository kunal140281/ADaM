rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(reshape2)
library(stringr)
library(lubridate)
library(tidyr)
library(sqldf)
library(rtables)
adsl<- ex_adsl %>% filter(SAFFL == 'Y')
colnames(adlb)
adlb1 <- as.data.frame(ex_adlb, stringsAsFactors = FALSE)
adlb <- adlb1%>% filter(SAFFL == 'Y', LBCAT =='CHEMISTRY', AVISITN != -1) %>%
  
  mutate( ANRIND1 = case_when(ANRIND =="LOW" ~ "Abnormal Low",
                              ANRIND =="HIGH" ~ "Abnormal High",
                              ANRIND =="NORMAL" ~ "Normal"),
          vis = paste0("vis",AVISITN) ) %>% 
  select(USUBJID, PARAM,LBCAT,SAFFL,ANRIND1,AVISITN, AVISIT,ABLFL,vis) %>% 
 pivot_wider(id_cols = c("USUBJID", "PARAM"),
                names_from = vis,
                values_from = ANRIND1)
 test <- adlb %>% select(USUBJID, PARAM, `vis0`,`vis1`) %>% 
   mutate(a_chg_01 = paste0(vis0,"->",vis1))
              
  base_cnt <-sqldf("select PARAM,vis0,
                   count(distinct USUBJID) as base_cnt
                   from test
                   group by PARAM,vis0") 
  
  pbase_cnt <-sqldf("select PARAM,a_chg_01,vis0,
                   count(distinct USUBJID) as pbase_cnt
                   from test
                   group by PARAM,a_chg_01")
  
  pbase_cnt1 <-sqldf("select PARAM,vis0,vis1,
                   count(distinct USUBJID) as pbase_cnt1
                   from test
                   group by PARAM,vis0,vis1")  
 fin <- dplyr::left_join(base_cnt,pbase_cnt, by = c("PARAM","vis0")) %>% 
 mutate(pct = (pbase_cnt/base_cnt) * 100,
           pct1 = round(pct,2),
           total = paste0(base_cnt,"(",pct1,")"))
 
 df <- fin %>%
   separate(a_chg_01, into = c("baseline", "postbaseline"), sep = "->", remove = FALSE) %>% 
   select(-baseline, -a_chg_01, -pct1, -pct)
 
 # transpose post baseline values
 df1 <- df %>% pivot_wider(id_cols = c("PARAM", "vis0", "base_cnt"),
                           names_from = postbaseline,
                           values_from = total) %>% 
   mutate(ord = case_when( vis0 == "Abnormal Low" ~ 1,
                           vis0 == "Normal" ~ 2,
                           vis0 == "Abnormal High" ~ 3,
                           vis0 == "Not Available" ~ 4)) %>% 
   arrange(PARAM, ord) %>% select("PARAM", "vis0","base_cnt", "Abnormal Low", "Normal", "Abnormal High" )
   

 foot_notes <- paste0("%: calculated using the number of subjects (N=XX) in the safety population as the denominator.

" )
df1 %>% 
   rtf_page(orientation = "landscape") %>% 
   
   
   rtf_title (title = "Section 14.3.5: Laboratory Assessments",
              subtitle = "Table 14.3.5.3 Biochemistry Shift Table (Safety Population)",
              text_justification = c("l","l"),
              text_font = c(3,3),
              # text_font_size = 10,
              text_format = c("b","" ) ) %>%
   
   rtf_colheader ( colheader = "Test (Unit)| Baseline|Baseline \n count |Abnormal Low \n n (%)| Normal\n n (%)| Abnormal High \n n (%)",
                   
                   col_rel_width = c(25,10,10,10,10,10),
                   text_justification = c("l","l","c","c","c","c") ) %>% 
   rtf_body(col_rel_width = c(25,10,10,10,10,10),
            text_justification = c("l","l","c","c","c","c"),
            group_by = "PARAM")%>% 
   
   
   rtf_footnote(
     foot_notes,                       
     text_justification  = "l"              
   ) %>%
   rtf_encode() %>% 
   write_rtf("Table 14.3.5.3 Biochemistry shift table (Safety Pop).rtf")     
 
 
