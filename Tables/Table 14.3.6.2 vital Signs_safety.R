# to create Table 14.3.6.2 Vital Signs – Change from Baseline (Safety Population)


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
advs <- read_sas(paste0(path,"advs.sas7bdat"))
OUTPUT <- "C:\\GOT\\Rprogramming\\OUTPUT"

# calling adsl dataset
adsl <- adsl %>% filter(SAFFL =='Y')
adsl_overall <- adsl %>% mutate(TRT01A = 'Overall')
master_adsl <-rbind(adsl,adsl_overall)

# calling adae dataset
advs1 <- advs %>% filter(SAFFL =='Y') %>% 
  select(USUBJID,TRT01A, PARAM, PARAMCD, PARAMN, AVISITN, AVISIT, CHG )
advs_overall <- advs1 %>% mutate(TRT01A = 'Overall')
master_advs <-rbind(advs1,advs_overall) %>% filter(CHG != '') %>% arrange(PARAMN, PARAM, PARAMCD,AVISITN, AVISIT)

# denom count
denom_cnt <- master_adsl %>% 
  group_by(TRT01A) %>% 
  summarize(N = n())



VS_des <- master_advs %>% group_by(TRT01A,PARAMN,PARAM,PARAMCD,AVISITN, AVISIT) %>% 
  summarise(n = as.character(n()),
            mean =as.character(round(mean(CHG),1)),
            sd =as.character(round(sd(CHG),2)),
            median =as.character(round(median(CHG),1)),
            min =as.character(round(min(CHG),0)),
            max =as.character(round(max(CHG),0)) )

# transpose to get all descriptive statistics in same column
Vs1 <- VS_des%>% 
  pivot_longer(cols = c("n","mean","sd","median", "min","max"),
               names_to ="Stat",
               values_to = "val"
              )
# transpose to get to the statistics under differnet treatment

vs <- Vs1 %>% pivot_wider(id_cols =c("PARAM", "AVISIT","Stat"),
                          names_from = TRT01A,
                          values_from = val) %>% 
  select(PARAM, AVISIT, Stat,Tafenoquine, Placebo, Overall)
 
                          
# header count
total <-as.vector(denom_cnt$N)[1]
drug <- as.vector(denom_cnt$N)[2]
placebo <- as.vector(denom_cnt$N)[3]

foot_notes <- paste0("Vital sign Change from baseline" )
vs %>% 
  rtf_page(orientation = "landscape") %>% 
  
  
  rtf_title (title = "Section 14.3.6: Other Safety Assessments (Alternative Presentation)",
             subtitle = "Table 14.3.6.2 Vital Signs – Change from Baseline (Safety Population)",
             text_justification = c("l","l"),
             text_font = c(3,3),
             # text_font_size = 10,
             text_format = c("b","" ) ) %>%
  
  rtf_colheader ( paste0("Test (Unit)| Time Point |Statistics| Test\n (N=", drug,")| Reference\n (N=", placebo,")| Any Treatment\n (N=", total,")"),
                  
                  col_rel_width = c(20,20,10,10,10,10),
                  text_justification = c("l","l","l","c","c","c") ) %>% 
  rtf_body(col_rel_width = c(20,20,10,10,10,10),
           text_justification = c("l","l","l","c","c","c"),
           group_by = c("PARAM","AVISIT"))%>% 
  
  
  rtf_footnote(
    foot_notes,                       
    text_justification  = "l"              
  ) %>%
  rtf_encode() %>% 
  write_rtf("Table 14.3.6.2 Vital Signs COB(Safety Pop).rtf")     






                          
                          
                          
