# to create Table 14.3.6.2 Vital Signs – Change from Baseline (Safety Population)


getwd()
rm(list=ls())
library(haven)
library(dplyr)
library(r2rtf)
library(reshape2)
library(stringr)
library(lubridate)
library(tidyr)
library(sqldf)



path <- "C:\\GOT\\Rprogramming\\"
adsl <- read_sas(paste0(path,"adsl.sas7bdat")) %>% filter(SAFFL =='Y')
adlb <- read_sas(paste0(path,"adlb.sas7bdat"))%>% filter(SAFFL =='Y')
OUTPUT <- "C:\\GOT\\Rprogramming\\OUTPUT"

# Creating negate function

"%notin%" <- Negate("%in%")

bio_chem <- adlb %>% filter(PARCAT1 == "CHEMISTRY" & toupper(ANRIND) %in% c("LOW","HIGH"))%>% 
  mutate(anlo = as.character(round(ANRLO,2)),
         anhi = as.character(round(ANRHI,2)),
         REF_RANGE = paste0(anlo,"-",anhi),
         ANRIND =str_to_sentence(ANRIND),
         BNRIND= str_to_sentence(BNRIND)) %>% 
  arrange(USUBJID,PARAMN, PARAM, AVISITN, AVISIT,BNRIND, ANRIND) %>% 
  select(USUBJID, PARAM,PARAMN,SAFFL,AVISITN,AVISIT,BNRIND,ANRIND )

chem <- bio_chem %>% mutate(BNRIND1 = case_when(BNRIND =="Low" ~ "Abnormal Low",
                                                BNRIND =="High" ~ "Abnormal High",
                                                BNRIND =="Normal" ~ "Normal",
                                                TRUE ~ "Not Available"),
                            ANRIND1 = case_when(ANRIND =="Low" ~ "Abnormal Low",
                                                ANRIND =="High" ~ "Abnormal High",
                                                ANRIND =="Normal" ~ "Normal",
                                                TRUE ~ "Not Available")
                            ) %>% filter (AVISIT != "UNSCHEDULED") %>% 
  select(USUBJID, PARAMN,PARAM,SAFFL, AVISITN, AVISIT, BNRIND1, ANRIND1)%>%
  pivot_wider(id_cols =c("USUBJID","PARAM", "PARAMN","SAFFL","BNRIND1"),
                        names_from = "AVISITN",
                        values_from = "ANRIND1")

chem_ <- chem %>% rename(a_vis_1 = 6, b_vis_15 = 7)
chem1 <- chem_ %>% mutate (chg_01 = paste0(BNRIND1,">",replace_na(a_vis_1,"Not Available")),
                         chg_15 = paste0(BNRIND1,">",replace_na(b_vis_15,"Not Available")))
                                         
 chem2 <- chem1 %>% pivot_longer(cols = c("chg_01", "chg_15"),
                                 names_to = "variable",
                                 values_to = "value")
 
 tab<- sqldf("select PARAM,PARAMN, SAFFL,BNRIND1,variable,value,
             count(distinct USUBJID) as sub_cnt
             from chem2
             group by PARAM, variable, value")
 
 # Big N count
 
 pop_cnt <- sqldf("select SAFFL,
                  count (distinct USUBJID)
                  as deno_cnt
                  from adsl")
 
pct <- dplyr::left_join(tab,pop_cnt, by ="SAFFL") %>% 
  mutate(pct = (sub_cnt/deno_cnt) * 100,
         pct1 = round(pct,2),
         total = paste0(sub_cnt,"(",pct1,")"))
   
df <- pct %>%
  separate(value, into = c("baseline", "postbaseline"), sep = ">", remove = FALSE) %>% 
  select(-BNRIND1) %>% filter(variable == "chg_01")
# transpose to get the shell
df1 <- df %>% pivot_wider(id_cols = c("PARAM" ,"baseline"),
                          names_from = "postbaseline",
                          values_from = "total")
                          
  
  
df2 <- df1 %>% mutate(ord = case_when( baseline == "Abnormal Low" ~ 1,
                                       baseline == "Normal" ~ 2,
                                       baseline == "Abnormal High" ~ 3,
                                       baseline == "Not Available" ~ 4),
                      Normal = "0(0.00)") %>% 
              arrange(PARAM, ord) %>% 
  select(1,2,5,7,3,4,6)

 df2[is.na(df2)] <- "0(0.00)" 
 
 
                                 
                                 
                                
   
   
   
  
                                                
                                                 
                            
                            