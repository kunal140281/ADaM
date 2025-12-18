# to create table 14.3.1.1 summary of treatment emergent adverse event


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
adae <- adae %>% filter(SAFFL =='Y') %>% 
  select(USUBJID,TRT01A, TRTEMFL, AESER, ASEV, SAFFL, AREL, ARELN,RELGR1,AACN, AESDTH)
adae_overall <- adae %>% mutate(TRT01A = 'Overall')
master_adae <-rbind(adae,adae_overall)

# denom count
denom_cnt <- master_adsl %>% 
  group_by(TRT01A) %>% 
  summarize(N = n())


# Calculation for teae
teae_count <- sqldf("select TRT01A ,
                    count(distinct USUBJID) as sub_count,
                    'TEAEs' as param, 
                    1 as param_ord,
                    count(usubjid) as evt_cnt
                    from master_adae
                    where TRTEMFL = 'Y'
                    group by TRT01A ")
  
  
# Serious TEAEs
  
teae_ser_count <- sqldf("select TRT01A ,
                    count(distinct USUBJID) as sub_count,
                    'Serious TEAEs' as param, 
                    2 as param_ord,
                    count(usubjid) as evt_cnt
                    from master_adae
                    where TRTEMFL = 'Y' and AESER = 'Y'
                    group by TRT01A ")

# Severe TEAEs

teae_sev_count <- sqldf("select TRT01A ,
                    count(distinct USUBJID) as sub_count,
                    'Severe TEAEs' as param, 
                    3 as param_ord,
                    count(usubjid) as evt_cnt
                    from master_adae
                    where TRTEMFL = 'Y' and ASEV = 'Severe'
                    group by TRT01A ")


# Related TEAEs

teae_rel_count <- sqldf("select TRT01A ,
                    count(distinct USUBJID) as sub_count,
                    'Related TEAEs' as param, 
                    4 as param_ord,
                    count(usubjid) as evt_cnt
                    from master_adae
                    where TRTEMFL = 'Y' and RELGR1 != 'Not Related'
                    group by TRT01A ")

#  TEAEs LEADING TO WITHDRAWL

teae_wthd_count <- sqldf("select TRT01A ,
                    count(distinct USUBJID) as sub_count,
                    'TEAEs Leading to Withdrawl' as param, 
                    5 as param_ord,
                    count(usubjid) as evt_cnt
                    from master_adae
                    where TRTEMFL = 'Y' and AACN == 'Drug Withdrawn'
                    group by TRT01A ")

                 

#  TEAEs LEADING TO DEATH

teae_death_count <- sqldf("select TRT01A ,
                    count(distinct USUBJID) as sub_count,
                    'TEAEs Leading to Death' as param, 
                    6 as param_ord,
                    count(usubjid) as evt_cnt
                    from master_adae
                    where TRTEMFL = 'Y' and AESDTH == 'Y'
                    group by TRT01A ")
# setting all teas

fin <- rbind(teae_count,teae_ser_count,teae_sev_count, teae_rel_count, teae_wthd_count,teae_death_count)
final <- left_join(denom_cnt,fin, by="TRT01A" ) %>% 
  mutate(pct = (sub_count/N) * 100,
         pct1 = round(pct,2),
         total = paste0(sub_count,"(",pct1,")","[",evt_cnt,"]"),
         stat ="n (%) E") %>% select(param, stat, total,param_ord,TRT01A)

# transpose to get the final dataset as per mock shell
final1 <- final %>% pivot_wider(id_cols = c("param", "param_ord", "stat"),
                                names_from = "TRT01A",
                                values_from = "total") %>% 
  select(param, param_ord, stat, Tquine, Placebo, Overall)
  
dummy <-tribble(
  ~param, ~param_ord, ~stat, ~Tquine, ~Placebo,  ~Overall,  
  "TEAEs Leading to Death", 6,"n (%) E", "0(0.00)[0]","0(0.00)[0]","0(0.00)[0]")
  test <- rbind(final1,dummy) %>% mutate(Placebo = replace_na(Placebo,"0(0.00)[0]" )) %>% select(-param_ord)
  
  total <-as.vector(denom_cnt$N)[1]
  drug <- as.vector(denom_cnt$N)[2]
  placebo <- as.vector(denom_cnt$N)[3]
  
  foot_notes <- paste0("TEAE: treatment emergent adverse event.
E: Number of events; N: Number of subjects dosed with each treatment (or any treatment as applicable); n: Number of subjects with characteristic; %: Calculated using the number of subjects treated with each treatment (or any treatment as applicable) as the denominator (n/N*100).
" )
  test %>% 
    rtf_page(orientation = "landscape") %>% 
    
  
    rtf_title (title = "Section 14.3.1: Adverse Events",
               subtitle = "Table 14.3.1.1 Summary of Treatment Emergent Adverse Events (Safety Population)",
               text_justification = c("l","l"),
               text_font = c(3,3),
               # text_font_size = 10,
               text_format = c("b","" ) ) %>%
    
    rtf_colheader ( paste0("Characteristic |Statistics| Test\n (N=", drug,")| Reference\n (N=", placebo,")| Any Treatment\n (N=", total,")"),
                    
                    col_rel_width = c(20,10,10,10,10),
                    text_justification = c("l","c","c","c","c") ) %>% 
    rtf_body(col_rel_width = c(20,10,10,10,10),
             text_justification = c("l","c","c","c","c"))%>% 
               
    
    rtf_footnote(
      foot_notes,                       
      text_justification  = "l"              
    ) %>%
    rtf_encode() %>% 
    write_rtf("Table 14.3.1.1 Summary of TEAE(Safety Pop).rtf")     
  
  
  

