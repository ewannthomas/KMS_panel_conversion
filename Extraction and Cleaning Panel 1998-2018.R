
library(tidyverse)


# Reading in Mothers ------------------------------------------------------

Iden<-readxl::read_excel("./1998-2018 panel/B1 KMS 2018 Main Module.xlsx")

Individual<-readxl::read_excel("./1998-2018 panel/B2 KMS 2018 Individual.xlsx")

Health<-readxl::read_excel("./1998-2018 panel/B3 KMS 2018 Health.xlsx")

HH_Details<-readxl::read_excel("./1998-2018 panel/B4 Household Details.xlsx")

RM<-readxl::read_excel("./1998-2018 panel/B5 ReturnMigration.xlsx")

OSMC<-readxl::read_excel("./1998-2018 panel/B5A Oversease Migration Cost.xlsx")

A_F<-readxl::read_excel("./1998-2018 panel/B6 A to F.xlsx")

Migration<-readxl::read_excel("./1998-2018 panel/B7 Migration.xlsx")

Remittances<-readxl::read_excel("./1998-2018 panel/B8 Remittances.xlsx")

Emigration_cost<-readxl::read_excel("./1998-2018 panel/B9 Emigration Cost.xlsx")

Student_Migration<-readxl::read_excel("./1998-2018 panel/B10 Student Migration.xlsx")

Women_Emigration<-readxl::read_excel("./1998-2018 panel/B11 Women Emigration.xlsx")

Gender<-readxl::read_excel("./1998-2018 panel/B12 to 16 Gender Module.xlsx")

RP_History<-readxl::read_excel("./1998-2018 panel/B15 RP History.xlsx")

A_D<-readxl::read_excel("./1998-2018 panel/B17 A to D.xlsx")


p_2003<-Iden %>% filter(Panel_Year2003==1) %>% select(ScheduleNo, Panel_SLNo2003)
p_2008<-Iden %>% filter(Panel_Year2008==1) %>% select(ScheduleNo, Panel_SLNo2008)
p_2013<-Iden %>% filter(Panel_Year2013==1) %>% select(ScheduleNo, Panel_SLNo2013)

# Reading in 1998 Mother --------------------------------------------------


Iden<-readxl::read_excel("./1998-2018 panel/B1 KMS 2018 Main Module.xlsx")

p_1998<-Iden %>% filter(Panel_Year1998==1) %>% select(ScheduleNo, Panel_SLNo1998) %>% 
  mutate(Panel_SLNo1998 = as.double(Panel_SLNo1998)) %>% 
  filter(Panel_SLNo1998!=0, !is.na(Panel_SLNo1998)) %>% 
  group_by(Panel_SLNo1998) %>% mutate(dups=max(row_number())) %>% 
  filter(dups<2) %>% select(-dups)# filtering out serial of panel HH in 1998

HH_1998<-readxl::read_excel("./1998/HOUSEHOLDS.xlsx") %>% 
  mutate(uniquenumber=as.double(uniquenumber)) # main HH iden for 1998

HH_1998<-left_join(p_1998, HH_1998, by=c("Panel_SLNo1998" = "uniquenumber"))#filtering 1998 panel HH using 2018 info

eeptools::isid(HH_1998, vars = c("Panel_SLNo1998")) 





Indiv_1998<-readxl::read_excel("./1998/INDIVIDUALS.xlsx")
eeptools::isid(Indiv_1998, vars = c("QNO", "A1"))


Emig_1998<-readxl::read_excel("./1998/EMIGRANTS.xlsx") %>% 
  mutate(ID=as.double(ID))
eeptools::isid(Emig_1998, vars = c("QNO", "SNO"))


Outmig_1998<-readxl::read_excel("./1998/OUTMIGRANTS.xlsx")

Returnemig_1998<-readxl::read_excel("./1998/RETURNEMIGRANTS.xlsx")

Returnoutmig_1998<-readxl::read_excel("./1998/RETURNOUTMIGRANTS.xlsx")

Panel_1998<-left_join(HH_1998, Emig_1998) %>% 
  left_join(., Outmig_1998) %>% 
  left_join(.,Returnemig_1998) %>% 
  left_join(.,Returnoutmig_1998)

rm(P_1998)

# Reading in 2003 Mother --------------------------------------------------

Iden<-readxl::read_excel("./1998-2018 panel/B1 KMS 2018 Main Module.xlsx")

p_2003<-Iden %>% filter(Panel_Year2003==1) %>% select(ScheduleNo, Panel_SLNo2003) %>% 
  mutate(Panel_SLNo2003 = as.double(Panel_SLNo2003)) %>% 
  filter(Panel_SLNo2003!=0, !is.na(Panel_SLNo2003)) %>% 
  group_by(Panel_SLNo2003) %>% mutate(dups=max(row_number()))
  filter(dups<2) %>% select(-dups) # filtering out serial of panel HH in 2003


HH_2003<-readxl::read_excel("./2003/MAINHOUSEHOLDS.xlsx")  
eeptools::isid(HH_2003, vars = c("slno"))

HH_1998<-left_join(p_1998, HH_1998, by=c("Panel_SLNo1998" = "uniquenumber"))#filtering 1998 panel HH using 2018 info

eeptools::isid(HH_1998, vars = c("Panel_SLNo1998")) 



Indiv_2003<-readxl::read_excel("./2003/BLOCK2INDIVIDUALS.xlsx")

REM_ROM_2003<-readxl::read_excel("./2003/BLOCK3REMROM.xlsx")

EMI_OMI_2003<-readxl::read_excel("./2003/BLOCK4EMIOMI.xlsx")


# Reading in 2008 Mother --------------------------------------------------

Iden<-readxl::read_excel("./1998-2018 panel/B1 KMS 2018 Main Module.xlsx")

p_1998<-Iden %>% filter(Panel_Year1998==1) %>% select(ScheduleNo, Panel_SLNo1998) %>% 
  mutate(Panel_SLNo1998 = as.double(Panel_SLNo1998)) %>% 
  filter(Panel_SLNo1998!=0, !is.na(Panel_SLNo1998)) # filtering out serial of panel HH in 1998

HH_2013<-readxl::read_excel("./2013/.xlsx")


# Reading in 2013 Mother --------------------------------------------------

Iden<-readxl::read_excel("./1998-2018 panel/B1 KMS 2018 Main Module.xlsx")

p_2013<-Iden %>% filter(Panel_Year2013==1) %>% select(ScheduleNo, Panel_SLNo2013) %>% 
  mutate(Panel_SLNo2013 = as.double(Panel_SLNo2013)) %>% 
  filter(Panel_SLNo2013!=0, !is.na(Panel_SLNo2013)) # filtering out serial of panel HH in 2013

HH_2013_1<-readxl::read_excel("./2013/HOUSEHOLD Part 1 2013.xls")

HH_2013_2<-readxl::read_excel("./2013/Household Part 2 2013.xls")









# blah --------------------------------------------------------------------


p_1998<-Iden %>% filter(Panel_Year1998==1) %>% select(ScheduleNo, Panel_SLNo1998) %>% 
  mutate(Panel_SLNo1998 = as.double(Panel_SLNo1998)) %>% 
  filter(Panel_SLNo1998!=0, !is.na(Panel_SLNo1998)) %>% 
  group_by(Panel_SLNo1998) %>% mutate(dups=max(row_number())) %>% 
  filter(dups<2) %>% select(-dups)# filtering out serial of panel HH in 1998


p_2003<-Iden %>% filter(Panel_Year2003==1) %>% select(ScheduleNo, Panel_SLNo2003) %>% 
  mutate(Panel_SLNo2003 = as.double(Panel_SLNo2003)) %>% 
  filter(Panel_SLNo2003!=0, !is.na(Panel_SLNo2003)) %>% 
  group_by(Panel_SLNo2003) %>% mutate(dups=max(row_number())) %>% 
  filter(dups<2) %>% select(-dups) # filtering out serial of panel HH in 2003


p_2008<-Iden %>% filter(Panel_Year2008==1) %>% select(ScheduleNo, Panel_SLNo2008) %>% 
  filter(Panel_SLNo2008!=0, !is.na(Panel_SLNo2008))  %>% 
  group_by(Panel_SLNo2008) %>% mutate(dups=max(row_number())) %>% 
  filter(dups<2) %>% select(-dups)# filtering out serial of panel HH in 2008


p_2013<-Iden %>% filter(Panel_Year2013==1) %>% select(ScheduleNo, Panel_SLNo2013) %>% 
  mutate(Panel_SLNo2013 = as.double(Panel_SLNo2013)) %>% 
  filter(Panel_SLNo2013!=0, !is.na(Panel_SLNo2013))  %>% 
  group_by(Panel_SLNo2013) %>% mutate(dups=max(row_number())) %>% 
  filter(dups<2) %>% select(-dups)# filtering out serial of panel HH in 2013

