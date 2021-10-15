



Iden<-Iden %>% filter(ScheduleNo!=18443)

eeptools::isid(p_1998, vars=c("Panel_SLNo1998"))


eeptools::isid(Iden, vars=c("FSUNo", "SHHNO","Panel_No" , "Panel_Year1998", "Panel_Year2003", "Panel_SLNo2008", "Panel_SLNo2013"))




p_2013<-p_2013 %>% group_by(Panel_SLNo2013) %>% mutate(dups=max(row_number()))

p_1998 %>% select(slno, dups) %>% View()


blah<-HH_2003 %>% filter(yn98==1) %>% select(slno, qno1998) %>% 
  filter(qno1998!=0, !is.na(qno1998))

eeptools::isid(blah, vars=c("qno1998"))



# 2018 --------------------------------------------------------------------





#Gender<-readxl::read_excel("./1998-2018 panel/B12 to 16 Gender Module.xlsx")%>%  
#filter(ScheduleNo!=18443)

#eeptools::isid(Gender, vars=c("ScheduleNo", "B12Q2B2SlNo"))




#RP_History<-readxl::read_excel("./1998-2018 panel/B15 RP History.xlsx")