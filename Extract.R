
library(tidyverse)

# Cleaning Selected 2018 Mother Blocks ------------------------------------

#Now we have identified the set of households appearing in other years wrt 2018. 
#Now clean each specifcally identified blocks starting with 2018, so that bind_rows can be applied to the data.

#cleaning iden 2018
Iden<-readxl::read_excel("./1998-2018 panel/B1 KMS 2018 Main Module.xlsx") %>% 
  filter(ScheduleNo!=18443)


Iden<-Iden %>% select(-1:-5, -Panel, -Panel_No, -DoI1, -DoI2, -TimeTaken1, -TimeTaken2)


#Cleaning Block 2
Individual<-readxl::read_excel("./1998-2018 panel/B2 KMS 2018 Individual.xlsx") %>% 
  filter(ScheduleNo!=18443)

eeptools::isid(Individual, vars=c("ScheduleNo", "SlNo"))

Individual<-Individual %>% rename(Person_Sl_No_2018=SlNo,
                      Relation_HH = B2Q2Relation,
                      Gender = B2Q3Gender,
                      Person_Sl_No_1998 = B2Q4SlNofrom1998,
                      Person_Sl_No_2003 = B2Q5SlNofrom2003,
                      Person_Sl_No_2008 = B2Q6SlNofrom2008,
                      Person_Sl_No_2013 = B2Q7SlNofrom2013, 
                      DoB_year = B2Q8DOByyyy,
                      Education = B2Q9Edn,
                      Economic_act = B2Q10EA,
                      Edu_Institute_Attending = B2Q11InstType,
                      Occupation = B2Q12Occu,
                      Hrs_worked_per_day = B2Q13Hr,
                      Days_worked_per_week = B2Q14Days,
                      Monthly_income = B2Q15Salary,
                      Marital_status = B2Q16MS,
                      Women_left = B2Q17MW) %>% 
  select(-B2Q2RelationSpecify, -B2Q8DOBmm, -B2Q12OccuSpecify)


#Cleaning Block 4
HH_Details<-readxl::read_excel("./1998-2018 panel/B4 Household Details.xlsx") %>% 
  filter(ScheduleNo!=18443)

eeptools::isid(HH_Details, vars=c("ScheduleNo"))

HH_Details<-HH_Details %>% rename(Ration_card = B4Q1,
                      Ration_card_Color = B4Q2,
                      Cooking_fuel_used = B4Q3,
                      Cook_fuel_stacking = B4Q3Specify,
                      House_type = B4Q4,
                      House_ownership_type = B4Q51,
                      Other_house_owned = B4Q52,
                      Loan_for_house_construction= B4Q53,
                      House_construct_cost = B4Q54,
                      Land_owned = B4Q61,
                      Land_owned_elsewhere = B4Q62,
                      Total_land_owned = B4Q63,
                      Motor_car = B4Q71,
                      Taxi_truck_lorry = B4Q72,
                      Scooter = B4Q73,
                      Mobile_phone = B4Q74,
                      TV = B4Q75,
                      TV_type = B4Q751,
                      Refrigerator = B4Q76,
                      Washing_machine = B4Q77,
                      Microwave_oven = B4Q78,
                      Computer = B4Q79,
                      Internet = B4Q710,
                      AC = B4Q711,
                      Inverter = B4Q712,
                      Religion = B4Q8, 
                      Religion_spec = B4Q8Specify,
                      Hindu_caste = B4Q81,
                      Hindu_caste_spec = B4Q81Specify,
                      Christ_denom =B4Q82,
                      Christ_denom_spec = B4Q82Specify,
                      Muslim_denom = B4Q83,
                      Muslim_denom_spec = B4Q83Specify,
                      Food_exp_month = B4Q91M,
                      Non_food_exp_month = B4Q92M,
                      Non_food_exp_yr = B4Q92Y,
                      Medical_exp_month = B4Q93Y,
                      Edu_exp_yr = B4Q94Y,
                      Total_consm_exp_month = B4Q95Total,
                      Total_income_month = B4Q10Income,
                      Total_savings_yr =B4Q11Savings,
                      Total_invest_yr = B4Q12Investment,
                      Total_debt_yr = B4Q13Debt,
                      Prospective_EMI = B4Q14, # Anyone in family about to emigrate
                      Prefer_kerala = B4Q151, # if not, why not abroad? following vars are ranks of answers
                      Prefer_india =B4Q152,
                      No_contact_out_india = B4Q153,
                      Ignorant_job_oppor_out_india = B4Q154,
                      Ignorant_apply_out_india = B4Q155,
                      Emig_expensive = B4Q156,
                      Close_to_family = B4Q157,
                      Work_cond_abroad_bad = B4Q158,
                      Living_cond_abroad_bad = B4Q159,
                      Other_reasons = B4Q1510,
                      Other_reasons_spec = B4Q1510Specify) %>% 
  select(-B4Q51Specify)


RM<-readxl::read_excel("./1998-2018 panel/B5 ReturnMigration.xlsx") %>% 
  filter(ScheduleNo!=18443)

eeptools::isid(RM, vars=c("ScheduleNo", "SlNo"))

RM<-RM %>% rename(Person_Sl_No_2018=SlNo,
                  Mig_date_REM_ROM = B5Q3yyyy,
                  First_dest_abroad_REM_ROM = B5Q4,
                  First_dest_country_state_REM_ROM = B5Q5,
                  Economic_act_REM_ROM = B5Q6,
              Industry_REM_ROM = B5Q61,
              Occupation_REM_ROM = B5Q62,
              Monthly_income_REM_ROM = B5Q63,
              REM_ROM = B5Q7,
              Country_state_REM_ROM = B5Q8,
              Return_date_REM_ROM = B5Q9,
              Return_reason_REM_ROM = B5Q10,
              Return_reason_spec_REM_ROM = B5Q10specify,
              Disease_accident_abroad_REM_ROM = B5Q11,
              Disease_accident_abroad_name_REM_ROM = B5Q12,
              Disease_accident_abroad_name_spec_REM_ROM = B5Q12specify,
              Treat_from_REM_ROM = B5Q13,
              Work_related_disease_REM_ROM = B5Q14,
              Nature_work_REM_ROM = B5Q15) %>% 
  select(-B5Q5specify, -B5Q8specify, -B5Q15specify,-B5Q3mm) %>% 
  mutate(REM_ROM = case_when(
    REM_ROM==1 ~ 0, # 0 is ROM
    REM_ROM==2 ~ 1  # 1 is REM
  ))



OSMC<-readxl::read_excel("./1998-2018 panel/B5A Oversease Migration Cost.xlsx") %>% 
  filter(ScheduleNo!=18443)

eeptools::isid(OSMC, vars=c("ScheduleNo", "SlNo"))

OSMC<-OSMC %>% rename(Person_Sl_No_2018=SlNo,
                Mig_yr_REM_ROM = B5A11,
                Paid_agent_REM_ROM = B5A12,
                Paid_others_REM_ROM = B5A13,
                Passport_exp_REM_ROM = B5A14,
                Visa_exp_REM_ROM = B5A15,
                Air_ticket_REM_ROM = B5A16,
                Emigration_clearance_REM_ROM = B5A17,
                Medical_test_exp_REM_ROM = B5A18,
                Other_exp_REM_ROM = B5A19,
                Total_emig_exp_REM_ROM = B5A110Total,
                Personal_savings_emig_fin_REM_ROM = B5A21,
                Parents_savings_emig_fin_REM_ROM = B5A22,
                Friends_borrow_emig_fin_REM_ROM = B5A23,
                Money_lender_emig_fin_REM_ROM = B5A24,
                Bank_emig_fin_REM_ROM = B5A25,
                Sell_mortgage_emig_fin_REM_ROM = B5A26,
                Other_sources_emig_fin_REM_ROM = B5A27)




Migration<-readxl::read_excel("./1998-2018 panel/B7 Migration.xlsx") %>% 
  filter(ScheduleNo!=18443)

eeptools::isid(Migration, vars=c("ScheduleNo", "B7SlNo"))

Migration<-Migration %>% rename(Person_Sl_No_2018 = B7SlNo,
                     Relation_HH = B7Q3,
                     Gender = B7Q4,
                     DoB_year = B7Q5yyyy,
                     EMI_OMI = B7Q6,
                     Country_state_EMI_OMI = B7Q7,
                     First_visit_yr_EMI_OMI = B7Q8yyyy,
                     First_vist_age_EMI_OMI = B7Q9,
                     Pre_mig_education_EMI_OMI = B7Q10, # at the time of migration
                     Pre_mig_marital_status_EMI_OMI = B7Q11,
                     Marital_status = B7Q12,
                     Education = B7Q13,
                     Pre_mig_economic_act_EMI_OMI = B7Q14,
                     Pre_mig_emp_stat_EMI_OMI = B7Q141,
                     Pre_mig_industry_EMI_OMI = B7Q142,
                     Pre_mig_occupation_EMI_OMI = B7Q143,
                     Pre_mig_monthly_income_EMI_OMI = B7Q144,
                     Economic_act = B7Q15,
                     Emp_stat = B7Q151,
                     Industry = B7Q152,
                     Occupation = B7Q153,
                     Monthly_income = B7Q154,
                     Reason_migration_EMI_OMI = B7Q16) %>% 
  select(-starts_with("B7")) %>% mutate(EMI_OMI = case_when(
    EMI_OMI==1 ~ 0, #coding 1 as 2 i.e OMI
    EMI_OMI==2 ~ 1  #coding 2 as 1 i.e EMI
  ))






Remittances<-readxl::read_excel("./1998-2018 panel/B8 Remittances.xlsx") %>% 
  filter(ScheduleNo!=18443)

eeptools::isid(Remittances, vars=c("ScheduleNo", "B78SlNo"))

Remittances<-Remittances %>% rename(Person_Sl_No_2018 = B78SlNo,
                         Mode_EMI_remit = B8Q1EMI,
                         Periodicity_EMI_remit = B8Q2EMI,
                         Mode_OMI_remit = B8Q1OMI,
                         Periodicity_OMI_remit = B8Q2OMI,
                         Cash_EMI_remit = B8Q3EMI,
                         Everyday_HH_exp_EMI_remit = B8Q31EMI) %>% rowwise() %>% 
  
  mutate(Child_edu_EMI_remit = sum(c(B8Q32EMI, B8Q63EMI), na.rm = T),
         Debt_repay_EMI_remit = sum(c(B8Q33EMI,B8Q64EMI), na.rm = T)) %>% 
  
  rename(Build_buy_HH_EMI_remit = B8Q34EMI,
         Repair_renew_HH_EMI_remit = B8Q35EMI) %>% 
  
  mutate(Buy_land_EMI_remit = sum(c(B8Q36EMI,B8Q5EMI), na.rm = T)) %>% 
  
  rename(Expand_new_business_EMI_remit = B8Q37EMI,
         Donation_EMI_remit = B8Q38EMI,
         Gold_EMI_remit = B8Q39EMI,
         Cash_in_hand_EMI_remit = B8Q310EMI,
         Gift_value_EMI_remit = B8Q4EMI,
         Special_total_EMI_remit = B8Q6EMI,
         Medical_aid_EMI_remit = B8Q61EMI,
         Pay_dowry_EMI_remit = B8Q62EMI,
         Stock_invest_EMI_remit = B8Q65EMI) %>% 
  
  mutate(Other_remitances_EMI_remit = sum(c(B8Q311EMI,B8Q66EMI), na.rm = T)) %>%
  
  rename(Total_EMI_remit = B8Q7EMITotal) %>% 
  rename(Cash_OMI_remit = B8Q3OMI,
         Everyday_HH_exp_OMI_remit = B8Q31OMI) %>% rowwise() %>% 
  
  mutate(Child_edu_OMI_remit = sum(c(B8Q32OMI, B8Q63OMI), na.rm = T),
         Debt_repay_OMI_remit = sum(c(B8Q33OMI,B8Q64OMI), na.rm = T)) %>% 
  
  rename(Build_buy_HH_OMI_remit = B8Q34OMI,
         Repair_renew_HH_OMI_remit = B8Q35OMI) %>% 
  
  mutate(Buy_land_OMI_remit = sum(c(B8Q36OMI,B8Q5OMI), na.rm = T)) %>% 
  
  rename(Expand_new_business_OMI_remit = B8Q37OMI,
         Donation_OMI_remit = B8Q38OMI,
         Gold_OMI_remit = B8Q39OMI,
         Cash_in_hand_OMI_remit = B8Q310OMI,
         Gift_value_OMI_remit = B8Q4OMI,
         Special_total_OMI_remit = B8Q6OMI,
         Medical_aid_OMI_remit = B8Q61OMI,
         Pay_dowry_OMI_remit = B8Q62OMI,
         Stock_invest_OMI_remit = B8Q65OMI) %>% 
  
  mutate(Other_remitances_OMI_remit = sum(c(B8Q311OMI,B8Q66OMI), na.rm = T)) %>%
  
  rename(Total_OMI_remit = B8Q7OMITotal) %>%
  
  select(-starts_with("B8Q")) %>% ungroup() %>% 
  mutate(Person_Sl_No_2018=Person_Sl_No_2018-70)







Emigration_cost<-readxl::read_excel("./1998-2018 panel/B9 Emigration Cost.xlsx") %>%  
filter(ScheduleNo!=18443)

eeptools::isid(Emigration_cost, vars=c("ScheduleNo", "B7SlNo"))

Emigration_cost<-Emigration_cost %>% rename(Person_Sl_No_2018=B7SlNo,
                           Mig_yr_EMI = B9A11,
                           Paid_agent_EMI = B9A12,
                           Paid_others_EMI = B9A13,
                           Passport_exp_EMI = B9A14,
                           Visa_exp_EMI = B9A15,
                           Air_ticket_EMI = B9A16,
                           Emigration_clearance_EMI = B9A17,
                           Medical_test_exp_EMI = B9A18,
                           Other_exp_EMI = B9A19,
                           Total_emig_exp_EMI = B9A110Total,
                           Personal_savings_emig_fin_EMI = B9A21,
                           Parents_savings_emig_fin_EMI = B9A22,
                           Friends_borrow_emig_fin_EMI = B9A23,
                           Money_lender_emig_fin_EMI = B9A24,
                           Bank_emig_fin_EMI = B9A25,
                           Sell_mortgage_emig_fin_EMI = B9A26,
                           Other_sources_emig_fin_EMI = B9A27) %>% 
  mutate(Person_Sl_No_2018=Person_Sl_No_2018-70)




Student_Migration<-readxl::read_excel("./1998-2018 panel/B10 Student Migration.xlsx") %>%  
  filter(ScheduleNo!=18443)

eeptools::isid(Student_Migration, vars=c("ScheduleNo", "B7SlNo"))

Student_Migration<-Student_Migration %>% rename(Person_Sl_No_2018=B7SlNo,
                             Destination_stud = B10Q3,
                             First_visit_yr_stud = B10Q4yyyy,
                             Course_stud = B10Q5,
                             Course_duration_stud = B10Q6years,
                             Scholarship_stud = B10Q7,
                             Scholarship_qamount_stud = B10Q72,
                             Capitation_fee_stud = B10Q81,
                             Tuition_fee_stud = B10Q82,
                             Caution_deposit_stud = B10Q83,
                             Travel_cost_monthly_stud = B10Q84,
                             Rent_food_monthly_stud = B10Q85,
                             Other_costs_stud = B10Q86,
                             Total_cost_stud = B10Q87Total,
                             Took_loan_stud = B10Q9,
                             Loan_amount_stud = B10Q91,
                             Bank_name_stud = B10Q92,
                             Interest_rate_stud = B10Q93,
                             Agent_involved_stud = B10Q10,
                             Agent_fee_stud = B10Q101) %>% 
  mutate(Person_Sl_No_2018=Person_Sl_No_2018-70) %>% 
  select(-starts_with(c("B10Q", "B7")))


#Merging HH and Individuals, REM, EMI and other blocks into 2018 cross section

Individual<-bind_rows(Individual, Migration ) %>% arrange(ScheduleNo, Person_Sl_No_2018) %>%
  group_by(ScheduleNo) %>% 
  mutate(Person_Sl_No_2018=row_number())

Cross_2018<-left_join(Iden, Individual) %>% 
  left_join(., HH_Details) %>% 
  left_join(., RM) %>% 
  left_join(., OSMC) %>% 
  left_join(., Remittances) %>% 
  left_join(., Emigration_cost) %>% 
  left_join(., Student_Migration) %>% 
  mutate(year=2018)


eeptools::isid(Cross_2018, vars=c("ScheduleNo", "Person_Sl_No_2018"))

rm(Iden, Individual, HH_Details, RM, OSMC, Migration, Remittances, Emigration_cost, Student_Migration)

save(Cross_2018, file = "./R Data/2018 Cross.Rdata")


# Cleaning Selected 2013 Mother Blocks ------------------------------------


#Filtering HH panel included in 2013
Iden<-readxl::read_excel("./1998-2018 panel/B1 KMS 2018 Main Module.xlsx")

p_2013<-Iden %>% filter(Panel_Year2013==1) %>% select(ScheduleNo, Panel_SLNo2013) %>% 
  mutate(Panel_SLNo2013 = as.double(Panel_SLNo2013)) %>% 
  filter(Panel_SLNo2013!=0, !is.na(Panel_SLNo2013))  %>% 
  group_by(Panel_SLNo2013) %>% mutate(dups=max(row_number())) %>% 
  filter(dups<2) %>% select(-dups)# filtering out serial of panel HH in 2013


HH_2013<-readxl::read_excel("./2013/HOUSEHOLD Part 1 2013.xls") %>% 
  select(1:156, 173, 192, 194, 196)

eeptools::isid(HH_2013, vars = c("SlNo"))

HH_2013<-left_join(p_2013, HH_2013, by=c("Panel_SLNo2013" = "SlNo"))#filtering 2013 panel HH using 2018 info

eeptools::isid(HH_2013, vars = c("Panel_SLNo2013"))

rm(p_2013, Iden)

#Cleaning HH_2013

Iden<-HH_2013 %>% select(-PanelType,-DoI1, -DoI2) %>% 
  rename(SHHNO = HHNo,
         EMI = EMI_Count,
         REM = REM_count,
         OMI = OMI_count,
         ROM = ROM_count,
         THHM = TYPEOFHH) %>% 
  select(1:12, 158)


#cleaning and extracting remittance details from HH_2013
Remittances_cons_exp<-HH_2013 %>% ungroup() %>% select(1,61:161) %>% 
  rename(Receive_remit = B10Q1, # renaming EMI remits
         Amount_EMI_remit = B10Q1_1Abrd,
         Thru_money_transfer_EMI_remit = B10Q2_1Abrd,
         Thru_bank_EMI_remit = B10Q2_2Abrd,
         Thru_hundai_EMI_remit = B10Q2_3Abrd,
         Thru_friends_EMI_remit = B10Q2_4Abrd,
         On_visit_EMI_remit = B10Q2_5Abrd,
         Thru_other_means_EMI_remit = B10Q2_6Abrd,
         Everyday_HH_exp_EMI_remit = B10Q3_1Abrd,
         Build_buy_HH_EMI_remit = B10Q3_4Abrd,
         Repair_renew_HH_EMI_remit = B10Q3_5Abrd,
         Donation_EMI_remit = B10Q3_10Abrd,
         Gold_EMI_remit = B10Q3_13Abrd,
         Cash_in_hand_EMI_remit = B10Q3_14Abrd,
         Periodicity_EMI_remit = B10Q4Abrd,
         Gift_value_EMI_remit = B10Q5_6Abrd) %>% rowwise() %>% 
  
    mutate(Automobile_buy_EMI_remit = sum(c(B10Q3_11Abrd, B10Q8_1Abrd),na.rm = T), # mutate for EMI remits
           Stock_invest_EMI_remit = sum(c(B10Q3_9Abrd,B10Q8_2Abrd),na.rm = T),
           Expand_new_business_EMI_remit = sum(c(B10Q3_8Abrd,B10Q8_3Abrd),na.rm = T),
           Pay_dowry_EMI_remit = sum(c(B10Q3_7Abrd,B10Q8_4Abrd),na.rm = T),
           Child_edu_EMI_remit = sum(c(B10Q3_2Abrd,B10Q8_5Abrd),na.rm = T),
           Medical_aid_EMI_remit = sum(c(B10Q3_12Abrd,B10Q8_6Abrd),na.rm = T),
           Debt_repay_EMI_remit = sum(c(B10Q3_3Abrd,B10Q8_7Abrd),na.rm = T),
           Other_remitances_EMI_remit = sum(c(B10Q3_15Abrd,B10Q8_8Abrd),na.rm = T),
           Buy_land_EMI_remit = sum(c(B10Q3_6Abrd,B10Q7Abrd), na.rm = T)) %>% 
    
    rename(Amount_OMI_remit = B10Q1_1OthrState, # renaming OMI remit
           Thru_money_transfer_OMI_remit = B10Q2_1OthrState,
           Thru_bank_OMI_remit = B10Q2_2OthrState,
           Thru_hundai_OMI_remit = B10Q2_3OthrState,
           Thru_friends_OMI_remit = B10Q2_4OthrState,
           On_visit_OMI_remit = B10Q2_5OthrState,
           Thru_other_means_OMI_remit = B10Q2_6OthrState,
           Everyday_HH_exp_OMI_remit = B10Q3_1OthrState,
           Build_buy_HH_OMI_remit = B10Q3_4OthrState,
           Repair_renew_HH_OMI_remit = B10Q3_5OthrState,
           Donation_OMI_remit = B10Q3_10OthrState,
           Gold_OMI_remit = B10Q3_13OthrState,
           Cash_in_hand_OMI_remit = B10Q3_14OthrState,
           Periodicity_OMI_remit = B10Q4OthrState,
           Gift_value_OMI_remit = B10Q5_6OthrState) %>% rowwise() %>% 
    #mutating for OMI remit
    mutate(Automobile_buy_OMI_remit = sum(c(B10Q3_11OthrState, B10Q8_1OthrState),na.rm = T),
           Stock_invest_OMI_remit = sum(c(B10Q3_9OthrState,B10Q8_2OthrState),na.rm = T),
           Expand_new_business_OMI_remit = sum(c(B10Q3_8OthrState,B10Q8_3OthrState),na.rm = T),
           Pay_dowry_OMI_remit = sum(c(B10Q3_7OthrState,B10Q8_4OthrState),na.rm = T),
           Child_edu_OMI_remit = sum(c(B10Q3_2OthrState,B10Q8_5OthrState),na.rm = T),
           Medical_aid_OMI_remit = sum(c(B10Q3_12OthrState,B10Q8_6OthrState),na.rm = T),
           Debt_repay_OMI_remit = sum(c(B10Q3_3OthrState,B10Q8_7OthrState),na.rm = T),
           Other_remitances_OMI_remit = sum(c(B10Q3_15OthrState,B10Q8_8OthrState),na.rm = T),
           Buy_land_OMI_remit = sum(c(B10Q3_6OthrState,B10Q7OthrState), na.rm = T)) %>% 
  
  mutate(Total_EMI_remit = sum(c(B10Q3_16AbrdTot,Gift_value_EMI_remit, TotalQ8),na.rm = T),
         Total_OMI_remit = sum(c(B10Q3_16OthrStateTot,Gift_value_OMI_remit, Total8Q8),na.rm = T)) %>% 
  
  ungroup() %>% 
  
  mutate(Mode_OMI_remit = case_when( # creating mode of remit for EMI and OMI using payment mode split up
    Thru_money_transfer_OMI_remit > 0 ~ 2,
    Thru_bank_OMI_remit > 0 ~ 1,
    Thru_hundai_OMI_remit > 0 ~ 5,
    Thru_friends_OMI_remit > 0 ~ 3,
    On_visit_OMI_remit > 0 ~ 4,
    Thru_other_means_OMI_remit > 0 ~ 5),
    
    Mode_EMI_remit = case_when(
      Thru_money_transfer_EMI_remit > 0 ~ 2,
      Thru_bank_EMI_remit > 0 ~ 1,
      Thru_hundai_EMI_remit > 0 ~ 5,
      Thru_friends_EMI_remit > 0 ~ 3,
      On_visit_EMI_remit > 0 ~ 4,
      Thru_other_means_EMI_remit > 0 ~ 5)) %>% 
  
  rename(Food_exp_month = FoodItemsLM,
         Non_food_exp_month = NonFoodItemsLM,
         Medical_exp_month = medeXlmnew,
         Edu_exp_yr = edueXlmnew,
         Total_consm_exp_month = totalmonthlyexpenses,
         Total_savings_yr =totalsavingL12M,
         Total_invest_yr = totalinvestmemntL12M,
         Total_debt_yr = TotalDebtL12M) %>% 
mutate(Non_food_exp_yr = Non_food_exp_month * 12) %>%
  
  select(-starts_with("B10"), -Total8Q8, -TotalQ8,-CigaretteLM,-BidiLM,-OtherTubaccoProductsLM,
         -AlcoholicProductsLM, -othersXlmnew,-OtherExpensesspecify, -TYPEOFHH)


#Cleaning HH_details of 2013 from HH_2013 file
HH_Details<-HH_2013 %>% select(1:2, 33:60) %>% 
  rename(Ration_card = RationCard,
         Ration_card_Color = RationCardColour,
         Cooking_fuel_used = CookingFuel,
         House_type = HouseType,
         Total_land_owned = Q6_3LandExtend,
         Other_house_owned = Q6_1Home,
         Land_owned_elsewhere = Q6_2Land,
         Motor_car = Q7_1B3,
         Taxi_truck_lorry = Q7_2B3,
         Scooter = Q7_3B3,
         Telephone = Q7_4B3,
         Mobile_phone = Q7_5B3,
         TV = Q7_6B3,
         DVD_player = Q7_7B3,
         Refrigerator = Q7_8B3,
         Microwave_oven = Q7_10B3,
         Computer = Q7_9B3,
         Internet = Q7_11B3,
         
         Hindu_caste = Q8_1,
         Hindu_caste_spec = Q8_1Others,
         Christ_denom =Q8_2,
         Muslim_denom = Q8_3) %>% 
  select(-CookingFuelOther, -LandExtend, -Q6B3,
         -Q8_2Others, -OwnHouse)

#Cleaning Individual details of 2013
HH_2013<-HH_2013 %>% select(1:2)


Individual<-readxl::read_excel("./2013/KMS Individual 2013.xls") %>% 
  rename(Panel_SLNo2013=SlNo) %>% 
  left_join(HH_2013, .) %>% 
select(1:3, 15:34)

Individual<-Individual %>% group_by(ScheduleNo, MemberID) %>% 
  mutate(dups=row_number()) %>% filter(dups!=2) %>% select(-dups) %>% 
  ungroup()# to remove the duplication of 7th member 0f HH 15299


eeptools::isid(Individual, vars=c("ScheduleNo", "MemberID"))



Individual<-Individual %>% mutate(DoB_year = as.double(lubridate::year(DoBMMYY))) %>% 
  rename(Person_Sl_No_2013 = MemberID,
         Relation_HH = Relation_to_HHH,
         Gender = Sex,
         Education = EduCodeCompl,
         Economic_act = EcoAct,
         Emp_stat = EmpStatus,
         Occupation = Occup,
         Hrs_worked_per_day = HoursWpD,
         Days_worked_per_week = DaysWorkedPWeek1,
         Monthly_income = MonthlyIncome,
         Marital_status = MaritalStatus) %>% 
  select(-RelationOther, -DoBMMYY, -EduCodeComplOthers, -EduCodePurs, -EduCodePursOthers,
         -EcoActOthers, -IndustryOthers, -OccupOthers, -ScheduleNo)

eeptools::isid(Individual, vars=c("Panel_SLNo2013", "Person_Sl_No_2013"))

#Individual<-Individual1 %>%  select(1, 3, 4:15, 38)

#HH_Details<-Individual1 %>% select(1:3, 16:37)

#Cleaning EMI&OMI details of 2013

HH_2013<-HH_2013 %>% select(1:2)


EMI<-readxl::read_excel("./2013/EMI 2013.xls") %>% 
  rename(Panel_SLNo2013=SlNo) %>% 
  select(1,20, 22, 23, 26,29:57) %>% rename(Person_Sl_No_2013 = MemberNo,
    Relation_HH = Relation,
    Gender = Sex,
    DoB_year = DOBYear,
    EMI_OMI = CurrentResidenceCode,
    Country_state_EMI_OMI = CountryStateCode,
    First_visit_yr_EMI_OMI = Firstleaveyear1,
    First_vist_age_EMI_OMI = AgeAtFirstLeft,
    Pre_mig_education_EMI_OMI = EducationAtMig, # at the time of migration
    Pre_mig_marital_status_EMI_OMI = MaritalStatusatMigration,
    Marital_status = CurrentMaritalStatus,
    Education = CurrentEducationStat,
    Pre_mig_economic_act_EMI_OMI = EconomicActivityBMig,
    Pre_mig_emp_stat_EMI_OMI = EmpStatusBMig,
    Pre_mig_industry_EMI_OMI = IndustryCodeBMig,
    Pre_mig_occupation_EMI_OMI = OccupationCodeBMig,
    Pre_mig_monthly_income_EMI_OMI = MonthlyIncomeBMig,
    Economic_act = EconomicActivityAMig,
    Emp_stat = EmpStatusAMig,
    Industry = IndustryCodeAMig,
    Occupation = OccupationCodeAMig,
    Monthly_income = MonthlyIncomeAMig) %>% 
  
  select(-LeaveFirstTime, -FIRSTLEAVEmonth1, -AGEFIRSTKEFTGROUP, -EducationOther, -CurrentEducationOther,
         -EconomicActivityBMigOther, -IndustryCodeBMigOthers, -OccupationCodeBMigOther, -EconomicActivityAMigOther,
         -IndustryCodeAMigOthers, -OccupationCodeAMigOther)

OMI<-readxl::read_excel("./2013/OMI 2013.xls") %>% 
  rename(Panel_SLNo2013=SlNo) %>% 
  select(1,18,20, 21, 24, 27:55) %>% 
  rename(Person_Sl_No_2013 = MemberNo,
             Relation_HH = Relation,
             Gender = Sex,
             DoB_year = DOBYear,
             EMI_OMI = CurrentResidenceCode,
             Country_state_EMI_OMI = CountryStateCode,
             First_visit_yr_EMI_OMI = Firstleaveyear1,
             First_vist_age_EMI_OMI = AgeAtFirstLeft,
             Pre_mig_education_EMI_OMI = EducationAtMig, # at the time of migration
             Pre_mig_marital_status_EMI_OMI = MaritalStatusatMigration,
             Marital_status = CurrentMaritalStatus,
             Education = CurrentEducationStat,
             Pre_mig_economic_act_EMI_OMI = EconomicActivityBMig,
             Pre_mig_emp_stat_EMI_OMI = EmpStatusBMig,
             Pre_mig_industry_EMI_OMI = IndustryCodeBMig,
             Pre_mig_occupation_EMI_OMI = OccupationCodeBMig,
             Pre_mig_monthly_income_EMI_OMI = MonthlyIncomeBMig,
             Economic_act = EconomicActivityAMig,
             Emp_stat = EmpStatusAMig,
             Industry = IndustryCodeAMig,
             Occupation = OccupationCodeAMig,
             Monthly_income = MonthlyIncomeAMig) %>% 
  
  select(-LeaveFirstTime, -FIRSTLEAVEmonth1, -AGEFIRSTKEFTGROUP, -EducationOther, -CurrentEducationOther,
         -EconomicActivityBMigOther, -IndustryCodeBMigOthers, -OccupationCodeBMigOther, -EconomicActivityAMigOther,
         -IndustryCodeAMigOthers, -OccupationCodeAMigOther)

#Binding EMI and OMI into one
EMI_OMI<-bind_rows(EMI, OMI) %>% 
  mutate(Person_Sl_No_2013 = Person_Sl_No_2013+70) %>% mutate(EMI_OMI = case_when(
    EMI_OMI==1 ~ 0, #coding 1 as 2 i.e OMI
    EMI_OMI==2 ~ 1  #coding 2 as 1 i.e EMI
  ))


#EMI_OMI<-left_join(HH_2013, EMI_OMI)

rm(EMI, OMI)


#Loading in EMI for extracting costs of Migration

EMI<-readxl::read_excel("./2013/EMI 2013.xls") %>% 
  rename(Panel_SLNo2013=SlNo) %>% 
  select(1,20,58:72) %>% 
  rename(Person_Sl_No_2013 = MemberNo,
         Paid_agent_EMI = B11Q1_1,
         Passport_exp_EMI = B11Q1_2,
         Visa_exp_EMI = B11Q1_3,
         Air_ticket_EMI = B11Q1_4,
         Emigration_clearance_EMI = B11Q1_5,
         Medical_test_exp_EMI = B11Q1_6,
         Other_exp_EMI = B11Q1_7,
         Total_emig_exp_EMI = B11Q1_8Tot,
         Personal_savings_emig_fin_EMI = B11Q2_1,
         Parents_savings_emig_fin_EMI = B11Q2_2,
         Friends_borrow_emig_fin_EMI = B11Q2_3,
         Money_lender_emig_fin_EMI = B11Q2_4,
         Bank_emig_fin_EMI = B11Q2_5,
         Sell_mortgage_emig_fin_EMI = B11Q2_6,
         Other_sources_emig_fin_EMI = B11Q2_7)
  
#left_join(HH_2013, .)

EMI_OMI<-left_join(EMI_OMI, EMI)

rm(EMI)

eeptools::isid(EMI_OMI, vars=c("Panel_SLNo2013", "Person_Sl_No_2013"))


#Cleaning REM&ROM in 2013

REM<-readxl::read_excel("./2013/REM 2013.xls") %>% 
  select(1,12, 17,26:35, 40, 63:70) %>% 
  rename(Panel_SLNo2013 = SlNo,
         Person_Sl_No_2013 = MemberID,
         Mig_date_REM_ROM = firstgone_year,
         Economic_act_REM_ROM = EconomicActivityBRetn,
         Industry_REM_ROM = IndustryCodeBRetn,
         Occupation_REM_ROM = OccupationCodeBRetn,
         Monthly_income_REM_ROM = MonthlyIncomeBRetn,
         REM_ROM = ResidenceBRetnCode,
         Country_state_REM_ROM = CountryStateCode,
         Return_date_REM_ROM = return_year,
         
         Paid_agent_REM_ROM = B13Q1_1,
         Passport_exp_REM_ROM = B13Q1_2,
         Visa_exp_REM_ROM = B13Q1_3,
         Air_ticket_REM_ROM = B13Q1_4,
         Emigration_clearance_REM_ROM = B13Q1_5,
         Medical_test_exp_REM_ROM = B13Q1_6,
         Other_exp_REM_ROM = B13Q1_7,
         Total_emig_exp_REM_ROM = B13Q1_8Tot) %>% 
  select(-EconomicActivityBRetnOther, -IndustryCodeBRetnOthers, -OccupationCodeBRetnOther, -EmpStatusBRetn)



ROM<-readxl::read_excel("./2013/ROM 2013.xls") %>% 
  select(1, 13, 18, 27:36, 41) %>% 
  rename(Panel_SLNo2013 = SlNo,
         Person_Sl_No_2013 = MemberID,
         Mig_date_REM_ROM = firstgone_year,
         Economic_act_REM_ROM = EconomicActivityBRetn,
         Industry_REM_ROM = IndustryCodeBRetn,
         Occupation_REM_ROM = OccupationCodeBRetn,
         Monthly_income_REM_ROM = MonthlyIncomeBRetn,
         REM_ROM = ResidenceBRetnCode,
         Country_state_REM_ROM = CountryStateCode,
         Return_date_REM_ROM = return_year) %>% 
  select(-EconomicActivityBRetnOther, -IndustryCodeBRetnOthers, -OccupationCodeBRetnOther, -EmpStatusBRetn)


REM_ROM<-bind_rows(REM, ROM) %>% mutate(REM_ROM = case_when(
  REM_ROM==1 ~ 0, #coding 1 as 0 i.e ROM
  REM_ROM==2 ~ 1  #coding 2 as 1 i.e REM
))


eeptools::isid(REM_ROM, vars=c("Panel_SLNo2013", "Person_Sl_No_2013"))

rm(REM, ROM)

#REM_ROM<-left_join(HH_2013, REM_ROM)


Individual<-bind_rows(Individual, EMI_OMI) %>% arrange(Panel_SLNo2013, Person_Sl_No_2013) %>%
  group_by(Panel_SLNo2013) %>% 
  mutate(Person_Sl_No_2013 = row_number())


Cross_2013<-left_join(Iden, Individual) %>% 
  left_join(., Remittances_cons_exp) %>% 
  left_join(., HH_Details) %>% 
  left_join(., REM_ROM) %>% 
  mutate(year=2013) %>% ungroup()


eeptools::isid(Cross_2013, vars=c("ScheduleNo", "Person_Sl_No_2013"))

rm(EMI_OMI, REM_ROM, Remittances_cons_exp, Individual, Iden, HH_2013, HH_Details)

save(Cross_2013, file = "./R Data/2013 Cross.Rdata")



# Cleaning Selected 2008 Mother Blocks ------------------------------------
Iden<-readxl::read_excel("./1998-2018 panel/B1 KMS 2018 Main Module.xlsx")

p_2008<-Iden %>% filter(Panel_Year2008==1) %>% select(ScheduleNo, Panel_SLNo2008) %>% 
  filter(Panel_SLNo2008!=0, !is.na(Panel_SLNo2008))  %>% 
  group_by(Panel_SLNo2008) %>% mutate(dups=max(row_number())) %>% 
  filter(dups<2) %>% select(-dups)# filtering out serial of panel HH in 2008

HH_2008<-readxl::read_excel("./2008/MigBLOCK1.xlsx")
eeptools::isid(HH_2008, vars = c("ManualNO")) 


HH_2008<-left_join(p_2008, HH_2008, by=c("Panel_SLNo2008" = "ManualNO"))#filtering 2008 panel HH using 2018 info

rm(p_2008, Iden)


#cleaning Iden of 2008
HH_2008<-HH_2008 %>% select(-5:-8, -16:-22) %>% #filtering hhno. investigator name, time etc.
  rename(FSUNo = FSU,
         SHHNO = SCHNO,
         THHM = HTYPE)  
eeptools::isid(HH_2008, vars = c("ScheduleNo")) 

Iden<-HH_2008

HH_2008<-HH_2008 %>% select(1:2) %>% ungroup()


#Cleaning Individual of 2008
Individual<-readxl::read_excel("./2008/MigBLOCK2.xlsx")


Individual<-Individual %>% select(-1, -4, -7, -9, -11) %>% 
  mutate(DoB_year = lubridate::year(DOB)) %>% 
  select(-DOB) %>% rename(Panel_SLNo2008 = ManualNO,
                          Person_Sl_No_2008=SLNO,
                          Gender = SEX,
                          Age = AgeYears,
                          Relation_HH = REL,
                          Education = ESTAT,
                          Economic_act = ACT,
                          Emp_status = LabourForceStatus,
                          Marital_status = MSTAT,
                          Women_left = WOMENleft) %>% 
  left_join(HH_2008, .) %>% 
  select(-ScheduleNo)#retaining only panel HH's 


eeptools::isid(Individual, vars = c("Panel_SLNo2008", "Person_Sl_No_2008"))


#Cleaning REM_ROM of 2008

REM_ROM<-readxl::read_excel("./2008/MigBLOCK3.xlsx")

REM_ROM<-REM_ROM %>% select(-1, -4:-7, -10:-11, -13, -17, -20:-22) %>% 
  rename(Person_Sl_No_2008 = SLNO,
         Panel_SLNo2008 = ManualNO,
         First_dest_country_state_REM_ROM = M13,
         Country_state_REM_ROM = M15,
         REM_ROM = M16,
         Pre_mig_Economic_act_REM_ROM = M14,
         Pre_mig_emp_status_REM_ROM = `LabourForceStatus prior to migration`) %>%  
  mutate(Mig_date_REM_ROM = lubridate::year(lubridate::my(case_when(
    !is.na(M12a) ~ M12a,
    is.na(M12a) ~ M12b
  ))),
  Return_date_REM_ROM =lubridate::year(lubridate::my(M17))) %>% 
  select(-M17, -M12a, -M12b) %>% mutate(REM_ROM = case_when(
    REM_ROM==1 ~ 0, #coding 1 as 2 i.e ROM
    REM_ROM==2 ~ 1  #coding 2 as 1 i.e REM
  ))

eeptools::isid(REM_ROM, vars = c("Panel_SLNo2008", "Person_Sl_No_2008"))




#Cleaning EMI_OMI of 2008
EMI_OMI<-readxl::read_excel("./2008/MigBLOCK4.xlsx")

EMI_OMI<-EMI_OMI %>% select(-1, -4, -8, -12, ) %>% 
  rename(Person_Sl_No_2008 = SLNO,
         Panel_SLNo2008 = ManualNO,
         Relation_HH = M19,
         Gender = M20,
         Country_state_EMI_OMI = M21, 
         EMI_OMI = M22,
         First_vist_age_EMI_OMI = M24,
         Pre_mig_education_EMI_OMI = M25,
         Pre_mig_marital_status_EMI_OMI = M26,
         Pre_mig_economic_act_EMI_OMI = M27,
         Pre_mig_emp_stat_EMI_OMI = `LabourForceStatus prior to Migration`,
         Economic_act = M28,
         Emp_stat = `CurrentLabourForceStatus`) %>% 
  mutate(First_visit_yr_EMI_OMI = lubridate::year(lubridate::my(M23))) %>% 
    select(-M23) %>% 
    mutate(EMI_OMI = case_when(
      EMI_OMI==1 ~ 0, #coding 1 as 2 i.e OMI
      EMI_OMI==2 ~ 1  #coding 2 as 1 i.e EMI
    ))
  
        # Person_Sl_No_2008 = str_sub(as.character(Person_Sl_No_2008), start = -1L),
        # Person_Sl_No_2008 = as.double(Person_Sl_No_2008)) %>% 


eeptools::isid(EMI_OMI, vars = c("Panel_SLNo2008", "Person_Sl_No_2008"))

Individual<-bind_rows(Individual, EMI_OMI) %>%arrange(Panel_SLNo2008, Person_Sl_No_2008) %>%
  group_by(Panel_SLNo2008) %>% 
  mutate(Person_Sl_No_2008=row_number())



#Cleaning HH_Detail of 2008

HH_Details<-readxl::read_excel("./2008/MigBLOCK5.xlsx") %>% 
  select(-1, -M30sp, -M301, -M34sp) %>% rename(Panel_SLNo2008 = ManualNO,
                                               HH_electricity = M29,
                                               Cooking_fuel_used = M30,
                                               House_type = M31,
                                               Other_house_owned = M321,
                                               Land_owned = M322,
                                               Total_land_owned = M323,
                                               
                                               Motor_car = M331,
                                               Taxi_truck_lorry = M332,
                                               Scooter = M333,
                                               Telephone = M334,
                                               Mobile_phone = M335,
                                               TV = M336,
                                               DVD_player = M337,
                                               Refrigerator = M338,
                                               Electric_cooking_oven = M339,
                                               Microwave_oven = M3310,
                                               Baking_oven = M3311,
                                               Computer = M3312,
                                               
                                               Religion = M34, 
                                               Hindu_caste = M351,
                                               Hindu_caste_spec = M351sp,
                                               Christ_denom =M352,
                                               Christ_denom_spec = M352sp,
                                               Muslim_denom = M353)

eeptools::isid(HH_Details, vars = c("Panel_SLNo2008"))


#Cleaning remittances of 2008
Remittances<-readxl::read_excel("./2008/MigBLOCK6.xlsx") %>%  
  select(-1,-5:-10,-12:-22, -33, -M40) %>%
  rename(Panel_SLNo2008 = ManualNO,
         Receive_remit = M36,
         Amount_EMI_remit = M37,
         Gift_value_EMI_remit = M38Amt,
         Child_edu_EMI_remit = M41Edn,
         Debt_repay_EMI_remit = M41Debt,
         Buy_land_EMI_remit = M40Amt,
         Pay_dowry_EMI_remit = M41Dowry,
         Expand_new_business_EMI_remit = M41eprise,
         Stock_invest_EMI_remit = M41share,
         Other_remitances_EMI_remit = M41Other,
         Automobile_buy_EMI_remit = M41car,
         Medical_aid_EMI_remit = M41Med)

eeptools::isid(Remittances, vars = c("Panel_SLNo2008"))

#Cleaning OSMC of 2008
OSMC<-readxl::read_excel("./2008/MigBLOCK7n8.xlsx") %>% 
  select(-1, -4, ) %>% 
  rename(Panel_SLNo2008 = ManualNO,
         Person_Sl_No_2008=SLNO,
         Paid_agent_EMI = M42a,
         Paid_others_EMI = M42b,
         Passport_exp_EMI = M42c,
         Visa_exp_EMI = M42d,
         Air_ticket_EMI = M42e,
         Emigration_clearance_EMI = M42f,
         Lost_to_fraud_EMI = M42g,
         Total_emig_exp_EMI = M42Total,
         
         Family_members_emig_fin_EMI = M43a,
         Personal_savings_emig_fin_EMI = M43b,
         Parents_savings_emig_fin_EMI = M43c,
         Friends_borrow_emig_fin_EMI = M43d,
         Money_lender_emig_fin_EMI = M43e,
         Bank_emig_fin_EMI = M43f,
         Other_sources_emig_fin_EMI = M43k,
         Sell_mortgage_emig_fin_EMI = M43g,
         Govt_assistance_emig_fin_EMI = M43j) %>% 
  select(-M43h,-M43i)


eeptools::isid(OSMC, vars = c("Panel_SLNo2008", "Person_Sl_No_2008"))

Cross_2008<-left_join(Iden, Individual) %>% 
  left_join(., HH_Details) %>% 
  left_join(., REM_ROM) %>% 
  left_join(., OSMC) %>% 
  left_join(., Remittances) %>% 
  mutate(year=2008) %>% ungroup()

Cross_2008<-Cross_2008 %>% group_by(ScheduleNo, Person_Sl_No_2008) %>% mutate(
  dups= row_number()) %>%  filter(dups==1) %>% 
  select(-dups) %>% ungroup()# removing duplicat members of HH 20134

eeptools::isid(Cross_2008, vars = c("ScheduleNo", "Person_Sl_No_2008"))

rm(Iden, Individual, REM_ROM, EMI_OMI, HH_Details, OSMC, Remittances, HH_2008)

save(Cross_2008, file = "./R Data/2008 Cross.Rdata")

# Cleaning Selected 2003 Mother Blocks ------------------------------------

#Filtering HH panel included in 2018
Iden<-readxl::read_excel("./1998-2018 panel/B1 KMS 2018 Main Module.xlsx")

p_2003<-Iden %>% filter(Panel_Year2003==1) %>% select(ScheduleNo, Panel_SLNo2003) %>% 
  mutate(Panel_SLNo2003 = as.double(Panel_SLNo2003)) %>% 
  filter(Panel_SLNo2003!=0, !is.na(Panel_SLNo2003)) %>% 
  group_by(Panel_SLNo2003) %>% mutate(dups=max(row_number())) %>% 
  filter(dups<2) %>% select(-dups) # filtering out serial of panel HH in 2003


HH_2003<-readxl::read_excel("./2003/MAINHOUSEHOLDS.xlsx")  
eeptools::isid(HH_2003, vars = c("slno"))

HH_2003<-left_join(p_2003, HH_2003, by=c("Panel_SLNo2003" = "slno"))#filtering 2003 panel HH using 2018 info
eeptools::isid(HH_2003, vars = c("Panel_SLNo2003")) 

rm(p_2003)


#cleaning Iden of 2003
Iden<-HH_2003 %>% ungroup() %>% 
  rename(
    FSUNo = pan,
    TalukCode = taluk,
    DistrictCode = district,
    REM= rem,
    ROM = rom,
    EMI = emi,
    OMI = omi,
    THHM = HHLDType,
  ) %>% select(ScheduleNo, FSUNo, DistrictCode, TalukCode, REM, ROM, EMI, OMI, THHM)

eeptools::isid(HH_2003, vars = c("ScheduleNo")) 


HH_2003<-HH_2003 %>% select(1:2) %>% ungroup()



#Cleaning Individual of 2003

Individual<-readxl::read_excel("./2003/BLOCK2INDIVIDUALS.xlsx") %>% 
  select(-2:-6, -10, -17) %>% 
  rename(Panel_SLNo2003 = slno,
         Person_Sl_No_2003 = b1,
         Relation_HH = b3,
         Gender = b4,
         DoB_year = b5yr,
         Education = b6,
         Economic_act = b7,
         Marital_status = b8,
         Women_left = b9,
         Age = age) %>% 
  left_join(HH_2003, .)


eeptools::isid(Individual, vars = c("ScheduleNo", "Person_Sl_No_2003"))




#Cleaning REM_ROM of 2003 

REM_ROM<-readxl::read_excel("./2003/BLOCK3REMROM.xlsx") %>% 
  select(-2:-12,-14:-21, -27, -29, -30) %>% 
  rename(Person_Sl_No_2003 = b1,
         Panel_SLNo2003 = slno,
         Mig_date_REM_ROM = b12yr,
         First_dest_country_state_REM_ROM = b13,
         Pre_mig_Economic_act_REM_ROM = b14,
         Country_state_REM_ROM = b15,
         REM_ROM = b16,
         Return_date_REM_ROM = b17y) %>% mutate(REM_ROM = case_when(
           REM_ROM==1 ~ 0, #coding 1 as 2 i.e ROM
           REM_ROM==2 ~ 1  #coding 2 as 1 i.e REM
         ))



REM_ROM<-REM_ROM %>% group_by(Panel_SLNo2003, Person_Sl_No_2003) %>% mutate(dups=row_number()) %>% 
  filter(dups!=2) %>% select(-dups) %>% ungroup()# filtering repetition of 3 individuals in HH 15325


eeptools::isid(REM_ROM, vars = c("Panel_SLNo2003", "Person_Sl_No_2003"))



#Cleaning EMI_OMI of 2003

EMI_OMI<-readxl::read_excel("./2003/BLOCK4EMIOMI.xlsx") %>% 
select(-2:-6, -10, -12, -19, -20) %>% 
  rename(Panel_SLNo2003 = slno,
         Relation_HH = b19,
         Gender = b20,
         Country_state_EMI_OMI = b21,
         EMI_OMI = b22,
         First_visit_yr_EMI_OMI =b23y,
         First_vist_age_EMI_OMI = b24,
         Pre_mig_education_EMI_OMI = b25,
         Pre_mig_marital_status_EMI_OMI = b26,
         Pre_mig_economic_act_EMI_OMI = b27,
         Economic_act = b28) %>% 
  mutate(EMI_OMI = case_when(
    EMI_OMI==1 ~ 0, #coding 1 as 2 i.e OMI
    EMI_OMI==2 ~ 1  #coding 2 as 1 i.e EMI
  ))


eeptools::isid(EMI_OMI, vars = c("Panel_SLNo2003", "Person_Sl_No_2003"))


#Cleaning HH Details of 2003
HH_Details<-readxl::read_excel("./2003/MAINHOUSEHOLDS.xlsx") %>% 
  select(-2:-37, -51:-58) %>% 
  rename(Panel_SLNo2003 = slno,
         HH_electricity = b35,
         Cooking_fuel_used = b36,
         House_type = b37,
         
         Motor_car = b381,
         Taxi_truck_lorry = b382,
         Scooter = b383,
         Telephone = b384,
         Mobile_phone = b385,
         TV = b386,
         DVD_player = b387,
         Refrigerator = b388,
         
         Religion = b39, 
         Hindu_caste = b40)


eeptools::isid(HH_Details, vars = c("Panel_SLNo2003"))

#Cleaning Remittances in 2003

Remittances<-readxl::read_excel("./2003/MAINHOUSEHOLDS.xlsx") %>% 
  select(-2:-23, -26, -28, -29, -33, -37:-58, ) %>% 
  rename(Panel_SLNo2003 = slno,
         Receive_remit = b29,
         Amount_EMI_remit = b30,
         Gift_value_EMI_remit = b31,
         Build_buy_HH_EMI_remit = b331,
         Buy_land_EMI_remit = b332,
         Repair_renew_HH_EMI_remit = b333,
         Expand_new_business_EMI_remit = b342,
         Other_remitances_EMI_remit = b343,
         Automobile_buy_EMI_remit = b341) 


eeptools::isid(Remittances, vars = c("Panel_SLNo2003"))

Cross_2003<-left_join(Iden, Individual) %>% 
  left_join(., HH_Details) %>% 
  left_join(., REM_ROM) %>% 
  left_join(., Remittances) %>% 
  mutate(year = 2003)

eeptools::isid(Cross_2003, vars = c("ScheduleNo", "Person_Sl_No_2003"))

rm(REM_ROM, Remittances, Individual, Iden, HH_2003, HH_Details, EMI_OMI)

save(Cross_2003, file = "./R Data/ 2003 Cross.Rdata")


# Cleaning Selected 1998 Mother Blocks ------------------------------------


#Cleaning Iden of 1998

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

Iden<-HH_1998 %>% select(1:2, 5, 14, 21:25, 73) %>%  
  rename(FSUNo = PAN,
         THHM = HHLDType,
         DistrictCode = DISTRICT,
         TalukCode = TALUK)

eeptools::isid(Iden, vars = c("ScheduleNo"))

rm(p_1998)

#cleaning Remittances block 9 of 1998

Remittances<-HH_1998 %>% select(1:2, 26, 27, 30) %>% 
  rename(Receive_remit = A33,
         Amount_EMI_remit = A34,
         Gift_value_EMI_remit = A35_TOT)

eeptools::isid(Remittances, vars = c("ScheduleNo"))


#Cleaning HH_details block 10 of 1998

HH_Details<-HH_1998 %>% select(1:2, 36:72) %>% 
  rename(HH_electricity = A37,
         Cooking_fuel_used = A38,
         
         House_type = A45,
         Total_land_owned = A46_TOT,
         
         Motor_car = A47_1,
         Taxi_truck_lorry = A47_2,
         Scooter = A47_4,
         Telephone = A47_5,
         TV = A47_6,
         DVD_player = A47_7,
         Refrigerator = A47_12,
         
         Religion = A48) %>% 
  select(-A39:-A44, -A46_WET, -A46_DRY, -A47_3, -A47_8:-A47_11, -27:-38)

eeptools::isid(HH_Details, vars = c("ScheduleNo"))


#Cleaning EMI and OMI of 1998

HH_1998<-HH_1998 %>% select(1:2) %>% ungroup()


EMI<-readxl::read_excel("./1998/EMIGRANTS.xlsx") %>% 
  select(3:27) %>% 
  rename(Panel_SLNo1998 = QNO,
         Person_Sl_No_1998=SNO,
         Relation_HH = A19,
         Gender = A20,
         Country_state_EMI_OMI = A21, 
         Reason_migration_EMI_OMI = A22,
         First_visit_yr_EMI_OMI = A23YR,
         First_vist_age_EMI_OMI = A24,
         Pre_mig_education_EMI_OMI = A25A,
         Pre_mig_marital_status_EMI_OMI = A26,
         Pre_mig_economic_act_EMI_OMI = A27,
         Economic_act = A28,
         
         Air_ticket_EMI = TICKET,
         Visa_exp_EMI = VISA,
         Paid_agent_EMI = AGENT,
         Lost_to_fraud_EMI = CHEAT,
         Total_emig_exp_EMI = TOTAL,
         
         Personal_savings_emig_fin_EMI = SAVING,
         Family_members_emig_fin_EMI = RELATIVE,
         Bank_emig_fin_EMI = LOAN,
         Sell_mortgage_emig_fin_EMI = LAND,
         Other_sources_emig_fin_EMI = OTHER) %>%
  mutate(EMI_OMI = 1,
         First_visit_yr_EMI_OMI = as.double(First_visit_yr_EMI_OMI)) %>% 
  select(-A23MN, -GOLD, -A25B)



OMI<-readxl::read_excel("./1998/OUTMIGRANTS.xlsx") %>% 
  select(3:16) %>% 
  rename(Panel_SLNo1998 = QNO,
         Person_Sl_No_1998=SNO,
         Relation_HH = A19,
         Gender = A20,
         Country_state_EMI_OMI = A21, 
         Reason_migration_EMI_OMI = A22,
         First_visit_yr_EMI_OMI = A23YR,
         First_vist_age_EMI_OMI = A24,
         Pre_mig_education_EMI_OMI = A25A,
         Pre_mig_marital_status_EMI_OMI = A26,
         Pre_mig_economic_act_EMI_OMI = A27,
         Economic_act = A28) %>%
  mutate(EMI_OMI = 0) %>% 
  select(-A23MN, -A25B)


EMI_OMI<-bind_rows(EMI, OMI) %>% 
  mutate(Panel_SLNo1998= as.double(Panel_SLNo1998))
  
rm(EMI, OMI)

#EMI_OMI<-left_join(HH_1998, EMI_OMI)

eeptools::isid(EMI_OMI, vars = c("Panel_SLNo1998", "Person_Sl_No_1998"))


#cleaning of Individuals in 1998

Individual<-readxl::read_excel("./1998/INDIVIDUALS.xlsx") %>% 
  select(3:12) %>% 
  rename( 
    Panel_SLNo1998 = QNO,
    Person_Sl_No_1998=A1,
    Gender = A4,
    DoB_year = A5YR,
    Relation_HH = A3,
    Education = A7,
    Economic_act = A8,
    Marital_status = A9) %>% 
  select(-A5, -A6) %>% mutate(Panel_SLNo1998 = as.double(Panel_SLNo1998)) %>% 
  left_join(HH_1998, .)

eeptools::isid(Individual, vars = c("ScheduleNo", "Person_Sl_No_1998"))


#Cleaning REM and ROM in 1998

REM<-readxl::read_excel("./1998/RETURNEMIGRANTS.xlsx") %>% 
  select(2, 4, -5:-12, 13:29) %>% 
  rename(Person_Sl_No_1998 = A1,
         Panel_SLNo1998 = QNO,
         Country_state_REM_ROM = A13,
         In_out_India_REM_ROM = A14,
         Return_date_REM_ROM = A15YR,
         
         Air_ticket_REM_ROM = TICKET,
         Visa_exp_REM_ROM = VISA,
         Paid_agent_REM_ROM = AGENT,
         Lost_to_fraud_REM_ROM = CHEAT,
         Total_emig_exp_REM_ROM = TOTAL,
         
         Personal_savings_emig_fin_REM_ROM = SAVING,
         Family_members_emig_fin_REM_ROM = RELATIVE,
         Bank_emig_fin_REM_ROM = LOAN,
         Sell_mortgage_emig_fin_REM_ROM = LAND,
         Other_sources_emig_fin_REM_ROM = OTHER
         
  ) %>% select(-A15MN, -A16, -A17, -GOLD) %>% 
  mutate(Panel_SLNo1998= as.double(Panel_SLNo1998))

ROM<-readxl::read_excel("./1998/RETURNOUTMIGRANTS.xlsx") %>% 
  select(3, 4, -5:-12, 13:16) %>% 
  rename(Person_Sl_No_1998 = A1,
         Panel_SLNo1998 = QNO,
         Country_state_REM_ROM = A13,
         In_out_India_REM_ROM = A14,
         Return_date_REM_ROM = A15YR) %>% select(-A15MN) %>% 
  mutate(Panel_SLNo1998= as.double(Panel_SLNo1998))

REM_ROM<-bind_rows(REM, ROM) 


rm(REM, ROM)


#REM_ROM<- left_join(HH_1998, REM_ROM)

eeptools::isid(REM_ROM, vars = c("Panel_SLNo1998", "Person_Sl_No_1998"))


rm(HH_1998)


Cross_1998<-left_join(Iden, Individual) %>% 
  left_join(., HH_Details) %>% 
  left_join(., EMI_OMI) %>% 
  left_join(., REM_ROM) %>% 
  left_join(., Remittances) %>% 
  mutate(year=1998) %>% ungroup()

rm(EMI_OMI, REM_ROM, Iden, Individual, HH_Details, Remittances)


save(Cross_1998, file = "./R Data/1998 Cross.Rdata")



# Panel Setting -----------------------------------------------------------





Cross_2018<-Cross_2018 %>% mutate(THHM = case_when(
  EMI>0 ~ "EMI",
  REM>0 ~ "REM",
  OMI>0 ~ "OMI",
  ROM>0 ~ "ROM",
  EMI==0 & REM==0 & OMI==0 & ROM==0 ~ "NON MIGRANT",
),
Christ_denom = as.double(Christ_denom),
Return_date_REM_ROM = as.double(Return_date_REM_ROM)) %>% 
  mutate(Person_Sl_No = as.character(Person_Sl_No_2018)) %>% 
  select(-Person_Sl_No_2018)



Cross_2013<-Cross_2013 %>% mutate(Total_land_owned = as.double(Total_land_owned)) %>% 
  mutate(Person_Sl_No = as.character(Person_Sl_No_2013)) %>% select(-Person_Sl_No_2013)

Cross_2008<-Cross_2008 %>% mutate(across(c(Relation_HH, Gender, Education, Economic_act, 
                                           Marital_status:Muslim_denom, -Hindu_caste_spec, -Christ_denom_spec,
                                           Relation_HH_EMI_OMI: Receive_remit, -Pre_mig_emp_status_REM_ROM,
                                           In_out_India_REM_ROM, Country_state_REM_ROM, First_dest_country_state_REM_ROM,
                                           Pre_mig_Economic_act_REM_ROM), ~ 
                                           as.double(.))) %>% 
  mutate(Person_Sl_No = as.character(Person_Sl_No_2008)) %>% 
  select(-Post_mig_emp_stat_EMI_OMI, -Emp_status, -Person_Sl_No_2008) 



Cross_2003<-Cross_2003 %>% mutate(Panel_SLNo2003 = as.character(Panel_SLNo2003),
                                  Person_Sl_No = as.character(Person_Sl_No_2003)) %>%
  rename(Country_state_REM_ROM2 = Country_state_REM_ROM) %>% 
  mutate(Country_state_REM_ROM = case_when(
    Country_state_REM_ROM2=="UNITED ARAB EMIRATES" ~ 1,
    Country_state_REM_ROM2=="SAUDI ARABIA" ~ 2,
    Country_state_REM_ROM2=="OMAN" ~ 3,
    Country_state_REM_ROM2=="QATAR" ~ 4,
    Country_state_REM_ROM2=="KUWAIT" ~ 5,
    Country_state_REM_ROM2=="BAHRAIN" ~ 6,
    Country_state_REM_ROM2=="AUSTRALIA" ~ 7,
    Country_state_REM_ROM2=="BANGLADESH" ~ 8,
    Country_state_REM_ROM2=="CANADA" ~ 9,
    Country_state_REM_ROM2=="CHINA" ~ 10,
    Country_state_REM_ROM2=="FRANCE" ~ 11,
    Country_state_REM_ROM2=="GERMANY" ~ 12,
    Country_state_REM_ROM2=="HONG KONG" ~ 13,
    Country_state_REM_ROM2=="HUNGARY" ~ 14,
    Country_state_REM_ROM2=="INDONESIA" ~ 15,
    Country_state_REM_ROM2=="IRAN" ~ 16,
    Country_state_REM_ROM2=="IRAQ" ~ 17,
    Country_state_REM_ROM2=="IRELAND" ~ 18,
    Country_state_REM_ROM2=="ITALY" ~ 19,
    Country_state_REM_ROM2=="JAPAN" ~ 20,
    Country_state_REM_ROM2=="KENYA" ~ 21,
    Country_state_REM_ROM2=="LIBYA" ~ 22,
    Country_state_REM_ROM2=="MALAYSIA" ~ 23,
    Country_state_REM_ROM2=="MALDIVES" ~ 24,
    Country_state_REM_ROM2=="MYANMAR (BURMA)" ~ 25,
    Country_state_REM_ROM2=="NEPAL" ~ 26,
    Country_state_REM_ROM2=="NETHERLANDS" ~ 27,
    Country_state_REM_ROM2=="NEW ZEALAND" ~ 28,
    Country_state_REM_ROM2=="NIGERIA" ~ 29,
    Country_state_REM_ROM2=="PAKISTAN" ~ 30,
    Country_state_REM_ROM2=="PHILIPPINES" ~ 31,
    Country_state_REM_ROM2=="SINGAPORE" ~ 32,
    Country_state_REM_ROM2=="SOUTH AFRICA" ~ 33,
    Country_state_REM_ROM2=="SRI LANKA" ~ 34,
    Country_state_REM_ROM2=="SWITZERLAND" ~ 35,
    Country_state_REM_ROM2=="TAIWAN" ~ 36,
    Country_state_REM_ROM2=="THAILAND" ~ 37,
    Country_state_REM_ROM2=="UNITED KINGDOM " ~ 38,
    Country_state_REM_ROM2=="UNITED STATES OF AMERICA " ~ 39,
    Country_state_REM_ROM2=="WEST INDIES" ~ 40,
    Country_state_REM_ROM2=="YEMEN" ~ 41,
    Country_state_REM_ROM2=="OTHERS" ~ 42,
    Country_state_REM_ROM2=="ANDAMAN NICOBAR" ~ 43,
    Country_state_REM_ROM2=="ANDHRA PRADESH" ~ 44,
    Country_state_REM_ROM2=="ARUNANCHAL PRADESH" ~ 45,
    Country_state_REM_ROM2=="ASSAM" ~ 46,
    Country_state_REM_ROM2=="BIHAR" ~ 47,
    Country_state_REM_ROM2=="CHANDIGARH" ~ 48,
    Country_state_REM_ROM2=="CHATTISGARH" ~ 49,
    Country_state_REM_ROM2=="DADAR & NAGAR HAVELI" ~ 50,
    Country_state_REM_ROM2=="DAMAN & DIU" ~ 51,
    Country_state_REM_ROM2=="GOA" ~ 52,
    Country_state_REM_ROM2=="GUJARAT" ~ 53,
    Country_state_REM_ROM2=="HARIYANA" ~ 54,
    Country_state_REM_ROM2=="HIMACHAL PRADESH" ~ 55,
    Country_state_REM_ROM2=="JAMMU & KASHMIR" ~ 56,
    Country_state_REM_ROM2=="JHARKHAND" ~ 57,
    Country_state_REM_ROM2=="KARNATAKA" ~ 58,
    Country_state_REM_ROM2=="KERALA" ~ 59,
    Country_state_REM_ROM2=="LEKSHADWEEP" ~ 60,
    Country_state_REM_ROM2=="MADHYA PRADESH" ~ 61,
    Country_state_REM_ROM2=="MAHARASHTRA" ~ 62,
    Country_state_REM_ROM2=="MANIPUR" ~ 63,
    Country_state_REM_ROM2=="MEGHALAYA" ~ 64,
    Country_state_REM_ROM2=="MIZORAM" ~ 65,
    Country_state_REM_ROM2=="NAGALAND" ~ 66,
    Country_state_REM_ROM2=="NEW DELHI" ~ 67,
    Country_state_REM_ROM2=="ORISSA" ~ 68,
    Country_state_REM_ROM2=="PONDICHERRY" ~ 69,
    Country_state_REM_ROM2=="PUNJAB" ~ 70,
    Country_state_REM_ROM2=="RAJASTHAN" ~ 71,
    Country_state_REM_ROM2=="SIKKIM" ~ 72,
    Country_state_REM_ROM2=="TAMIL NADU" ~ 73,
    Country_state_REM_ROM2=="THRIPURA" ~ 74,
    Country_state_REM_ROM2=="UTTAR PRADESH" ~ 75,
    Country_state_REM_ROM2=="UTTARANCHAL" ~ 76,
    Country_state_REM_ROM2=="WEST BENGAL" ~ 77,
    Country_state_REM_ROM2=="MADURAI" ~ 73,
    Country_state_REM_ROM2=="ANDAMAN" ~ 43,
    Country_state_REM_ROM2=="UAE" ~ 1,
    Country_state_REM_ROM2=="ENGLAND" ~ 38)) %>% 
  
  mutate(First_dest_country_state_REM_ROM = case_when(
    First_dest_country_state_REM_ROM=="UNITED ARAB EMIRATES" ~ 1,
    First_dest_country_state_REM_ROM=="SAUDI ARABIA" ~ 2,
    First_dest_country_state_REM_ROM=="OMAN" ~ 3,
    First_dest_country_state_REM_ROM=="QATAR" ~ 4,
    First_dest_country_state_REM_ROM=="KUWAIT" ~ 5,
    First_dest_country_state_REM_ROM=="BAHRAIN" ~ 6,
    First_dest_country_state_REM_ROM=="AUSTRALIA" ~ 7,
    First_dest_country_state_REM_ROM=="BANGLADESH" ~ 8,
    First_dest_country_state_REM_ROM=="CANADA" ~ 9,
    First_dest_country_state_REM_ROM=="CHINA" ~ 10,
    First_dest_country_state_REM_ROM=="FRANCE" ~ 11,
    First_dest_country_state_REM_ROM=="GERMANY" ~ 12,
    First_dest_country_state_REM_ROM=="HONG KONG" ~ 13,
    First_dest_country_state_REM_ROM=="HUNGARY" ~ 14,
    First_dest_country_state_REM_ROM=="INDONESIA" ~ 15,
    First_dest_country_state_REM_ROM=="IRAN" ~ 16,
    First_dest_country_state_REM_ROM=="IRAQ" ~ 17,
    First_dest_country_state_REM_ROM=="IRELAND" ~ 18,
    First_dest_country_state_REM_ROM=="ITALY" ~ 19,
    First_dest_country_state_REM_ROM=="JAPAN" ~ 20,
    First_dest_country_state_REM_ROM=="KENYA" ~ 21,
    First_dest_country_state_REM_ROM=="LIBYA" ~ 22,
    First_dest_country_state_REM_ROM=="MALAYSIA" ~ 23,
    First_dest_country_state_REM_ROM=="MALDIVES" ~ 24,
    First_dest_country_state_REM_ROM=="MYANMAR (BURMA)" ~ 25,
    First_dest_country_state_REM_ROM=="NEPAL" ~ 26,
    First_dest_country_state_REM_ROM=="NETHERLANDS" ~ 27,
    First_dest_country_state_REM_ROM=="NEW ZEALAND" ~ 28,
    First_dest_country_state_REM_ROM=="NIGERIA" ~ 29,
    First_dest_country_state_REM_ROM=="PAKISTAN" ~ 30,
    First_dest_country_state_REM_ROM=="PHILIPPINES" ~ 31,
    First_dest_country_state_REM_ROM=="SINGAPORE" ~ 32,
    First_dest_country_state_REM_ROM=="SOUTH AFRICA" ~ 33,
    First_dest_country_state_REM_ROM=="SRI LANKA" ~ 34,
    First_dest_country_state_REM_ROM=="SWITZERLAND" ~ 35,
    First_dest_country_state_REM_ROM=="TAIWAN" ~ 36,
    First_dest_country_state_REM_ROM=="THAILAND" ~ 37,
    First_dest_country_state_REM_ROM=="UNITED KINGDOM " ~ 38,
    First_dest_country_state_REM_ROM=="UNITED STATES OF AMERICA " ~ 39,
    First_dest_country_state_REM_ROM=="WEST INDIES" ~ 40,
    First_dest_country_state_REM_ROM=="YEMEN" ~ 41,
    First_dest_country_state_REM_ROM=="OTHERS" ~ 42,
    First_dest_country_state_REM_ROM=="ANDAMAN NICOBAR" ~ 43,
    First_dest_country_state_REM_ROM=="ANDHRA PRADESH" ~ 44,
    First_dest_country_state_REM_ROM=="ARUNANCHAL PRADESH" ~ 45,
    First_dest_country_state_REM_ROM=="ASSAM" ~ 46,
    First_dest_country_state_REM_ROM=="BIHAR" ~ 47,
    First_dest_country_state_REM_ROM=="CHANDIGARH" ~ 48,
    First_dest_country_state_REM_ROM=="CHATTISGARH" ~ 49,
    First_dest_country_state_REM_ROM=="DADAR & NAGAR HAVELI" ~ 50,
    First_dest_country_state_REM_ROM=="DAMAN & DIU" ~ 51,
    First_dest_country_state_REM_ROM=="GOA" ~ 52,
    First_dest_country_state_REM_ROM=="GUJARAT" ~ 53,
    First_dest_country_state_REM_ROM=="HARIYANA" ~ 54,
    First_dest_country_state_REM_ROM=="HIMACHAL PRADESH" ~ 55,
    First_dest_country_state_REM_ROM=="JAMMU & KASHMIR" ~ 56,
    First_dest_country_state_REM_ROM=="JHARKHAND" ~ 57,
    First_dest_country_state_REM_ROM=="KARNATAKA" ~ 58,
    First_dest_country_state_REM_ROM=="KERALA" ~ 59,
    First_dest_country_state_REM_ROM=="LEKSHADWEEP" ~ 60,
    First_dest_country_state_REM_ROM=="MADHYA PRADESH" ~ 61,
    First_dest_country_state_REM_ROM=="MAHARASHTRA" ~ 62,
    First_dest_country_state_REM_ROM=="MANIPUR" ~ 63,
    First_dest_country_state_REM_ROM=="MEGHALAYA" ~ 64,
    First_dest_country_state_REM_ROM=="MIZORAM" ~ 65,
    First_dest_country_state_REM_ROM=="NAGALAND" ~ 66,
    First_dest_country_state_REM_ROM=="NEW DELHI" ~ 67,
    First_dest_country_state_REM_ROM=="ORISSA" ~ 68,
    First_dest_country_state_REM_ROM=="PONDICHERRY" ~ 69,
    First_dest_country_state_REM_ROM=="PUNJAB" ~ 70,
    First_dest_country_state_REM_ROM=="RAJASTHAN" ~ 71,
    First_dest_country_state_REM_ROM=="SIKKIM" ~ 72,
    First_dest_country_state_REM_ROM=="TAMIL NADU" ~ 73,
    First_dest_country_state_REM_ROM=="THRIPURA" ~ 74,
    First_dest_country_state_REM_ROM=="UTTAR PRADESH" ~ 75,
    First_dest_country_state_REM_ROM=="UTTARANCHAL" ~ 76,
    First_dest_country_state_REM_ROM=="WEST BENGAL" ~ 77,
    First_dest_country_state_REM_ROM=="MADURAI" ~ 73,
    First_dest_country_state_REM_ROM=="ANDAMAN" ~ 43,
    First_dest_country_state_REM_ROM=="UAE" ~ 1,
    First_dest_country_state_REM_ROM=="ENGLAND" ~ 38
  )) %>% 
  mutate(Pre_mig_Economic_act_REM_ROM = as.double(Pre_mig_Economic_act_REM_ROM)) %>% 
  select(-Country_state_REM_ROM2, -Person_Sl_No_2003)



Cross_1998<-Cross_1998 %>% 
  mutate(Panel_SLNo1998 = as.character(Panel_SLNo1998),
         Person_Sl_No = as.character(Person_Sl_No_1998)) %>% 
  select(-Person_Sl_No_1998)



KMS_panel<-bind_rows(Cross_2018, Cross_2013, Cross_2008, Cross_2003, Cross_1998)


eeptools::isid(KMS_panel, vars =c("ScheduleNo", "Person_Sl_No", "year"))

rm(Cross_2018, Cross_2013, Cross_2008, Cross_2003, Cross_1998)


# Cleaning the Panel ------------------------------------------------------

#Removing all NA columns and other irrelevant columns

KMS_panel<-KMS_panel %>% select(-TalukNew, -Mig_yr_REM_ROM,-Disease_accident_abroad_name_REM_ROM,-Disease_accident_abroad_name_spec_REM_ROM,
                                -Treat_from_REM_ROM,-Work_related_disease_REM_ROM,-Nature_work_REM_ROM,-Destination_stud,-First_visit_yr_stud,
                                -Course_stud,-Course_duration_stud,-Scholarship_stud,-Scholarship_qamount_stud,-Capitation_fee_stud,-Tuition_fee_stud,
                                -Caution_deposit_stud,-Travel_cost_monthly_stud,-Rent_food_monthly_stud,-Other_costs_stud,-Total_cost_stud,
                                -Took_loan_stud,-Loan_amount_stud,-Bank_name_stud,-Interest_rate_stud,-Agent_involved_stud,-Agent_fee_stud, #Student Block ends here
                                -Edu_Institute_Attending,-House_ownership_type,-House_construct_cost,-Loan_for_house_construction,
                                -TV_type, -Religion_spec)


#Re-coding Gender to 0 & 1 as factor
KMS_panel<-KMS_panel %>% mutate(Gender= case_when(
  Gender==1 ~ 0,
  Gender==2 ~ 1
))

KMS_panel$Gender<-factor(KMS_panel$Gender, level=c("0", "1"), 
                         labels = c("Male", "Female"))

#Re-coding THHM for taking values REM, ROM, EMI, OMI and NON MIGRANT

KMS_panel<-KMS_panel %>% mutate(THHM = case_when(
  THHM=="NON MIIG" ~ "NON MIGRANT",
  THHM=="NONMIGRANT" ~ "NON MIGRANT",
  THHM=="OTHERS" ~ "NON MIGRANT",
  TRUE ~ THHM
))


KMS_panel<-KMS_panel %>% mutate(THHM = case_when(
  THHM=="EMI" ~ 1,
  THHM=="OMI" ~ 2,
  THHM=="REM" ~ 3,
  THHM=="ROM" ~ 4,
  THHM=="NON MIGRANT" ~ 5
))

KMS_panel$THHM<-factor(KMS_panel$THHM, level=c("1", "2", "3", "4", "5"), 
                       labels = c("EMI", "OMI", "REM", "ROM", "NON MIGRANT"))


#Re-coding Relation to HH head
KMS_panel<-KMS_panel %>% mutate(Relation_HH = case_when(
  year==2018 & Relation_HH==8 ~ 10, #making bro/sis as 10
  year==2018 & Relation_HH==9 ~ 8, #making servant to 8
  year==2018 & Relation_HH==10 ~ 9, #making others to 9,
  TRUE ~ Relation_HH
))

KMS_panel$Relation_HH<-factor(KMS_panel$Relation_HH, level=c("1", "2", "3", "4", "5", "6","7", "8", "9", "10"), 
                       labels = c("Head of the HH", "Husband/Wife", "Unmarried children", "Married children", "Son-in-law/Daughter-in-law ",
                                  "Grandchild", " Father/Mother/Mother-in-law", "Servant ", "Others", "Brother/ Sister"))


#Re-coding for Education of Individuals
KMS_panel<-KMS_panel %>% mutate(Education = case_when(
  year>2012 & Education==22 ~ 1, # illiterate to 1
  year>2012 & Education==23 ~ 2, # no formal schooling to 2
  year>2012 & Education==21 ~ 2, # others who only write to 2
  year>2012 & Education>=0 & Education < 8 ~ 3, #Primary
  year>2012 & Education>=8 & Education < 11 ~ 4, #secondary
  year>2012 & Education>=11 & Education < 14 ~ 5, #higher secondary
  year>2012 & Education>=14 & Education < 21 ~ 6, #Graduation and above
  
  year==2008 & Education==4 ~ 3, #primary to new primary code
  year==2008 & Education==5 ~ 4, #Secondary
  year==2008 & Education==6 ~ 5, #higher secondary
  year==2008 & Education==7 ~ 6, # graduation and above
  year==2008 & Education==8 ~ 7, #Others (only in 2008)
  
  year==2003 & Education==4 ~ 3, #primary to new primary code
  year==2003 & Education==5 ~ 4, #Secondary
  year==2003 & Education==6 ~ 5, #higher secondary
  year==2003 & Education==7 ~ 6, # graduation and above
  
  year==1998 & Education==7 ~ 3, #correcting primary not completed to primary
  
  Education>23 ~ NA_real_,
  Education==0 ~ NA_real_,
  TRUE ~ Education
))


KMS_panel$Education<-factor(KMS_panel$Education, level=c("1", "2", "3", "4", "5", "6","7"), 
                              labels = c("Illiterate", "Literate without school education", "Primary", "Secondary", "Higher Secondary",
                                         "Graduation and Above", "Others (only in 2008)"))

  
#Re-coding for Economic Act of Individuals
KMS_panel<-KMS_panel %>% mutate(Economic_act = case_when(
  year==2008 & Economic_act==13 ~ 17,
  TRUE ~ Economic_act
))


KMS_panel$Economic_act<-factor(KMS_panel$Economic_act, level=c(paste(1:17, sep = ",")), 
                            labels = c("Employed in State /Central Govt.", "Employed in Semi Govt. Aided school/college, co-operative /local admin bodies", 
                                       "Employed in Private Sector", "Self employment", "Unpaid family work",
                                       "Agricultural labour", "Labourers in non-agricultural sector", "Job seekers", "Job not required", "Students",
                                       "Household works", "Pensioners", "Too old to work", "Too young to work", "Disabled","NREGA", "Others"))


#Re-coding Marital Status 


KMS_panel$Marital_status<-factor(KMS_panel$Marital_status, level=c("1", "2", "3", "4", "5"), 
                            labels = c("Never married", "Married", "Widow/Widower", "Divorced", "Separated"))



#Re-coding Ration card
KMS_panel<-KMS_panel %>% mutate(Ration_card = case_when(
  Ration_card==1 ~ 1,
  Ration_card==2 ~ 0
))


KMS_panel$Ration_card<-factor(KMS_panel$Ration_card, level=c("0", "1"), 
                         labels = c("NO", "YES"))


#Re-coding Ration card color
KMS_panel<-KMS_panel %>% mutate(Ration_card_Color = case_when(
Ration_card_Color < 4 ~ 1,
Ration_card_Color==4 ~ 0
))

KMS_panel$Ration_card<-factor(KMS_panel$Ration_card, level=c("0", "1"), 
                              labels = c("APL-White Card", "BPL-Yellow, Pink, Blue Card"))

#Re-coding Cooking Fuel
KMS_panel<-KMS_panel %>% mutate(Cooking_fuel_used = case_when(
  year>=1998 & year<=2003 & Cooking_fuel_used == 4 ~ 5, #making other fuels as 5
  year>=1998 & year<=2003 & Cooking_fuel_used == 5 ~ 4, #making LPG as 4,
  TRUE ~ Cooking_fuel_used
))

KMS_panel$Cooking_fuel_used<-factor(KMS_panel$Cooking_fuel_used, level=c("1", "2", "3", "4", "5"), 
                              labels = c("Wood", "Electricity", "Kerosene", "LPG", "Others"))


#making House type to factor
KMS_panel$House_type<-factor(KMS_panel$House_type, level=c("1", "2", "3", "4", "5"), 
                                    labels = c("Luxurious (3 or more bedrooms with attached bathrooms, concrete/tile roof, tiled floor)", 
                                               "Very Good (2 bed rooms with attached bathrooms, concrete/tile roof, Mosaic floor)", 
                                               "Good (1 bed room, brick and cement walls, concrete or tile roof)", 
                                               "Poor (Brick walls, cement floor, tin or asbestos roof)", 
                                               "Kutcha (Mud walls, Mud floor &Thatched roof)"))

#Re-coding Other house owned
KMS_panel<-KMS_panel %>% mutate(Other_house_owned = case_when(
  year==2013 & Other_house_owned == 0 ~ NA_real_, #making zeros which represent no response as NA
  Other_house_owned==2 ~ 0, #making NO as 0
  TRUE ~ Other_house_owned
))

KMS_panel$Other_house_owned<-factor(KMS_panel$Other_house_owned, level=c("0", "1"), 
                                    labels = c("NO", "YES"))


#Re-coding Land Owned owned
KMS_panel<-KMS_panel %>% mutate(Land_owned = case_when(
 Land_owned == 2 ~ 0, #, #making NO as 0
  TRUE ~ Land_owned
))

KMS_panel$Land_owned<-factor(KMS_panel$Land_owned, level=c("0", "1"), 
                                    labels = c("NO", "YES"))

#Re-coding Land Owned owned elsewhere
KMS_panel<-KMS_panel %>% mutate(Land_owned_elsewhere = case_when(
  year==2013 & Land_owned_elsewhere ==0 ~ NA_real_,
  Land_owned_elsewhere == 2 ~ 0, #, #making NO as 0
  TRUE ~ Land_owned_elsewhere
))

KMS_panel$Land_owned_elsewhere<-factor(KMS_panel$Land_owned_elsewhere, level=c("0", "1"), 
                             labels = c("NO", "YES"))


#Re-coding Asstes as Dummies :)
KMS_panel<-KMS_panel %>% mutate(across(c(Motor_car:Inverter), ~ case_when(
  . ==0 ~ NA_real_,
  . == 2 ~ 0, #, #making NO as 0
  TRUE ~ .
)))

KMS_panel<-KMS_panel %>% mutate(across(c(Motor_car:Inverter), ~ factor(., level=c("0", "1"), 
                                       labels = c("NO", "YES"))))



#RE-Coding Religion
KMS_panel<-KMS_panel %>% mutate(Religion = case_when(
  year==1998 & Religion<=3 ~ 1, #castes in to hindu
  year==1998 & Religion==4 ~ 2, #Syrian Christians to 2
  year==1998 & Religion==5 ~ 2, #Latin Christians to 2
  year==1998 & Religion==6 ~ 3, #Shia Muslims to 3
  year==1998 & Religion==7 ~ 3, #Sunni Muslims to 3
  
  year==2008 & Religion==5 ~ 4, #others and sikhs clubbed together
  
  TRUE ~ Religion
))

#Re-coding castes 
KMS_panel<-KMS_panel %>% mutate(Hindu_caste = case_when(
  year==2008 & Hindu_caste==6 ~ 7, #making SC ST to 7 as in 2018
  year==2008 & Hindu_caste==7 ~ 9, #making others as 9
  
  year==2003 & Hindu_caste==1 ~ 7, #making SC ST to 7 as in 2018
  year==2003 & Hindu_caste==2 ~ 1, #making Nair to 1 as in 2018
  year==2003 & Hindu_caste==3 ~ 2, #making Ezhava to 2 as in 2018
  year==2003 & Hindu_caste==4 ~ 3, #making Brahmans to 3 as in 2018
  year==2003 & Hindu_caste==5 ~ 9, #making other hindus to 9 as in 2018
  
  year==1998 & Religion==1 ~ 7, #making SC ST to 7 as in 2018
  year==1998 & Religion==2 ~ 1, #making Nair to 1 as in 2018
  year==1998 & Religion==3 ~ 2, #making Ezhava to 2 as in 2018
  year==1998 & Religion==9 ~ 4, #making Nadar to 4 as in 2018
  
))

#Re-coding Christian denomination
KMS_panel<-KMS_panel %>% mutate(Hindu_caste = case_when(
  
))














# Saving Image ------------------------------------------------------------

save.image(file = "./R Data/KMS Panel.Rdata")
