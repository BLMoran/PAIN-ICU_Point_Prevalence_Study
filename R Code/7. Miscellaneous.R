## Part 9: Miscellaneous
library(broom)

### Part a. Pain Assessment in Patients That Received Opioids Vs Those that Did Not.

# Generate Variable of had opioid vs have not.
PPP_Assess_Opioid <- PPP2 %>% mutate(Opioid = case_when(
  (Opioid_oMEDD_Tot > 0) ~ "Yes",
  (Opioid_oMEDD_Tot == 0) ~ "No")) %>% 
  select(Opioid, mech_vent, pain_assessed)

#Ventilated 
OpioidAssessMV <- PPP_Assess_Opioid %>% filter(mech_vent == "Ventilated") %>% filter(pain_assessed == "Yes")
Cont.Table <- table(Opioid = OpioidAssessMV$Opioid, Pain_Assessed = OpioidAssessMV$pain_assessed)
Cont.Table
OpioidAssess1 <- tidy(prop.test(x = c(108, 31), n = c(139, 139))) %>% mutate(Comparator = "Assessment With/Without Opioids (Vent)") 

# Non-Ventilated Pts
OpioidAssessNonMV <- PPP_Assess_Opioid %>% filter(mech_vent == "Non-Ventilated") %>% filter(pain_assessed == "Yes")
Cont.Table2 <- table(Opioid = OpioidAssessNonMV$Opioid, Pain_Assessed = OpioidAssessNonMV$pain_assessed)
Cont.Table2
OpioidAssess2 <- tidy(prop.test(x = c(134, 130), n = c(264, 264))) %>% mutate(Comparator = "Assessment With/Without Opioids (Vent)") 

PainAssessOpioidComparison <- rbind(OpioidAssess1, OpioidAssess2) %>% select(Comparator, estimate1, estimate2, conf.low, conf.high, p.value) %>%  as.data.frame() %>% format(scientific=FALSE, digits = 1)
Cont.Table2 <- table(Opioid = OpioidAssessNonMV$Opioid, Pain_Assessed = OpioidAssessNonMV$pain_assessed)
rmarkdown::paged_table(PainAssessOpioidComparison)


### Part b. Table of Opioid Route by Ventilation Status

# Create Variables for Opioid Table
# Opioid IV (Binary)
PPP2 <- PPP2 %>% mutate(Opioid_IV = case_when(
  (OpioidIV_Tot_oMEDD >0) ~ "Yes",
  (OpioidIV_Tot_oMEDD ==0) ~ "No"))

# Opioid PO (Binary)
PPP2 <- PPP2 %>% mutate(Opioid_PO = case_when(
  (OpioidPO_Tot_oMEDD >0) ~ "Yes",
  (OpioidPO_Tot_oMEDD ==0) ~ "No"))

# Opioid Tota (Binary)
PPP2 <- PPP2 %>% mutate(Opioid_Tot = case_when(
  (Opioid_oMEDD_Tot >0) ~ "Yes",
  (Opioid_oMEDD_Tot ==0) ~ "No"))

# Create Table of Opioid Route
topioids <- PPP2 %>% 
  select(Opioid_IV, Opioid_PO, Opioid_Tot, mech_vent) %>% 
  tbl_summary(by = mech_vent, ,missing = "no",
              label = list(Opioid_IV = "Intravenous Opioid",
                           Opioid_PO = "Oral Opioid",
                           Opioid_Tot = "Any Opioid"),
              value = list(Opioid_IV ~ "Yes",
                           Opioid_PO ~ "Yes",
                           Opioid_Tot ~ "Yes"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  modify_header(list(
    label ~"**Analgesic Adjunct**")) %>% 
  bold_labels() %>% 
  add_overall() %>% 
  modify_caption("**Table 23. Opioid Administration Route**")

topioids
