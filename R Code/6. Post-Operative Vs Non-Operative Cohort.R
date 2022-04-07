## Pain Management- Post-Operative vs Non-Operative Cohort

### Part a: Demographics

# Change Name of Post-Op Responses
PPP <- PPP %>% mutate(postop = case_when(
  (postop == "No") ~ "Non-Operative",
  (postop == "Yes") ~ "Post-Operative"))

# Generate Demographic Table

PPP %>% 
  select(age, gender, apacheiiscore, apache3, admit_source, mech_vent, postop) %>% 
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>% 
      tbl_summary(by = postop,
                  missing = "no",
                  label = list(age="Age (Years)",
                               gender="Sex",
                               apacheiiscore="APACHE2 Score",
                               apache3="Admission Diagnosis",
                               admit_source="Admission Source"),
                  statistic = list(all_continuous() ~ "{mean} ({sd})",
                                   all_categorical() ~ "{n} ({p}%)"))  %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall() %>% 
      modify_caption("**Table 12. Patient Demographics in Post-Operative Vs Non-Operative**")
  )

### Part b: Pain Assessment Tools

# Generate Table of Assessment Tools in Post-Op Vs Non Post-Op Pts
PPP %>% 
  select(Assesstool, mech_vent, postop) %>% 
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>% 
      tbl_summary(by = postop, ,missing = "no",
                  label = list(Assesstool="Pain Assessment Tool"),
                  statistic = list(all_continuous() ~ "{mean} ({sd})",
                                   all_categorical() ~ "{n} ({p}%)")) %>%
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall()%>%
      modify_caption("**Table 13. Pain Assessment Tools in Post-Operative Vs Non-Operative**") 
  )

###   Part b.1. Comparison of Pain Assessment Tools in Post-Operative and Non-Operative Groups in Each Ventilation Strata  

library(broom)

# Omnibus Test for Differences in Assessment Tool Type Each Group (Ventilated and Non-Ventilated)
# Ventilated Pts
PPPVent <- PPP %>% filter(mech_vent == "Ventilated")
AssToolVent <- tidy(chisq.test(PPPVent$Assesstool, PPPVent$postop))  %>% mutate(Comparator = "Assessment Tool (Ventilated)")   

# Non-Ventilated Pts
PPPNonVent <- PPP %>% filter(mech_vent == "Non-Ventilated")
AssToolNonVent <- tidy(chisq.test(PPPNonVent$Assesstool, PPPNonVent$postop))  %>% mutate(Comparator = "Assessment Tool (Non-Vent)")    

TotalAssessToolComparison <- rbind(AssToolVent,AssToolNonVent) %>% select(Comparator, p.value)

rmarkdown::paged_table(TotalAssessToolComparison)

###   Part c: Pain Assessment Frequency

# Generate Table of Assessment Frequency in Post-Op Vs Non Post-Op Pts
PPP %>% 
  select(freq_assess, mech_vent, postop) %>% 
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>% 
      tbl_summary(by = postop, ,missing = "no",
                  label = list(freq_assess="Pain Assessment Frequency"),
                  statistic = list(all_continuous() ~ "{mean} ({sd})",
                                   all_categorical() ~ "{n} ({p}%)")) %>%
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall()%>%
      modify_caption("**Table 14. Pain Assessment Frequency in Post-Operative Vs Non-Operative**") 
  )

###   Part c.1. Comparison of Pain Assessment Frequency in Post-Operative and Non-Operative Groups in Each Ventilation Strata  

# Omnibus Test for Differences in Assessment Frequency Each Group (Ventilated and Non-Ventilated)
# Ventilated Pts
AssFreqVent <- tidy(chisq.test(PPPVent$freq_assess, PPPVent$postop))  %>% mutate(Comparator = "Assessment Freq (Ventilated)")   
AssFreqMV.Table <- table(Frequency = PPPVent$freq_assess, Op = PPPVent$postop)

# Non-Ventilated Pts
AssFreqNonVent <- tidy(chisq.test(PPPNonVent$freq_assess, PPPNonVent$postop))  %>% mutate(Comparator = "Assessment Freq (Non-Vent)")    
AssFreqNonMV.Table <- table(Frequency = PPPNonVent$freq_assess, Op = PPPNonVent$postop)

TotalAssessFreqComparison <- rbind(AssFreqVent,AssFreqNonVent) %>% select(Comparator, p.value)

rmarkdown::paged_table(TotalAssessFreqComparison)

###   Part d: Pain Management- Opioid Type

# Generate Table of Assessment Frequency in Post-Op Vs Non Post-Op Pts
PPP %>% 
  select(Opioidinf, OpioidPCA, OpioidPO,  mech_vent, postop) %>% 
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = postop, ,missing = "no",
                  label = list(Opioidinf="Type of Opioid Infusion",
                               OpioidPCA="Type of Opioid PCA",
                               OpioidPO="Type of Oral Opioid"),
                  statistic = list(all_continuous() ~ "{mean} ({sd})",
                                   all_categorical() ~ "{n} ({p}%)")) %>% 
      modify_header(list(
        label ~"**Opioid Route**")) %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall() %>% 
      modify_caption("**Table 15. Opioid Type and Route of Administration in Post-Operative Vs Non-Operative**")
  )

###   Part d.1. Comparison of Type of Opioid in Post-Operative and Non-Operative Groups in Each Ventilation Strata  

# Omnibus Test for Differences in Opioid Infusion Type in Each Group (Ventilated and Non-Ventilated)
# Opioid Infusion
# Ventilated Pts
OpioidInfTypeVent <- tidy(chisq.test(PPPVent$Opioidinf, PPPVent$postop))%>% mutate(Comparator = "Opioid Inf (Vent)")      

# Non-Ventilated Pts
OpioidInfTypeNonVent <- tidy(chisq.test(PPPNonVent$Opioidinf, PPPNonVent$postop))%>% mutate(Comparator = "Opioid Inf (Non-Vent)")     

# Opioid PCA
# Ventilated Pts
OpioidPCATypeVent <- tidy(chisq.test(PPPVent$OpioidPCA, PPPVent$postop))%>% mutate(Comparator = "Opioid PCA (Vent)")      

# Non-Ventilated Pts
OpioidPCATypeNonVent <- tidy(chisq.test(PPPNonVent$OpioidPCA, PPPNonVent$postop))%>% mutate(Comparator = "Opioid PCA (Non-Vent)")     

# PO Opioid
# Ventilated Pts
OpioidPOTypeVent <- tidy(chisq.test(PPPVent$OpioidPO, PPPVent$postop))%>% mutate(Comparator = "Oral Opioid (Vent)")      

# Non-Ventilated Pts
OpioidPOTypeNonVent <- tidy(chisq.test(PPPNonVent$OpioidPO, PPPNonVent$postop))%>% mutate(Comparator = "Oral Opioid (Non-Vent)")    

OpioidTypeComparison <- rbind(OpioidInfTypeVent, OpioidInfTypeNonVent, OpioidPCATypeVent, OpioidPCATypeNonVent, OpioidPOTypeVent, OpioidPOTypeNonVent) %>% select(Comparator, p.value) %>%  as.data.frame() %>% format(scientific=FALSE)

rmarkdown::paged_table(OpioidTypeComparison)

###   Part e: Pain Management- Cumulative Opioid Dose


PPP %>% 
  select(Morphinf_oMEDD, Fentinf_oMEDD, mech_vent, postop) %>% 
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = postop, missing = "no",
                  type = everything() ~ "continuous2",
                  statistic = all_continuous() ~ c("{mean} ({sd})",
                                                   "{median} ({p25}, {p75})"),
                  digits = all_continuous() ~ 1,
                  label = list(Morphinf_oMEDD = "Morphine Infusion",
                               Fentinf_oMEDD = "Fentanyl Infusion")) %>% 
      modify_header(list(
        label ~"**Opioid Type & Route**")) %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall() %>% 
      modify_caption("**Table 16. Cumulative Doses of Opioid Infusions in Post-Operative Vs Non-Operative**")
  )

###   Part e.1. Comparison of Dose of Opioid in Post-Operative and Non-Operative Groups in Each Ventilation Strata  

# Test for Differences in Opioid Infusion Type in Each Group (Ventilated and Non-Ventilated)
# Opioid Infusion- Morphine
# Ventilated Pts
Inf1 <- tidy(wilcox.test(PPPVent$Morphinf_oMEDD ~ PPPVent$postop, paired = FALSE, conf.int = TRUE)) %>% mutate(Comparator = "Morphine Infusion (Vent)") 

# Non-Ventilated Pts
Inf2 <- tidy(wilcox.test(PPPNonVent$Morphinf_oMEDD ~ PPPNonVent$postop, paired = FALSE, conf.int = TRUE)) %>% mutate(Comparator = "Morphine Infusion (Non-Vent)") 

# Opioid Infusion- Fentanyl
# Ventilated Pts
Inf3 <- tidy(wilcox.test(PPPVent$Fentinf_oMEDD ~ PPPVent$postop, paired = FALSE, conf.int = TRUE)) %>% mutate(Comparator = "Fentanyl Infusion (Vent)")  

# Non-Ventilated Pts
Inf4 <- tidy(wilcox.test(PPPNonVent$Fentinf_oMEDD ~ PPPNonVent$postop, paired = FALSE, conf.int = TRUE)) %>% mutate(Comparator = "Fentanyl Infusion (Non-Vent)")   

TableInf <- rbind(Inf1, Inf2, Inf3, Inf4) %>% select(Comparator, p.value, conf.low, conf.high)

rmarkdown::paged_table(TableInf)

PPP %>% 
  select(MorphPCA_oMEDD, FentPCA_oMEDD, HydroPCA_oMEDD, OxyPCA_oMEDD, mech_vent, postop) %>% 
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = postop, missing = "no",
                  type = everything() ~ "continuous2",
                  statistic = all_continuous() ~ c("{mean} ({sd})",
                                                   "{median} ({p25}, {p75})"),
                  digits = all_continuous() ~ 1,
                  label = list(MorphPCA_oMEDD = "Morphine PCA",
                               FentPCA_oMEDD = "Fentanyl PCA",
                               HydroPCA_oMEDD = "Hydromorphone PCA",
                               OxyPCA_oMEDD = "Oxycodone PCA")) %>% 
      modify_header(list(
        label ~"**Opioid Type & Route**")) %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall() %>% 
      modify_caption("**Table 17. Cumulative Doses of Opioid PCAs in Post-Operative Vs Non-Operative**")
  )

PPP %>% 
  select(OxycodPO_oMEDD, MorphPO_oMEDD, TapentPO_oMEDD, TramPO_oMEDD, BupPO_oMEDD, CodPO_oMEDD, mech_vent, postop) %>% 
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = postop, missing = "no",
                  type = everything() ~ "continuous2",
                  statistic = all_continuous() ~ c("{mean} ({sd})",
                                                   "{median} ({p25}, {p75})"),
                  digits = all_continuous() ~ 1,
                  label = list(OxycodPO_oMEDD = "Oxycodone",
                               TramPO_oMEDD = "Tramadol",
                               TapentPO_oMEDD = "Tapentadol",
                               BupPO_oMEDD = "Buprenorphine",
                               MorphPO_oMEDD = "Morphine",
                               CodPO_oMEDD = "Codeine")) %>%
      modify_header(list(
        label ~"**Opioid Type**")) %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall() %>%
      modify_caption("**Table 18. Cumulative Doses of Oral Opioid in Post-Operative Vs Non-Operative**")
  )

### Part f: Pain Management- Cumulative Total Opioid Dose

# Generate table of total oMEDD 
tIVPostOp <-
  PPP_IV %>%
  select(oMEDD, mech_vent, postop) %>%
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = postop, missing = "no",
                  type = everything() ~ "continuous2",
                  statistic = all_continuous() ~ c("{mean} ({sd})",
                                                   "{median} ({p25}, {p75})"),
                  digits = all_continuous() ~ 1,
                  label = list(oMEDD = "Parenteral Opioid")) %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall()
  )

t2POPostOp <-
  PPP_PO %>%
  select(oMEDD, mech_vent, postop) %>%
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = postop, missing = "no",
                  type = everything() ~ "continuous2",
                  statistic = all_continuous() ~ c("{mean} ({sd})",
                                                   "{median} ({p25}, {p75})"),
                  digits = all_continuous() ~ 1,
                  label = list(oMEDD = "Oral Opioid")) %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall()
  )

t3TotPostOp <-
  PPP_Tot %>%
  select(oMEDD, mech_vent, postop) %>%
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = postop, missing = "no",
                  type = everything() ~ "continuous2",
                  statistic = all_continuous() ~ c("{mean} ({sd})",
                                                   "{median} ({p25}, {p75})"),
                  digits = all_continuous() ~ 1,
                  label = list(oMEDD = "Total Opioid")) %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall() 
  )

tbl_stack(list(tIVPostOp, t2POPostOp, t3TotPostOp)) %>% 
  modify_header(list(
    label ~"**Opioid Route**"))%>% 
  modify_caption("**Table 19. Cumulative Doses of Opioids in Post-Operative Vs Non-Operative**")

### Part f.1. Comparison of Dose of Opioid in Post-Operative and Non-Operative Groups in Each Ventilation Strata

# Test for Differences in Opioid Infusion Type in Each Group (Ventilated and Non-Ventilated)
# Parenteral Opioid
# Ventilated Pts
PPPIVVent <- PPP_IV %>% filter(mech_vent == "Ventilated")
PPPIVNonVent <- PPP_IV %>% filter(mech_vent == "Non-Ventilated")

Par1 <- tidy(wilcox.test(PPPIVVent$oMEDD ~ PPPIVVent$postop, paired = FALSE, conf.int = TRUE)) %>% mutate(Comparator = "Parenteral Opioid (Vent)") 

# Non-Ventilated Pts
Par2 <- tidy(wilcox.test(PPPIVNonVent$oMEDD ~ PPPIVNonVent$postop, paired = FALSE, conf.int = TRUE)) %>% mutate(Comparator = "Parenteral Opioid (Non-Vent)") 

# Oral Opioid
# Ventilated Pts
PPPPOVent <- PPP_PO %>% filter(mech_vent == "Ventilated")
PPPPONonVent <- PPP_PO %>% filter(mech_vent == "Non-Ventilated")

Or1 <- tidy(wilcox.test(PPPPOVent$oMEDD ~ PPPPOVent$postop, paired = FALSE, conf.int = TRUE)) %>% mutate(Comparator = "Oral Opioid (Vent)")  

# Non-Ventilated Pts
Or2 <- tidy(wilcox.test(PPPPONonVent$oMEDD ~ PPPPONonVent$postop, paired = FALSE, conf.int = TRUE)) %>% mutate(Comparator = "Oral Opioid (Non-Vent)")  

# Total Opioid
# Ventilated Pts
PPPTotVent <- PPP_Tot %>% filter(mech_vent == "Ventilated")
PPPTotNonVent <- PPP_Tot %>% filter(mech_vent == "Non-Ventilated")
Tot1 <- tidy(wilcox.test(PPPTotVent$oMEDD ~ PPPTotVent$postop, paired = FALSE, conf.int = TRUE)) %>% mutate(Comparator = "Total Opioid (Vent)")  

# Non-Ventilated Pts
Tot2 <- tidy(wilcox.test(PPPTotNonVent$oMEDD ~ PPPTotNonVent$postop, paired = FALSE, conf.int = TRUE)) %>% mutate(Comparator = "Total Opioid (Non-Vent)") 

TotalOpioidComparison <- rbind(Par1, Par2, Or1, Or2, Tot1, Tot2) %>% select(Comparator, p.value, conf.low, conf.high)

rmarkdown::paged_table(TotalOpioidComparison)

### Part g: Pain Management- Simple Analgesia

# Create Table of Oral Simple Analgesia
tsimp1PostOp<- PPP %>% 
  select(oral_type_para___1, oral_type_ibup___1, oral_type_ketor___1, oral_type_napro___1, oral_type_dicon___1,
         oral_type_melox___1, oral_type_celec___1, oral_type_indom___1, mech_vent, postop) %>% 
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = postop, ,missing = "no",
                  label = list(oral_type_para___1 = "Paracetamol",
                               oral_type_ibup___1 = "Ibuprofen",
                               oral_type_ketor___1 = "Ketorolac",
                               oral_type_napro___1 = "Naproxen",
                               oral_type_dicon___1 = "Diclofenac",
                               oral_type_melox___1 = "Meloxicam",
                               oral_type_celec___1 = "Celecoxib",
                               oral_type_indom___1 = "Indomethacin"),
                  value = list(oral_type_para___1 ~ "Checked",
                               oral_type_ibup___1 ~ "Checked",
                               oral_type_ketor___1 ~ "Checked",
                               oral_type_napro___1 ~ "Checked",
                               oral_type_dicon___1 ~ "Checked",
                               oral_type_melox___1 ~ "Checked",
                               oral_type_celec___1 ~ "CChecked",
                               oral_type_indom___1 ~ "Checked"),
                  statistic = list(all_continuous() ~ "{mean} ({sd})",
                                   all_categorical() ~ "{n} ({p}%)")) %>% 
      modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
      modify_header(list(
        label ~"**Simple Analgesic**")) %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall()
  )

# Create Table of IV Simple Analgesia

tsimp2PostOp <- PPP %>% 
  select(intrv_type_parac___1, intrv_type_ibu___1,
         intrv_type_ket___1, intrv_type_paracox___1, intrv_type_indom___1,  mech_vent, postop) %>% 
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = postop, ,missing = "no",
                  label = list(intrv_type_parac___1 = "Paracetamol",
                               intrv_type_ibu___1 = "Ibuprofen",
                               intrv_type_ket___1 = "Ketorlac",
                               intrv_type_paracox___1 = "Parecoxib",
                               intrv_type_indom___1 = "Indomethacin"),
                  value = list(intrv_type_parac___1 ~ "Checked",
                               intrv_type_ibu___1 ~ "Checked",
                               intrv_type_ket___1 ~ "Checked",
                               intrv_type_paracox___1 ~ "Checked",
                               intrv_type_indom___1 ~ "Checked"),
                  statistic = list(all_continuous() ~ "{mean} ({sd})",
                                   all_categorical() ~ "{n} ({p}%)")) %>% 
      modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
      modify_header(list(
        label ~"**Simple Analgesic**")) %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall()
  )

# Stack the Two tables
tbl_stack(list(tsimp1PostOp, tsimp2PostOp),
          group_header = c("Oral Analgesia", "IV Analgesia")) %>% 
  modify_caption("**Table 20. Simple Analgesia in Post-Operative Vs Non-Operative**")

### Part h: Pain Management- Adjunct Analgesia

# Generate Ketamine Table First
tadj1PostOp <- PPP %>% 
  select(intrv_type_ketinf___1, mech_vent, postop) %>% 
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = postop, ,missing = "no",
                  label = list(intrv_type_ketinf___1 = "Ketmine Infusion"),
                  value = list(intrv_type_ketinf___1 ~ "Checked"),
                  statistic = list(all_continuous() ~ "{mean} ({sd})",
                                   all_categorical() ~ "{n} ({p}%)")) %>% 
      modify_header(list(
        label ~"**Analgesic Adjunct**")) %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall()
  )

# Generate Regional Analgesia Table
tadj2PostOp <- PPP %>% 
  select(regional_route___1, regional_route___2, regional_route___3, regional_route___4, regional_route___5, regional_route___6, regional_route___7, regional_route___8, mech_vent, postop) %>% 
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = postop, ,missing = "no",
                  label = list(regional_route___1 = "Thoracic Epidural",
                               regional_route___2 = "Lumbar Epidural",
                               regional_route___3 = "Transversus Abdominus Plane Block",
                               regional_route___4 = "Wound Catheter",
                               regional_route___5 = "Paravertebral Block",
                               regional_route___6 = "Extrapleural Block",
                               regional_route___7 = "Brachial Plexus Block",
                               regional_route___8 = "Lower Limb Block"),
                  value = list(regional_route___1 ~ "Checked",
                               regional_route___2 ~ "Checked",
                               regional_route___3 ~ "Checked",
                               regional_route___4 ~ "Checked",
                               regional_route___5 ~ "Checked",
                               regional_route___6 ~ "Checked",
                               regional_route___7 ~ "CChecked",
                               regional_route___8 ~ "Checked"),
                  statistic = list(all_continuous() ~ "{mean} ({sd})",
                                   all_categorical() ~ "{n} ({p}%)")) %>% 
      modify_header(list(
        label ~"**Analgesic Adjunct**")) %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall()
  )

# Stack Table
tbl_stack(list(tadj1PostOp, tadj2PostOp),
          group_header = c(" ", "Regional Analgesia")) %>% 
  modify_caption("**Table 21. Adjuvant Analgesia in Post-Operative Vs Non-Operative**")

### Part i: Pain Management- Anti-Neuropathic Analgesia
PPP %>% 
  select(anti_neuropath___1, anti_neuropath___2, anti_neuropath___3, anti_neuropath___4, anti_neuropath___5, anti_neuropath___6, mech_vent, postop) %>% 
  mutate(mech_vent = paste(mech_vent)) %>%
  tbl_strata(
    strata = mech_vent,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = postop, ,missing = "no",
                  label = list(anti_neuropath___1 = "Gabapentin",
                               anti_neuropath___2 = "Pregabalin",
                               anti_neuropath___3 = "Carbamazepine",
                               anti_neuropath___4 = "Valproate",
                               anti_neuropath___5 = "Amitriptylline",
                               anti_neuropath___6 = "Lignocaine Infusion"),
                  value = list(anti_neuropath___1 ~ "Checked",
                               anti_neuropath___2 ~ "Checked",
                               anti_neuropath___3 ~ "Checked",
                               anti_neuropath___4 ~ "Checked",
                               anti_neuropath___5 ~ "Checked",
                               anti_neuropath___6 ~ "Checked"),
                  statistic = list(all_continuous() ~ "{mean} ({sd})",
                                   all_categorical() ~ "{n} ({p}%)")) %>% 
      modify_header(list(
        label ~"**Anti-Neuropathic Agent**")) %>% 
      bold_labels() %>% 
      italicize_levels() %>% 
      add_overall() %>% 
      modify_caption("**Table 22. Anti-Neuropathic Agents in Post-Operative Vs Non-Operative**")
  )

### Part j. Comparison of All Analgesic Adjuncts in Post-Operative and Non-Operative Groups in Each Ventilation Strata

# Generate Required Variables for Chi-Squared Tests
# Simple Analgesia
# Paracetamol
PPP <- PPP %>% 
  mutate(Paracetamol = case_when(
    (intrv_type_parac___1 == "Checked") ~ "Yes",
    (oral_type_para___1 == "Checked") ~ "Yes")) %>% 
  mutate(Paracetamol = fct_explicit_na(Paracetamol, na_level = "No"))

#NSAIDs
PPP <- PPP %>% 
  mutate(NSAID = case_when(
    (oral_type_ibup___1 == "Checked") ~ "Yes",
    (oral_type_ketor___1 == "Checked") ~ "Yes",
    (oral_type_napro___1 == "Checked") ~ "Yes",
    (oral_type_dicon___1 == "Checked") ~ "Yes",
    (oral_type_melox___1 == "Checked") ~ "Yes",
    (oral_type_celec___1 == "Checked") ~ "Yes",
    (oral_type_indom___1 == "Checked") ~ "Yes",
    (intrv_type_ibu___1 == "Checked") ~ "Yes",
    (intrv_type_ket___1 == "Checked") ~ "Yes",
    (intrv_type_paracox___1 == "Checked") ~ "Yes",
    (intrv_type_indom___1 == "Checked") ~ "Yes",
  )) %>% 
  mutate(NSAID = fct_explicit_na(NSAID, na_level = "No"))

# Anti-Neuropathic Agents
PPP <- PPP %>% 
  mutate(Anti_Neuro = case_when(
    (anti_neuropath___1 == "Checked") ~ "Yes",
    (anti_neuropath___2 == "Checked") ~ "Yes",
    (anti_neuropath___3 == "Checked") ~ "Yes",
    (anti_neuropath___4 == "Checked") ~ "Yes",
    (anti_neuropath___5 == "Checked") ~ "Yes",
    (anti_neuropath___6 == "Checked") ~ "Yes")) %>% 
  mutate(Anti_Neuro = fct_explicit_na(Anti_Neuro, na_level = "No"))


PPPVent <- PPP %>% filter(mech_vent == "Ventilated")
PPPNonVent <- PPP %>% filter(mech_vent == "Non-Ventilated")


# Test for Differences in Simple Analgesia Type in Each Group (Ventilated and Non-Ventilated)
# Paracetamol
# Ventilated Pts
Para1 <- tidy(chisq.test(PPPVent$Paracetamol, PPPVent$postop == "Post-Operative")) %>% mutate(Comparator = "Paracetamol (Vent)")
ParaVent.Table <- table(Paracetamol = PPPVent$Paracetamol, Op = PPPVent$postop)

# Non-Ventilated Pts
Para2 <- tidy(chisq.test(PPPNonVent$Paracetamol, PPPNonVent$postop == "Non-Operative")) %>% mutate(Comparator = "Paracetamol (Non-Vent)") 
ParaNonVent.Table <- table(Paracetamol = PPPNonVent$Paracetamol, Op = PPPNonVent$postop)

# NSAIDS
#Ventilated 
NSAID1 <- tidy(chisq.test(PPPVent$NSAID, PPPVent$postop == "Post-Operative")) %>% mutate(Comparator = "NSAIDs (Vent)") 

# Non-Ventilated Pts
NSAID2 <- tidy(chisq.test(PPPNonVent$NSAID, PPPNonVent$postop == "Non-Operative")) %>% mutate(Comparator = "NSAIDs (Non-Vent)") 

# Test for Differences in Ketamine in Each Group (Ventilated and Non-Ventilated)
# Ventilated Pts
Ket1 <- tidy(chisq.test(PPPVent$intrv_type_ketinf___1, PPPVent$postop == "Post-Operative")) %>% mutate(Comparator = "Ketamine (Vent)")  

# Non-Ventilated Pts
Ket2 <- tidy(chisq.test(PPPNonVent$intrv_type_ketinf___1, PPPNonVent$postop == "Non-Operative")) %>% mutate(Comparator = "Ketamine (Non-Vent)")  

# Test for Differences in Regional Analgesia in Each Group (Ventilated and Non-Ventilated)
# Ventilated Pts
RA1 <- tidy(chisq.test(PPPVent$regional_anaesth, PPPVent$postop == "Post-Operative")) %>% mutate(Comparator = "Regional Analgesia (Vent)")  

# Non-Ventilated Pts
RA2 <- tidy(chisq.test(PPPNonVent$regional_anaesth, PPPNonVent$postop == "Non-Operative")) %>% mutate(Comparator = "Regional Analgesia (Vent)")  

# Test for Differences in Anti-Neuropathic Medications in Each Group (Ventilated and Non-Ventilated)
# Ventilated Pts
Neur1 <- tidy(chisq.test(PPPVent$Anti_Neuro, PPPVent$postop == "Post-Operative")) %>% mutate(Comparator = "Anti-Neuropathic Agents (Vent)")  

# Non-Ventilated Pts
Neur2 <- tidy(chisq.test(PPPNonVent$Anti_Neuro, PPPNonVent$postop == "Non-Operative")) %>% mutate(Comparator = "Anti-Neuropathic (Non-Vent)") 

TotalAdjunctComparison <- rbind(Para1, Para2, NSAID1, NSAID2, Ket1, Ket2, RA1, RA2, Neur1, Neur2) %>% select(Comparator, p.value) %>%  as.data.frame() %>% format(scientific=FALSE)

rmarkdown::paged_table(TotalAdjunctComparison)

