#Pain Management- Adjunct Analgesia


# Generate Ketamine Table
tadj1 <- PPP %>% 
  select(intrv_type_ketinf___1, mech_vent) %>% 
  tbl_summary(by = mech_vent, ,missing = "no",
              label = list(intrv_type_ketinf___1 = "Ketmine Infusion"),
              value = list(intrv_type_ketinf___1 ~ "Checked"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  modify_header(list(
    label ~"**Analgesic Adjunct**")) %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall()

# Generate Regional Analgesia Table
tadj2 <- PPP %>% 
  select(regional_route___1, regional_route___2, regional_route___3, regional_route___4, regional_route___5, regional_route___6, regional_route___7, regional_route___8, mech_vent) %>% 
  tbl_summary(by = mech_vent, ,missing = "no",
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
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  modify_header(list(
    label ~"**Analgesic Adjunct**")) %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall()

# Stack Table
tbl_stack(list(tadj1, tadj2),
          group_header = c(" ", "Regional Analgesia")) %>% 
  modify_caption("**Table 10. Adjuvant Analgesia**")

#Anti-Neuropathic Analgesia

PPP %>% 
  select(anti_neuropath___1, anti_neuropath___2, anti_neuropath___3, anti_neuropath___4, anti_neuropath___5, anti_neuropath___6, mech_vent) %>% 
  tbl_summary(by = mech_vent, ,missing = "no",
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
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  modify_header(list(
    label ~"**Anti-Neuropathic Agent**")) %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall() %>% 
  modify_caption("**Table 11. Anti-Neuropathic Agents**")

# Create Table of Analgesic Adjuncts
# Generate Required Variables
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


tadjuncts<- PPP %>% 
  select(Paracetamol, NSAID, intrv_type_ketinf___1, regional_anaesth, Anti_Neuro, mech_vent) %>% 
  tbl_summary(by = mech_vent, ,missing = "no",
              label = list(NSAID = "NSAIDs",
                           intrv_type_ketinf___1 = "Ketamine Infusion",
                           regional_anaesth = "Regional Analgesia",
                           Anti_Neuro = "Anti-Neuropathic Agents"),
              value = list(Paracetamol ~ "Yes",
                           NSAID ~ "Yes",
                           intrv_type_ketinf___1 ~ "Checked",
                           regional_anaesth ~ "Yes",
                           Anti_Neuro ~ "Yes"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  modify_header(list(
    label ~"**Analgesic Adjunct**")) %>% 
  bold_labels() %>% 
  add_overall() %>% 
  modify_caption("**Table 2. Analgesic Adjuncts**")

tadjuncts
