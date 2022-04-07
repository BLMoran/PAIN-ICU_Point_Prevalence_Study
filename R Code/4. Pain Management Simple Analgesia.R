## Part 5: Pain Management- Simple Analgesia
library(tidyverse)
library(gtsummary)

# Create Table of Oral Simple Analgesia
tsimp1<- PPP %>% 
  select(oral_type_para___1, oral_type_ibup___1, oral_type_ketor___1, oral_type_napro___1, oral_type_dicon___1,
         oral_type_melox___1, oral_type_celec___1, oral_type_indom___1, mech_vent) %>% 
  tbl_summary(by = mech_vent, ,missing = "no",
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

# Create Table of IV Simple Analgesia


tsimp2 <- PPP %>% 
  select(intrv_type_parac___1, intrv_type_ibu___1,
         intrv_type_ket___1, intrv_type_paracox___1, intrv_type_indom___1,  mech_vent) %>% 
  tbl_summary(by = mech_vent, ,missing = "no",
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

# Stack the Two tables
tbl_stack(list(tsimp1, tsimp2),
          group_header = c("Oral Analgesia", "IV Analgesia")) %>% 
  modify_caption("**Table 9. Simple Analgesia**")

