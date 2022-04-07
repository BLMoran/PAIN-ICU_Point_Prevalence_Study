## Data Wrangling and Generation of Table 1

# Load packages with pacman
pacman::p_load(readxl, #import dataset
               tidyverse, # data wrangling & ggplot
               gtsummary, # creating tables
               broom) #tiding some tests
               
# Open and attach CSV file #
PPP <- read_excel("~/Research/PhD/2. Variable Selection- PPP/Data/PPP Data.xlsx")

library(tidyverse)
library(gtsummary)

# Remove Variables of Cardiac Surgery and Neurological/Neurosurgical
exclude <- c(401:405, 402.02, 407:410, 601, 601.06)
exclude2 <- c(1202:1213, 1501:1503, 1505:1601)

PPP<- PPP %>% filter(!(apacheiii_non_op %in% exclude)) %>% 
  filter(!(apacheiii_postop %in% exclude2))

# Rename Variables for admission diagnoses
# To simplify the table, all post-operative diagnoses are renamed Post-Operative"
PPP <- PPP %>% 
  mutate(apache3 = case_when(
    (apacheiii_postop>=1301) & (apacheiii_postop<=1304) ~ "Post-Operative",
    (apacheiii_postop>=1401) & (apacheiii_postop<=1413) ~ "Post-Operative",
    (apacheiii_postop>=1602) & (apacheiii_postop<=1605) ~ "Post-Operative",
    (apacheiii_postop>=1701) & (apacheiii_postop<=1705) ~ "Post-Operative",
    (apacheiii_postop>=1801) & (apacheiii_postop<=1803) ~ "Post-Operative",
    (apacheiii_postop>=1902) & (apacheiii_postop<=1904) ~ "Post-Operative",
    (apacheiii_postop==2101) ~ "Post-Operative",
    (apacheiii_postop==2201) ~ "Post-Operative",
    (apacheiii_non_op>=101) & (apacheiii_non_op<=111) ~ "Cardiovascular",
    (apacheiii_non_op>=201) & (apacheiii_non_op<=213) ~ "Respiratory",
    (apacheiii_non_op>=301) & (apacheiii_non_op<=313) ~ "Other",
    (apacheiii_non_op>=406) & (apacheiii_non_op<407) ~ "Other",
    (apacheiii_non_op>=501) & (apacheiii_non_op<=504) ~ "Sepsis",
    (apacheiii_non_op>601) & (apacheiii_non_op<=605) ~ "Trauma",
    (apacheiii_non_op>=701) & (apacheiii_non_op<=704) ~ "Other",
    (apacheiii_non_op>=801) & (apacheiii_non_op<=802) ~ "Other",
    (apacheiii_non_op>=901) & (apacheiii_non_op<=903) ~ "Other",
    (apacheiii_non_op>=1101) & (apacheiii_non_op<=1102) ~ "Other"))

# Reorder Apache3
PPP <- PPP %>% mutate(apache3 = factor(apache3, levels = c("Cardiovascular", "Respiratory", "Sepsis", "Trauma", "Post-Operative", "Other")))


# Rename Admission Source Details
PPP <- PPP %>% mutate(admit_source = case_when(
  (admit_source == "Transfer from another hospital (except from another ICU)") ~ "Other",
  (admit_source == "Hospital Floor (Ward)") ~ "Hospital Ward",
  (admit_source == "Admitted from Operating Theatre following EMERGENCY surgery") ~ "Operating Theatre",
  (admit_source == "Admitted from Operating Theatre following ELECTIVE surgery") ~ "Operating Theatre",
  (admit_source == "Emergency Department") ~ "Emergency Department",
  (admit_source == "Transfer from another ICU") ~ "Other"))

# Reorder Admission Source
PPP <- PPP %>% mutate(admit_source = factor(admit_source, levels = c("Emergency Department", "Operating Theatre", "Hospital Ward", "Other")))


PPP <- PPP %>% mutate(mech_vent = case_when(
  (mech_vent == "No") ~ "Non-Ventilated",
  (mech_vent == "Yes") ~ "Ventilated"))

# Reorder Factor Levels
PPP <- PPP %>% mutate(
  mech_vent = factor(mech_vent, 
                     levels = c("Ventilated", "Non-Ventilated")))


# Generate Table 1 with gtsummary

PPP %>% 
  select(age, gender, apacheiiscore, apache3, admit_source, mech_vent) %>% 
  tbl_summary(by = mech_vent, missing = "no",
              label = list(age="Age (Years)",
                           gender="Sex",
                           apacheiiscore="APACHE2 Score",
                           apache3="Admission Diagnosis",
                           admit_source="Admission Source"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall() %>% 
  modify_caption("**Table 1. Patient Demographics**")
