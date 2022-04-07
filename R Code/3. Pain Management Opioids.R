## Part 4: Pain Management- Opioids

### Part a: Opioid Type

# Create Variable of Opioid Infusion
PPP <- PPP %>% 
  mutate(Opioidinf = case_when(
    (intrv_type_morphinf___1 == "Checked") ~ "Morphine",
    (intrv_type_fentinf___1 == "Checked") ~ "Fentanyl",
    (intrv_type_hydroinf___1 == "Checked") ~ "Hydromorphone")) %>% 
  mutate(Opioidinf = fct_explicit_na(Opioidinf, na_level = "None"))

# Reorder Factor Levels
PPP <- PPP %>% mutate(Opioidinf = factor(Opioidinf, levels = c(
  "Morphine", "Fentanyl", "Hydromorphone", "None")))

# Create Variable of Opioid PCA
PPP <- PPP %>% 
  mutate(OpioidPCA = case_when(
    (pca_type_morph___1 == "Checked") ~ "Morphine",
    (pca_type_fent___1 == "Checked") ~ "Fentanyl",
    (pca_type_hydro___1 == "Checked") ~ "Hydromorphone",
    (pca_type_oxyc___1 == "Checked") ~ "Oxycodone")) %>% 
  mutate(OpioidPCA = fct_explicit_na(OpioidPCA, na_level = "None"))

# Reorder Factor Levels
PPP <- PPP %>% mutate(OpioidPCA = factor(OpioidPCA, levels = c(
  "Morphine", "Fentanyl", "Hydromorphone", "Oxycodone", "None")))

# Create Variable of Oral Opioid
PPP <- PPP %>% 
  mutate(OpioidPO = case_when(
    (oral_type_oxy___1 == "Checked") ~ "Oxycodone",
    (oral_type_oxycod___1 == "Checked") ~ "Oxycodone",
    (oral_type_targ___1 == "Checked") ~ "Oxycodone",
    (oral_type_tram___1 == "Checked") ~ "Tramadol",
    (oral_type_tap___1 == "Checked") ~ "Tapentadol",
    (oral_type_bup___1 == "Checked") ~ "Buprenorphine",
    (oral_type_ms___1 == "Checked") ~ "Morphine",
    (oral_type_ord___1 == "Checked") ~ "Morphine",
    (oral_type_meth___1 == "Checked") ~ "Methadone",
    (oral_type_cod___1 == "Checked") ~ "Codeine")) %>% 
  mutate(OpioidPO = fct_explicit_na(OpioidPO, na_level = "None"))

# Reorder Factor Levels
PPP <- PPP %>% mutate(OpioidPO = factor(OpioidPO, levels = c(
  "Oxycodone", "Morphine", "Tapentadol", "Tramadol", "Codeine", "Buprenorphine", "Methadone", "None")))

# Generate Table
PPP %>% 
  select(Opioidinf, OpioidPCA, OpioidPO,  mech_vent) %>% 
  tbl_summary(by = mech_vent, ,missing = "no",
              label = list(Opioidinf="Type of Opioid Infusion",
                           OpioidPCA="Type of Opioid PCA",
                           OpioidPO="Type of Oral Opioid"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  modify_header(list(
    label ~"**Opioid Route**")) %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall() %>% 
  modify_caption("**Table 4. Opioid Type and Route of Administration**")

### Part b: Cumulative Opioid Dose

# Generate new variables of cumulative doses of each route of opioid
# Cumulative doses of Separate Opioid Infusions

PPP <- PPP %>% mutate(
  Morphinf_oMEDD = intrv_dose_morphinf*3,
  Fentinf_oMEDD = intrv_dose_fentinf/5,
  Hydroinf_oMEDD = intrv_dose_hydroinf*15,
  MorphPCA_oMEDD = pca_dose_morph*3,
  FentPCA_oMEDD = pca_dose_fent/5,
  HydroPCA_oMEDD = pca_dose_hydro*15,
  OxyPCA_oMEDD = pca_dose_oxyc*3,
  OxycontPO_oMEDD = oral_dose_oxy*1.5,
  OxycodonePO_oMEDD = oral_dose_oxycod*1.5,
  TargPO_oMEDD = oral_dose_targ*1.5,
  TramPO_oMEDD = oral_dose_tram/5,
  TapentPO_oMEDD = oral_dose_tap*0.4,
  BupPO_oMEDD = oral_dose_bup*0.04,
  MSContPO_oMEDD = oral_dose_ms,
  OrdPO_oMEDD = oral_dose_ord,
  CodPO_oMEDD = oral_dose_cod/7.8) %>% 
  mutate(OxycodPO_oMEDD = coalesce(OxycontPO_oMEDD, OxycodonePO_oMEDD, TargPO_oMEDD)) %>% 
  mutate(MorphPO_oMEDD = coalesce(MSContPO_oMEDD, OrdPO_oMEDD))

# Generate Table of Cumulative Opioid Doses for Opioid Infusion

toMEDD1 <- PPP %>% 
  select(Morphinf_oMEDD, Fentinf_oMEDD, mech_vent) %>% 
  tbl_summary(by = mech_vent, missing = "no",
              type = all_continuous() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})",
                                               "{median} ({p25}, {p75})"),
              digits = all_continuous() ~ 1,
              label = list(Morphinf_oMEDD = "Morphine Infusion",
                           Fentinf_oMEDD = "Fentanyl Infusion")) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  modify_header(list(
    label ~"**Opioid Type & Route**")) %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall() %>% 
  modify_caption("**Table 5. Cumulative Doses of Opioid Infusions**")

# Generate Table of Cumulative Opioid Doses for Opioid PCAs

toMEDD2 <- PPP %>% 
  select(MorphPCA_oMEDD, FentPCA_oMEDD, HydroPCA_oMEDD, OxyPCA_oMEDD, mech_vent) %>% 
  tbl_summary(by = mech_vent, missing = "no",
              type = everything() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})",
                                               "{median} ({p25}, {p75})"),
              digits = all_continuous() ~ 1,
              label = list(MorphPCA_oMEDD = "Morphine PCA",
                           FentPCA_oMEDD = "Fentanyl PCA",
                           HydroPCA_oMEDD = "Hydromorphone PCA",
                           OxyPCA_oMEDD = "Oxycodone PCA")) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  modify_header(list(
    label ~"**Opioid Type & Route**")) %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall() %>% 
  modify_caption("**Table 6. Cumulative Doses of Opioid PCAs**")

# Generate Table of Cumulative Opioid Doses for Oral Opioid

toMEDD3 <- PPP %>% 
  select(OxycodPO_oMEDD, MorphPO_oMEDD, TapentPO_oMEDD, TramPO_oMEDD, BupPO_oMEDD, CodPO_oMEDD, mech_vent) %>% 
  tbl_summary(by = mech_vent, missing = "no",
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
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  modify_header(list(
    label ~"**Opioid Type**")) %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall() %>% 
  modify_caption("**Table 7. Cumulative Doses of Oral Opioids**")

### Part c: Cumulative Opioid Dose in Mechanically Ventilated Patients

# Opioid Infusion Doses

# Wrangle Data Frames to allow box plot of varying lengths
Morphinf<-PPP %>%  filter(mech_vent=="Ventilated") %>% drop_na(Morphinf_oMEDD) %>%  rename(oMEDD = Morphinf_oMEDD) %>% select(oMEDD)
Fentinf<- PPP %>% filter(mech_vent=="Ventilated") %>% drop_na(Fentinf_oMEDD) %>% rename(oMEDD = Fentinf_oMEDD) %>% select(oMEDD)

Morphinf$group<- 'Morphine'
Fentinf$group<- 'Fentanyl'

Opioidinf<- rbind(Morphinf, Fentinf)

ggplot(Opioidinf, aes(x=group, y=oMEDD)) +
  geom_boxplot(position="dodge", outlier.shape=NA)+
  scale_fill_manual(values = c("steelblue3", "grey65"))+
  labs(x="Opioid Infusion", y="Cumulative Daily Opioid Dose (oMEDD)", title="Figure 3: Cumulative Doses of Opioid Infusions", subtitle = "In Mechanically Ventilated Patients")+
  coord_cartesian(y = c(0,750))

ggsave("Fig3.png", dpi = 300)

# Opioid PCA Doses

# Wrangle Data Frames to allow box plot of varying lengths
MorphPCA<-PPP %>%  filter(mech_vent=="Ventilated") %>% drop_na(MorphPCA_oMEDD) %>% rename(oMEDD = MorphPCA_oMEDD) %>% select(oMEDD)
FentPCA<- PPP %>% filter(mech_vent=="Ventilated") %>% drop_na(FentPCA_oMEDD) %>% rename(oMEDD = FentPCA_oMEDD) %>% select(oMEDD)
HydroPCA<- PPP %>% filter(mech_vent=="Ventilated") %>% drop_na(HydroPCA_oMEDD) %>% rename(oMEDD = HydroPCA_oMEDD) %>% select(oMEDD)
OxyPCA<- PPP %>% filter(mech_vent=="Ventilated") %>% drop_na(OxyPCA_oMEDD) %>% rename(oMEDD = OxyPCA_oMEDD) %>% select(oMEDD)

MorphPCA$group<- 'Morphine'
FentPCA$group<- 'Fentanyl'
HydroPCA$group<- 'Hydromorphone'
OxyPCA$group<- 'Oxycodone'

OpioidPCA<- rbind(MorphPCA, FentPCA, HydroPCA, OxyPCA)

ggplot(OpioidPCA, aes(x=group, y=oMEDD)) +
  geom_boxplot(position="dodge", outlier.shape=NA)+
  labs(x="Opioid PCA", y="Cumulative Daily Opioid Dose (oMEDD)", title="Figure 4: Cumulative Doses of Opioid PCAs", subtitle = "In Mechanically Ventilated Patients")+
  coord_cartesian(y = c(0,350))

# Oral Opioid Doses

# Wrangle Data Frames to allow box plot of varying lengths
OxycodPO<- PPP %>% filter(mech_vent=="Ventilated") %>% drop_na(OxycodPO_oMEDD) %>% rename(oMEDD = OxycodPO_oMEDD) %>% select(oMEDD)
TramPO<- PPP %>% filter(mech_vent=="Ventilated") %>% drop_na(TramPO_oMEDD) %>% rename(oMEDD = TramPO_oMEDD) %>% select(oMEDD)
TapentPO<- PPP %>% filter(mech_vent=="Ventilated") %>% drop_na(TapentPO_oMEDD) %>% rename(oMEDD = TapentPO_oMEDD) %>% select(oMEDD)
BupPO<- PPP %>% filter(mech_vent=="Ventilated") %>% drop_na(BupPO_oMEDD) %>% rename(oMEDD = BupPO_oMEDD) %>% select(oMEDD)
MorphPO<- PPP %>% filter(mech_vent=="Ventilated") %>% drop_na(MorphPO_oMEDD) %>% rename(oMEDD = MorphPO_oMEDD) %>% select(oMEDD)
CodPO<- PPP %>% filter(mech_vent=="Ventilated") %>% drop_na(CodPO_oMEDD) %>% rename(oMEDD = CodPO_oMEDD) %>% select(oMEDD)

OxycodPO$group<- 'Oxycodone'
TramPO$group<- 'Tramadol'
TapentPO$group<- 'Tapentadol'
BupPO$group<- 'Buprenorphine'
MorphPO$group<- 'Morphine'
CodPO$group<- 'Codeine'

OpioidPO<- rbind(OxycodPO, TramPO, TapentPO, BupPO, MorphPO, CodPO)

ggplot(OpioidPO, aes(x=group, y=oMEDD)) +
  geom_boxplot(position="dodge", outlier.shape=NA)+
  labs(x="Oral Opioid", y="Cumulative Daily Opioid Dose (oMEDD)", title="Figure 5: Cumulative Doses of Oral Opioid", subtitle = "In Mechanically Ventilated Patients")+
  coord_cartesian(y = c(0,70))

### Part d: Cumulative Opioid Dose in Non-Mechanically Ventilated Patients

# Wrangle Data Frames to allow box plot of varying lengths
MorphinfNonMV<-PPP %>%  filter(mech_vent=="Non-Ventilated") %>% drop_na(Morphinf_oMEDD) %>%  rename(oMEDD = Morphinf_oMEDD) %>% select(oMEDD)
FentinfNonMV<- PPP %>% filter(mech_vent=="Non-Ventilated") %>% drop_na(Fentinf_oMEDD) %>% rename(oMEDD = Fentinf_oMEDD) %>% select(oMEDD)

MorphinfNonMV$group<- 'Morphine'
FentinfNonMV$group<- 'Fentanyl'

OpioidinfNonMV<- rbind(MorphinfNonMV, FentinfNonMV)

ggplot(OpioidinfNonMV, aes(x=group, y=oMEDD)) +
  geom_boxplot(position="dodge", outlier.shape=NA)+
  scale_fill_manual(values = c("steelblue3", "grey65"))+
  labs(x="Opioid Infusion", y="Cumulative Daily Opioid Dose (oMEDD)", title="Figure 6: Cumulative Doses of Opioid Infusions", subtitle = "In Non-Mechanically Ventilated Patients")+
  coord_cartesian(y = c(0,300))

# Opioid PCA Doses

# Wrangle Data Frames to allow box plot of varying lengths
MorphPCANonMV<-PPP %>%  filter(mech_vent=="Non-Ventilated") %>% drop_na(MorphPCA_oMEDD) %>% rename(oMEDD = MorphPCA_oMEDD) %>% select(oMEDD)
FentPCANonMV<- PPP %>% filter(mech_vent=="Non-Ventilated") %>% drop_na(FentPCA_oMEDD) %>% rename(oMEDD = FentPCA_oMEDD) %>% select(oMEDD)
HydroPCANonMV<- PPP %>% filter(mech_vent=="Non-Ventilated") %>% drop_na(HydroPCA_oMEDD) %>% rename(oMEDD = HydroPCA_oMEDD) %>% select(oMEDD)
OxyPCANonMV<- PPP %>% filter(mech_vent=="Non-Ventilated") %>% drop_na(OxyPCA_oMEDD) %>% rename(oMEDD = OxyPCA_oMEDD) %>% select(oMEDD)

MorphPCANonMV$group<- 'Morphine'
FentPCANonMV$group<- 'Fentanyl'
HydroPCANonMV$group<- 'Hydromorphone'
OxyPCANonMV$group<- 'Oxycodone'

OpioidPCANonMV<- rbind(MorphPCANonMV, FentPCANonMV, HydroPCANonMV, OxyPCANonMV)

positions<-c("Oxycodone", "Morphine", "Fentanyl", "Hydromorphone")
ggplot(OpioidPCANonMV, aes(x=group, y=oMEDD)) +
  geom_boxplot(position="dodge", outlier.shape=NA)+
  labs(x="Opioid PCA", y="Cumulative Daily Opioid Dose (oMEDD)", title="Figure 7: Cumulative Doses of Opioid PCAs", subtitle = "In Non-Mechanically Ventilated Patients")+
  coord_cartesian(y = c(0,400))+
  scale_x_discrete(limits=positions)

# Oral Opioid Doses

# Wrangle Data Frames to allow box plot of varying lengths
OxycodPONonMV<- PPP %>% filter(mech_vent=="Non-Ventilated") %>% drop_na(OxycodPO_oMEDD) %>% rename(oMEDD = OxycodPO_oMEDD) %>% select(oMEDD)
TramPONonMV<- PPP %>% filter(mech_vent=="Non-Ventilated") %>% drop_na(TramPO_oMEDD) %>% rename(oMEDD = TramPO_oMEDD) %>% select(oMEDD)
TapentPONonMV<- PPP %>% filter(mech_vent=="Non-Ventilated") %>% drop_na(TapentPO_oMEDD) %>% rename(oMEDD = TapentPO_oMEDD) %>% select(oMEDD)
BupPONonMV<- PPP %>% filter(mech_vent=="Non-Ventilated") %>% drop_na(BupPO_oMEDD) %>% rename(oMEDD = BupPO_oMEDD) %>% select(oMEDD)
MorphPONonMV<- PPP %>% filter(mech_vent=="Non-Ventilated") %>% drop_na(MorphPO_oMEDD) %>% rename(oMEDD = MorphPO_oMEDD) %>% select(oMEDD)
CodPONonMV<- PPP %>% filter(mech_vent=="Non-Ventilated") %>% drop_na(CodPO_oMEDD) %>% rename(oMEDD = CodPO_oMEDD) %>% select(oMEDD)

OxycodPONonMV$group<- 'Oxycodone'
TramPONonMV$group<- 'Tramadol'
TapentPONonMV$group<- 'Tapentadol'
BupPONonMV$group<- 'Buprenorphine'
MorphPONonMV$group<- 'Morphine'
CodPONonMV$group<- 'Codeine'

OpioidPONonMV<- rbind(OxycodPONonMV, TramPONonMV, TapentPONonMV, BupPONonMV, MorphPONonMV, CodPONonMV)

positions<-c("Oxycodone", "Morphine", "Tapentadol", "Tramadol", "Buprenorphine", "Codeine")
ggplot(OpioidPONonMV, aes(x=group, y=oMEDD)) +
  geom_boxplot(position="dodge", outlier.shape=NA)+
  labs(x="Oral Opioid", y="Cumulative Daily Opioid Dose (oMEDD)", title="Figure 8: Cumulative Doses of Oral Opioid", subtitle = "In Non-Mechanically Ventilated Patients")+
  coord_cartesian(y = c(0,70))+
  scale_x_discrete(limits=positions)

### Part e: Cumulative Total Opioid Dose in All Patients

# Generate new variable of oMEDD for IV & PO opioids

PPP2 <- PPP %>% select(oral_dose_oxy:pca_dose_peth, -oral_dose_other, -intrv_dose_other, mech_vent, postop, pain_assessed) %>% 
  select(contains('dose'), mech_vent, postop, pain_assessed) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  mutate(OpioidIV_Tot_oMEDD = intrv_dose_morphbol*3 + intrv_dose_morphinf*3 + intrv_dose_fentbol/5
         + intrv_dose_fentinf/5 + intrv_dose_hydrobol*15 +
           intrv_dose_hydroinf*15 + intrv_dose_oxyc*3 + intrv_dose_trama/5 +
           pca_dose_morph*3 + pca_dose_fent/5 + pca_dose_hydro*15 + 
           pca_dose_oxyc*3,
         OpioidPO_Tot_oMEDD = oral_dose_oxy*1.5 + oral_dose_oxycod*1.5 + oral_dose_targ*1.5 +
           oral_dose_tram/5 + oral_dose_tap*0.4 + oral_dose_bup*0.04 +
           oral_dose_ms + oral_dose_ord + oral_dose_cod/7.8,
         Opioid_oMEDD_Tot = OpioidIV_Tot_oMEDD + OpioidPO_Tot_oMEDD)

PPP_IV <- PPP2 %>% filter(OpioidIV_Tot_oMEDD>0) %>% rename(oMEDD = OpioidIV_Tot_oMEDD) %>% select(oMEDD, mech_vent, postop)
PPP_PO <- PPP2 %>% filter(OpioidPO_Tot_oMEDD>0 )%>% rename(oMEDD = OpioidPO_Tot_oMEDD) %>% select(oMEDD, mech_vent, postop)
PPP_Tot <- PPP2 %>% filter(Opioid_oMEDD_Tot>0) %>% rename(oMEDD = Opioid_oMEDD_Tot) %>% select(oMEDD, mech_vent, postop)

# Generate table of total oMEDD 
t1 <-
  PPP_IV %>%
  select(oMEDD, mech_vent) %>%
  tbl_summary(by = mech_vent, missing = "no",
              type = everything() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})",
                                               "{median} ({p25}, {p75})"),
              digits = all_continuous() ~ 1,
              label = list(oMEDD = "Parenteral Opioid")) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall() %>% 
  add_n()

t2 <-
  PPP_PO %>%
  select(oMEDD, mech_vent) %>%
  tbl_summary(by = mech_vent, missing = "no",
              type = everything() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})",
                                               "{median} ({p25}, {p75})"),
              digits = all_continuous() ~ 1,
              label = list(oMEDD = "Oral Opioid")) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall() %>% 
  add_n()

t3 <-
  PPP_Tot %>%
  select(oMEDD, mech_vent) %>%
  tbl_summary(by = mech_vent, missing = "no",
              type = everything() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})",
                                               "{median} ({p25}, {p75})"),
              digits = all_continuous() ~ 1,
              label = list(oMEDD = "Total Opioid")) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall() %>% 
  add_n()

tbl_stack(list(t1, t2, t3)) %>% 
  modify_header(list(
    label ~"**Opioid Route**"))%>% 
  modify_caption("**Table 8. Cumulative Doses of Opioids**")

# Generate Graph of Total oMEDD

PPP_PO$group<- 'Oral Opioid'
PPP_IV$group<- 'Parenteral Opioid'
PPP_Tot$group<- 'Total Opioid'


TotalOpioid<- rbind(PPP_PO, PPP_IV, PPP_Tot)

ggplot(TotalOpioid, aes(x=group, y=oMEDD, fill=mech_vent)) +
  geom_boxplot(outlier.shape=NA)+
  scale_fill_manual(values = c("#abbcea", "#4D81DA"))+
  labs(x=" ", y="Cumulative Daily Opioid Dose (oMEDD)", title="Figure 4: Cumulative Doses of Opioids", subtitle = "In Patients That Received Opioids", fill="Ventilation Status")+
  theme_bw()+
  coord_cartesian(y = c(0,800))

ggsave("Fig4.png", dpi = 300)
