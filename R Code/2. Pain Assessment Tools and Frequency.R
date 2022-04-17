## Part 3: Assessment of Pain
library(gtsummary)
library(ggplot2)

### Part a: Pain Assessment Tool

PPP <- PPP %>% 
  mutate(Assesstool = case_when(
    (pain_tool___1 == "Checked") ~ "Critical Care Pain Observation Tool",
    (pain_tool___2 == "Checked") ~ "Behavioural Pain Score",
    (pain_tool___3 == "Checked") ~ "Numerical Pain Score",
    (pain_tool___4 == "Checked") ~ "Faces",
    (pain_tool___5 == "Checked") ~ "Non-Verbal Pain Score",
    (pain_tool___6 == "Checked") ~ "Non-Validated Tool",
    (pain_tool___7 == "Checked") ~ "Non-Validated Tool")) %>% 
  mutate(Assesstool = fct_explicit_na(Assesstool, na_level = "None"))

# Reorder Factor Levels
PPP <- PPP %>% mutate(Assesstool = factor(Assesstool, levels = c(
  "Critical Care Pain Observation Tool", "Behavioural Pain Score", "Numerical Pain Score", 
  "Faces", "Non-Verbal Pain Score", "Non-Validated Tool", "None")))

#Table of Pain Assessment Tool Counts (%)
PPP %>% 
  select(Assesstool, mech_vent) %>% 
  tbl_summary(by = mech_vent, ,missing = "no",
              label = list(Assesstool="Pain Assessment Tool"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall()%>%
  modify_caption("**Table 2. Pain Assessment Tools**") 

# Graph of Pain Assessment Tools

positions<-c("Critical Care Pain Observation Tool", "Behavioural Pain Score", "Numerical Pain Score", "Faces", "Non-Verbal Pain Score", "Non-Validated Tool" , "None")
PPP %>% ggplot()+ 
  geom_bar(aes(x=Assesstool, fill=mech_vent), position = "dodge") + 
  scale_fill_manual(values = c("steelblue3", "grey65"))+
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom")+
  scale_x_discrete(limits=positions, labels=c("CPOT", "BPS", "NPS", "Faces", "NVPS", "Non-Val", "None" ))+
  labs(x="Assessment Tools", y="Tool Count", title="Figure 1: Pain Assessment Tools")

Fig1 <- PPP %>% ggplot(aes(x = Assesstool, group = mech_vent))+
  geom_bar(aes(y = ..prop.., fill = mech_vent), stat="count")+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1L))+
  theme_bw()+
  facet_grid(. ~mech_vent)+
  theme(legend.position = "none",
        panel.border = element_blank(),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing.x = unit(2, "lines"))+
  scale_fill_manual(values = c("#abbcea", "#4D81DA"))+
  scale_x_discrete(limits=positions, labels=c("CPOT", "BPS", "NPS", "Faces", "NVPS", "Non-Val", "None" ))+
  labs(x="Assessment Tools", y="Percentage (%)", title="Figure 1: Pain Assessment Tools")+
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop..), stat= "count", vjust = -.5, size = 3)

ggsave("Fig1.png", dpi = 300)


### Part b: Pain Assessment Frequency
# Tidy data for assessment frequency

PPP <- PPP %>% mutate(freq_assess = case_when(
  (freq_assess=="Hourly") ~ "Hourly",
  (freq_assess=="2 hourly") ~ "2 Hourly",
  (freq_assess=="4 hourly") ~ "4 Hourly",
  (freq_assess=="6 hourly") ~ "6 Hourly",
  (freq_assess=="8 hourly") ~ "8 Hourly",
  (freq_assess=="12 hourly") ~ "12 Hourly",
  (freq_assess=="Other") ~ "Other/None")) %>% 
  mutate(freq_assess = fct_explicit_na(freq_assess, na_level = "Other/None"))

# Reorder Factor Levels
PPP <- PPP %>% mutate(freq_assess = factor(freq_assess, levels = c(
  "Hourly", "2 Hourly", "4 Hourly", "6 Hourly", "8 Hourly", "12 Hourly", "Other/None")))

PPP %>% 
  select(freq_assess, mech_vent) %>% 
  tbl_summary(by = mech_vent, ,missing = "no",
              label = list(freq_assess="Pain Assessment Frequency"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Ventilation Status**") %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_overall() %>% 
  modify_caption("**Table 3. Pain Assessment Frequency**")

# Bar Chart of Assessment Tools #

ggplot(PPP, aes(x=freq_assess, fill=mech_vent))+ 
  geom_bar(position = "dodge") + 
  scale_fill_manual(values = c("steelblue3", "grey65"))+
  theme_bw() +
  theme(legend.title = element_blank(), legend.position ="bottom")+
  labs(x="Assessment Frequency", y="Count", title="Figure 2: Pain Assessment Frequency")

PPP %>% ggplot(aes(x = freq_assess, group = mech_vent))+
  geom_bar(aes(y = ..prop.., x = as.factor(freq_assess), fill = mech_vent), stat="count")+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1L))+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme_bw()+
  facet_grid(. ~mech_vent)+
  theme(legend.position = "none",
        panel.border = element_blank(),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing.x = unit(2, "lines"))+
  scale_fill_manual(values = c("#abbcea", "#4D81DA"))+
  labs(x="Assessment Tools", y="Percentage (%)", title="Figure 2: Pain Assessment Frequency")+
  geom_text(aes( label = scales::percent(..prop.., accuracy = 1L),
                 y= ..prop..), stat= "count", vjust = -.5, size = 3)

Fig1 <- PPP %>% ggplot(aes(x = Assesstool, group = mech_vent))+
  geom_bar(aes(y = ..prop.., fill = mech_vent), stat="count")+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1L))+
  theme_bw()+
  facet_grid(. ~mech_vent)+
  theme(legend.position = "none",
        panel.border = element_blank(),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing.x = unit(2, "lines"))+
  scale_fill_manual(values = c("#abbcea", "#4D81DA"))+
  scale_x_discrete(limits=positions, labels=c("CPOT", "BPS", "NPS", "Faces", "NVPS", "Non-Val", "None" ))+
  labs(x="Assessment Tools", y="Percentage (%)", title="Figure 1: Pain Assessment Frequency")+
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop..), stat= "count", vjust = -.5, size = 3)

ggsave("Fig1.png", dpi = 300)
