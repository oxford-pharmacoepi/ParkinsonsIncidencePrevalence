#Inc
#overall incidence 
ip_subtypes_paper_plots <- here("Results", db.name, "paper_ip_subtypes_plots")

if (!dir.exists(ip_subtypes_paper_plots)) {
  dir.create(ip_subtypes_paper_plots)
}

incSubtypes %>%
  filter(denominator_age_group == '18 to 150', denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  mutate(outcome_cohort_name = factor(outcome_cohort_name, c("Parkinsonism", "PD", "VP", "DIP"))) %>% 
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper)) +
  geom_point(aes(), size = 4) +
  geom_errorbar(width=100)+
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~outcome_cohort_name, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face="bold"),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 1) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        legend.position="none",
        plot.title = element_text(hjust = 1)) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceOverallName <- paste0("SubtypesIncidenceOverallPopulation", ".png")
ggsave(file = here(ip_subtypes_paper_plots, SubtypesIncidenceOverallName), width = 18, height = 10, dpi = 600)

#2. incidence by sex
incSubtypes %>%
  filter(denominator_age_group == '18 to 150' & (denominator_sex != "Both")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  mutate(outcome_cohort_name = factor(outcome_cohort_name, c("Parkinsonism", "PD", "VP", "DIP"))) %>% 
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = denominator_sex, color = denominator_sex)) +
  scale_color_manual(values = c("Female" = "red", "Male" = "blue"))+
  geom_point(aes(colour = denominator_sex), size = 4) +
  geom_errorbar(width=100)+
  scale_y_continuous(labels = label_comma()) +
  labs(colour = "Sex") +
  facet_wrap(~outcome_cohort_name, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face="bold"),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 1) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text=element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face = "bold"),
        plot.title = element_text(hjust = 1)) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceBySexName <- paste0("SubtypesIncidenceBySex", ".png")
ggsave(file = here(ip_subtypes_paper_plots, SubtypesIncidenceBySexName), width = 18, height = 10, dpi = 600)

#3. incidence by age
for (subtype in c("DrugInducedParkinsonism", "VascularParkinsonism", "ParkinsonsDisease", "Parkinsonism")){
incSubtypes %>%
  filter(!(denominator_age_group %in% c("18 to 150", "18 to 30", "31 to 40", "41 to 50")) & (denominator_sex == "Both") & outcome_cohort_name == subtype) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = denominator_age_group, color = denominator_age_group)) +
  geom_point(aes(colour = denominator_age_group), size = 4) +
  geom_errorbar(width=100)+
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~denominator_age_group, scales = "free") +
  labs(colour = "Age Bands") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face="bold"),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 1) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text=element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face = "bold"),
        plot.title = element_text(hjust = 1)) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceMaleName <- paste0("SubtypesIncidence", subtype, ".png")
ggsave(file = here(ip_subtypes_paper_plots, SubtypesIncidenceBySexName), width = 18, height = 10, dpi = 600)
}

#4. 
SubtypesIncidenceFemaleName <- paste0("SubtypesIncidenceFemale", ".png")
png(here("Results", db.name, "Plots", SubtypesIncidenceFemaleName),
    width = 10, height = 8, units = "in", res = 1200)
print(SubtypesIncidenceFemale, newpage = FALSE)
dev.off()

#5.
SubtypesIncidenceStratifiedByAgeAndSexName <- paste0("SubtypesIncidenceStratifiedByAgeAndSex", ".png")
png(here("Results", db.name, "Plots", SubtypesIncidenceFemaleName),
    width = 10, height = 8, units = "in", res = 1200)
print(SubtypesIncidenceStratifiedByAgeAndSex, newpage = FALSE)
dev.off()


#### prev
#1. 
prevSubtypes %>%
  filter(denominator_age_group == '18 to 150', denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>% 
  mutate(outcome_cohort_name = factor(outcome_cohort_name, c("Parkinsonism", "PD", "VP", "DIP"))) %>% 
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper)) +
  geom_point(aes(), size = 4) +
  geom_errorbar(width=100)+
  facet_wrap(~outcome_cohort_name, scales = "free") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face="bold"),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 1) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        legend.position="none",
        plot.title = element_text(hjust = 1)) +
  xlab("Time") + ylab("Prevalence")

SubtypesPrevalenceOverallName <- paste0("SubtypesPrevalenceOverallPopulation", ".png")
ggsave(file = here(ip_subtypes_paper_plots, SubtypesPrevalenceOverallName), width = 18, height = 10, dpi = 600)

#2. 
SubtypesPrevalenceBothName <- paste0("SubtypesPrevalenceBoth", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceBothName), width = 10, height = 5, units = "in", res = 1200)
print(SubtypesPrevalenceBoth, newpage = FALSE)
dev.off()

#3. 
SubtypesPrevalenceMaleName <- paste0("SubtypesPrevalenceMale", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceMaleName),
    width = 10, height = 5, units = "in", res = 1200)
print(SubtypesPrevalenceMale, newpage = FALSE)
dev.off()

#4. 
SubtypesPrevalenceFemaleName <- paste0("SubtypesPrevalenceFemale", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceFemaleName),
    width = 10, height = 5, units = "in", res = 1200)
print(SubtypesPrevalenceFemale, newpage = FALSE)
dev.off()

#5. 
SubtypesPrevalenceStratifiedByAgeAndSexName <- paste0("SubtypesPrevalenceStratifiedByAgeAndSex", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceStratifiedByAgeAndSexName),
    width = 10, height = 5, units = "in", res = 1200)
print(SubtypesPrevalenceStratifiedByAgeAndSex, newpage = FALSE)
dev.off()

