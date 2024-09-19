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

SubtypesIncidenceOverallName <- paste0("SubtypesIncidenceOverallPopulation", ".tiff")
ggsave(file = here(ip_subtypes_paper_plots, SubtypesIncidenceOverallName), width = 10, height = 6.5, device = "tiff", dpi = 300)

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

SubtypesIncidenceBySexName <- paste0("SubtypesIncidenceBySex", ".tiff")
ggsave(file = here(ip_subtypes_paper_plots, SubtypesIncidenceBySexName), width = 10, height = 6.5, device = "tiff", dpi = 300)

#3. incidence by age
for (subtype in c("ParkinsonsDisease", "Parkinsonism")){
incSubtypes %>%
  filter(!(denominator_age_group %in% c("18 to 150", "18 to 30", "31 to 40")) & (denominator_sex == "Both") & outcome_cohort_name == subtype) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = denominator_age_group)) +
  geom_point(aes(), size = 4) +
  geom_errorbar(width=100)+
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~denominator_age_group, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face="bold"),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 1) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 1),
        legend.position="none") +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceByAge <- paste0("SubtypesIncidenceAge", subtype, ".tiff")
ggsave(file = here(ip_subtypes_paper_plots, SubtypesIncidenceByAge), width = 10, height = 6.5, units = "in", device = "tiff", dpi = 300)
}

for (subtype in c("DrugInducedParkinsonism", "VascularParkinsonism")){
  incSubtypes %>%
    filter(!(denominator_age_group %in% c("18 to 150", "18 to 30", "31 to 40", "41 to 50", "51 to 60")) & (denominator_sex == "Both") & outcome_cohort_name == subtype) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
    ggplot(aes(x = incidence_start_date, y = incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = denominator_age_group)) +
    geom_point(aes(), size = 4) +
    geom_errorbar(width=100)+
    scale_y_continuous(labels = label_comma()) +
    facet_wrap(~denominator_age_group, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
          axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 20, face="bold"),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 1) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          strip.text.x = element_text(size = 20, face = "bold"),
          strip.text.y = element_text(size = 20, face = "bold"),
          plot.title = element_text(hjust = 1),
          legend.position="none") +
    xlab("Time") + ylab("Incidence (per 100,000 person-years)")
  
  SubtypesIncidenceByAge <- paste0("SubtypesIncidenceAge", subtype, ".tiff")
  ggsave(file = here(ip_subtypes_paper_plots, SubtypesIncidenceByAge), width = 10, height = 6.5, device = "tiff", dpi = 300)
}

#4. incidence by age and sex
for (subtype in c("ParkinsonsDisease", "Parkinsonism")){
  incSubtypes %>%
    filter(!(denominator_age_group %in% c("18 to 150", "18 to 30", "31 to 40")) & (denominator_sex != "Both") & outcome_cohort_name == subtype) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
    ggplot(aes(x = incidence_start_date, y = incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = denominator_age_group, color = denominator_sex)) +
    scale_color_manual(values = c("Female" = "red", "Male" = "blue"))+
    geom_point(aes(), size = 4) +
    geom_errorbar(width=100)+
    labs(colour = "Sex") +
    scale_y_continuous(labels = label_comma()) +
    facet_wrap(~denominator_age_group, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
          axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 20, face="bold"),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 1) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          strip.text.x = element_text(size = 20, face = "bold"),
          strip.text.y = element_text(size = 20, face = "bold"),
          plot.title = element_text(hjust = 1),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          legend.text=element_text(size=20, face = "bold"),
          legend.title = element_text(size=20, face = "bold")) +
    xlab("Time") + ylab("Incidence (per 100,000 person-years)")
  
  SubtypesIncidenceByAgeSex <- paste0("SubtypesIncidenceAgeSex", subtype, ".tiff")
  ggsave(file = here(ip_subtypes_paper_plots, SubtypesIncidenceByAgeSex), width = 10, height = 6.5, device = "tiff", dpi = 300)
}

for (subtype in c("DrugInducedParkinsonism", "VascularParkinsonism")){
  incSubtypes %>%
    filter(!(denominator_age_group %in% c("18 to 150", "18 to 30", "31 to 40", "41 to 50", "51 to 60")) & (denominator_sex != "Both") & outcome_cohort_name == subtype) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
    ggplot(aes(x = incidence_start_date, y = incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = denominator_age_group, color = denominator_sex)) +
    scale_color_manual(values = c("Female" = "red", "Male" = "blue"))+
    geom_point(aes(), size = 4) +
    geom_errorbar(width=100)+
    labs(colour = "Sex") +
    scale_y_continuous(labels = label_comma()) +
    facet_wrap(~denominator_age_group, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
          axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 20, face="bold"),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 1) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          strip.text.x = element_text(size = 20, face = "bold"),
          strip.text.y = element_text(size = 20, face = "bold"),
          plot.title = element_text(hjust = 1),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          legend.text=element_text(size=20, face = "bold"),
          legend.title = element_text(size=20, face = "bold")) +
    xlab("Time") + ylab("Incidence (per 100,000 person-years)")
  
  SubtypesIncidenceByAgeSex <- paste0("SubtypesIncidenceAgeSex", subtype, ".tiff")
  ggsave(file = here(ip_subtypes_paper_plots, SubtypesIncidenceByAgeSex), width = 10, height = 6.5, device = "tiff", dpi = 300)
}

#### prev
#1. overall prevalence
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

SubtypesPrevalenceOverallName <- paste0("SubtypesPrevalenceOverallPopulation", ".tiff")
ggsave(file = here(ip_subtypes_paper_plots, SubtypesPrevalenceOverallName), width = 10, height = 6.5, device = "tiff", dpi = 300)

#2. prevalence by sex
prevSubtypes %>%
  filter(denominator_age_group == '18 to 150' & (denominator_sex != "Both")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>% 
  mutate(outcome_cohort_name = factor(outcome_cohort_name, c("Parkinsonism", "PD", "VP", "DIP"))) %>% 
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = denominator_sex, color = denominator_sex)) +
  scale_color_manual(values = c("Female" = "red", "Male" = "blue"))+
  geom_point(aes(), size = 4) +
  geom_errorbar(width=100)+
  labs(colour = "Sex") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) + 
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
  xlab("Time") + ylab("Prevalence")

SubtypesPrevalenceBySexName <- paste0("SubtypesPrevalenceBySex", ".tiff")
ggsave(file = here(ip_subtypes_paper_plots, SubtypesPrevalenceBySexName), width = 10, height = 6.5, device = "tiff",dpi = 300)

#3. prevalence by age
for (subtype in c("ParkinsonsDisease", "Parkinsonism")){
prevSubtypes %>%
  filter(!(denominator_age_group %in% c("18 to 150", "18 to 30", "31 to 40")) & (denominator_sex == "Both") & outcome_cohort_name == subtype) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>% 
  mutate(outcome_cohort_name = factor(outcome_cohort_name, c("Parkinsonism", "PD", "VP", "DIP"))) %>% 
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = denominator_age_group)) +
  geom_point(aes(), size = 4) +
  geom_errorbar(width=100)+
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) + 
  facet_wrap(~denominator_age_group, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
          axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 20, face="bold"),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 1) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          strip.text.x = element_text(size = 20, face = "bold"),
          strip.text.y = element_text(size = 20, face = "bold"),
          plot.title = element_text(hjust = 1),
          legend.position="none") +
  xlab("Time") + ylab("Prevalence")
  
  SubtypesPrevalenceByAge <- paste0("SubtypesPrevalenceAge", subtype, ".tiff")
  ggsave(file = here(ip_subtypes_paper_plots, SubtypesPrevalenceByAge), width = 10, height = 6.5, device = "tiff", dpi = 300)
}

for (subtype in c("DrugInducedParkinsonism", "VascularParkinsonism")){
  prevSubtypes %>%
    filter(!(denominator_age_group %in% c("18 to 150", "18 to 30", "31 to 40", "41 to 50", "51 to 60")) & (denominator_sex == "Both") & outcome_cohort_name == subtype) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>% 
    mutate(outcome_cohort_name = factor(outcome_cohort_name, c("Parkinsonism", "PD", "VP", "DIP"))) %>% 
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
    ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = denominator_age_group)) +
    geom_point(aes(), size = 4) +
    geom_errorbar(width=100)+
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, NA)
    ) + 
    facet_wrap(~denominator_age_group, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
          axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 20, face="bold"),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 1) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          strip.text.x = element_text(size = 20, face = "bold"),
          strip.text.y = element_text(size = 20, face = "bold"),
          plot.title = element_text(hjust = 1),
          legend.position="none") +
    xlab("Time") + ylab("Prevalence")
  
  SubtypesPrevalenceByAge <- paste0("SubtypesPrevalenceAge", subtype, ".tiff")
  ggsave(file = here(ip_subtypes_paper_plots, SubtypesPrevalenceByAge), width = 10, height = 6.5, device = "tiff", dpi = 300)
}

#4. prevalence by age and sex
for (subtype in c("ParkinsonsDisease", "Parkinsonism")){
  prevSubtypes %>%
    filter(!(denominator_age_group %in% c("18 to 150", "18 to 30", "31 to 40")) & (denominator_sex != "Both") & outcome_cohort_name == subtype) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>% 
    mutate(outcome_cohort_name = factor(outcome_cohort_name, c("Parkinsonism", "PD", "VP", "DIP"))) %>% 
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
    ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = denominator_age_group, color = denominator_sex)) +
    scale_color_manual(values = c("Female" = "red", "Male" = "blue"))+
    geom_point(aes(), size = 4) +
    geom_errorbar(width=100)+
    labs(colour = "Sex") +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, NA)
    ) + 
    facet_wrap(~denominator_age_group, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
          axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 20, face="bold"),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 1) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          strip.text.x = element_text(size = 20, face = "bold"),
          strip.text.y = element_text(size = 20, face = "bold"),
          plot.title = element_text(hjust = 1),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          legend.text=element_text(size=20, face = "bold"),
          legend.title = element_text(size=20, face = "bold")) +
    xlab("Time") + ylab("Prevalence")
  
  SubtypesPrevalenceByAgeSex <- paste0("SubtypesPrevalenceAgeSex", subtype, ".tiff")
  ggsave(file = here(ip_subtypes_paper_plots, SubtypesPrevalenceByAgeSex), width = 10, height = 6.5, device = "tiff", dpi = 300)
}

for (subtype in c("DrugInducedParkinsonism", "VascularParkinsonism")){
  prevSubtypes %>%
    filter(!(denominator_age_group %in% c("18 to 150", "18 to 30", "31 to 40", "41 to 50", "51 to 60")) & (denominator_sex != "Both") & outcome_cohort_name == subtype) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>% 
    mutate(outcome_cohort_name = factor(outcome_cohort_name, c("Parkinsonism", "PD", "VP", "DIP"))) %>% 
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
    ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = denominator_age_group, color = denominator_sex)) +
    scale_color_manual(values = c("Female" = "red", "Male" = "blue"))+
    geom_point(aes(), size = 4) +
    geom_errorbar(width=100)+
    labs(colour = "Sex") +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, NA)
    ) + 
    facet_wrap(~denominator_age_group, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
          axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 20, face="bold"),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 1) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          strip.text.x = element_text(size = 20, face = "bold"),
          strip.text.y = element_text(size = 20, face = "bold"),
          plot.title = element_text(hjust = 1),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          legend.text=element_text(size=20, face = "bold"),
          legend.title = element_text(size=20, face = "bold")) +
    xlab("Time") + ylab("Prevalence")
  
  SubtypesPrevalenceByAgeSex <- paste0("SubtypesPrevalenceAgeSex", subtype, ".tiff")
  ggsave(file = here(ip_subtypes_paper_plots, SubtypesPrevalenceByAgeSex), width = 10, height = 6.5, device = "tiff", dpi = 300)
}
