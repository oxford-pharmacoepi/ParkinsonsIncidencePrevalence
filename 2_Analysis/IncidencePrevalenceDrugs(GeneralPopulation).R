##############################################################################
#                                                                            #
#                                Prevalence                                  #
#                                                                            #
##############################################################################
info(logger, 'PREVALENCE OF ANTIPARKISNON DRUGS FOR THE GENERAL POPULATION')
prevDrugs <- estimatePeriodPrevalence(cdm = cdm,
                                      denominatorTable = "denominator_general_pop",
                                      outcomeTable = drug_table_name,                                       interval = "Years",
                                      outcomeLookbackDays = 30) #discuss

# 1.Plots for prevalence of the drug use in the overall population
DrugsPrevalenceOverall<- PrevalenceTableDrugs %>%
  filter(denominator_age_group == '18;150', denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonismPrevalent", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonismPrevalent", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDiseasePrevalent", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonismPrevalent", "Parkinsonism")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .5, color = NA, show.legend = F) +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Prevalence of antiparkinson drugs in the overall population") + 
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceOverallName <- paste0("DrugsPrevalenceOverallPopulation", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceOverallName),
    width = 10, height = 8)
print(DrugsPrevalenceOverall, newpage = FALSE)
dev.off()

#2. Plots for prevalence of overall population of different age groups
DrugsPrevalenceBoth<- PrevalenceTableDrugs %>%
  filter(denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonismPrevalent", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonismPrevalent", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDiseasePrevalent", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonismPrevalent", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .5, color = NA, show.legend = F) +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Prevalence of antiparkinson drugs in the overall population, stratified by age") + 
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceBothName <- paste0("DrugsPrevalenceBoth", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceBothName),
    width = 10, height = 8)
print(DrugsPrevalenceBoth, newpage = FALSE)
dev.off()

#3. Plots for prevalence of men of different age groups
DrugsPrevalenceMale<- PrevalenceTableDrugs %>%
  filter(denominator_sex == "Male") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonismPrevalent", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonismPrevalent", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDiseasePrevalent", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonismPrevalent", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .5, color = NA, show.legend = F) +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Prevalence of antiparkinson drugs in male, stratified by age") + 
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceMaleName <- paste0("DrugsPrevalenceMale", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceMaleName),
    width = 10, height = 8)
print(DrugsPrevalenceMale, newpage = FALSE)
dev.off()

#4. Plots for prevalence of women of different age groups
DrugsPrevalenceFemale<- PrevalenceTableDrugs %>%
  filter(denominator_sex == "Female") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonismPrevalent", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonismPrevalent", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDiseasePrevalent", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonismPrevalent", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .5, color = NA, show.legend = F) +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Prevalence of antiparkinson drugs in female, stratified by age") + 
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceFemaleName <- paste0("DrugsPrevalenceFemale", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceFemaleName),
    width = 10, height = 8)
print(DrugsPrevalenceFemale, newpage = FALSE)
dev.off()

#5. Plots for prevalence overall
DrugsPrevalenceStratifiedByAgeAndSex<- PrevalenceTableDrugs %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonismPrevalent", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonismPrevalent", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDiseasePrevalent", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonismPrevalent", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .5, color = NA, show.legend = F) +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Prevalence of antiparkinson drugs, stratified by both age and sex") + 
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceStratifiedByAgeAndSexName <- paste0("DrugsPrevalenceStratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceStratifiedByAgeAndSexName),
    width = 20, height = 8)
print(DrugsPrevalenceStratifiedByAgeAndSex, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                                Incidence                                   #
#                                                                            #
##############################################################################
info(logger, 'INCIDENCE OF ANTIPARKINSON FOR THE GENERAL POPULATION')
incDrugs <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_general_pop",
  outcomeTable = drug_table_name, 
  interval = "years",
  outcomeWashout = 30, #discuss
  repeatedEvents = TRUE #discuss
)

# 1.Plots for incidence of the drug use in the overall population
DrugsIncidenceOverall<- IncidenceTableDrugs %>%
  filter(denominator_age_group == '18;150', denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonismPrevalent", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonismPrevalent", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDiseasePrevalent", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonismPrevalent", "Parkinsonism")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  ggtitle("Incidence of antiparkinson drugs in the overall population") + 
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")
theme(plot.title = element_text(hjust = 0.5)) +

DrugsIncidenceOverallName <- paste0("DrugsIncidenceOverallPopulation", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceOverallName),
    width = 10, height = 8)
print(DrugsIncidenceOverall, newpage = FALSE)
dev.off()

#2. Plots for incidence of overall population of different age groups
DrugsIncidenceBoth<- IncidenceTableDrugs %>%
  filter(denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonismPrevalent", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonismPrevalent", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDiseasePrevalent", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonismPrevalent", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Incidence of antiparkinson drugs in the overall population, stratified by age") + 
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceBothName <- paste0("DrugsIncidenceBoth", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceBothName),
    width = 10, height = 8)
print(DrugsIncidenceBoth, newpage = FALSE)
dev.off()

#3. Plots for incidence of men of different age groups
DrugsIncidenceMale<- IncidenceTableDrugs %>%
  filter(denominator_sex == "Male") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonismPrevalent", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonismPrevalent", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDiseasePrevalent", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonismPrevalent", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Incidence of antiparkinson drugs in men, stratified by age") + 
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceMaleName <- paste0("DrugsIncidenceMale", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceMaleName),
    width = 10, height = 8)
print(DrugsIncidenceMale, newpage = FALSE)
dev.off()

#4. Plots for incidence of women of different age groups
DrugsIncidenceFemale<- IncidenceTableDrugs %>%
  filter(denominator_sex == "Female") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonismPrevalent", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonismPrevalent", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDiseasePrevalent", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonismPrevalent", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Incidence of antiparkinson drugs in women, stratified by age") + 
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceFemaleName <- paste0("DrugsIncidenceFemale", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceFemaleName),
    width = 10, height = 8)
print(DrugsIncidenceFemale, newpage = FALSE)
dev.off()

#5. Plots for incidence overall
DrugsIncidenceStratifiedByAgeAndSex<- IncidenceTableDrugs %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonismPrevalent", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonismPrevalent", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDiseasePrevalent", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonismPrevalent", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Prevalence of antiparkinson drugs, stratified by both age and sex") + 
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceStratifiedByAgeAndSexName <- paste0("DrugsIncidenceStratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceStratifiedByAgeAndSexName),
    width = 20, height = 8)
print(DrugsIncidenceStratifiedByAgeAndSex, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                            Gather & export                                 #
#                                                                            #
##############################################################################
info(logger, 'GATHERING RESULTS FOR ANTIPARKINSON FOR THE GENERAL POPULATION')
study_results <- gatherIncidencePrevalenceResults(
  cdm = cdm,
  resultList=list(incDrugs, prevDrugs),
  databaseName = db.name) 

exportIncidencePrevalenceResults(result=study_results,
                                 zipName= paste0(db.name, "IncidencePrevalenceResultsDrugs"), 
                                 outputFolder=here::here("Results", db.name))
