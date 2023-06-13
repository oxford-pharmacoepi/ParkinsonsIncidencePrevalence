##############################################################################
#                                                                            #
#                              Making Denominators                           #
#                                                                            #
##############################################################################
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date(c("2007-01-01", "2021-12-31")),
  ageGroup = list(c(18,150), c(18, 30),c(31,40),c(41,50),c(51,60),c(61,70),c(71,80),c(81,150)),
  sex = c("Female", "Male", "Both"),
  daysPriorHistory = 365,
  verbose = T
)

##############################################################################
#                                                                            #
#                                Prevalence                                  #
#                                                                            #
##############################################################################
prevSubtypes <- estimatePeriodPrevalence(cdm = cdm,
                                  denominatorTable = "denominator",
                                  outcomeTable = subtype_table_name,
                                  outcomeCohortId = outcome_cohorts_subtypes$cohortId,
                                  outcomeCohortName = outcome_cohorts_subtypes$cohortName,
                                  interval = "Years",
                                  minCellCount = 5,
                                  completeDatabaseIntervals = F,
                                  verbose = T)

PrevalenceTableSubtypes <- prevSubtypes %>%
  left_join(settings(prevSubtypes)) 

###creating a folder for the plots
plots.folder <- here("Results", db.name, "Plots")
if (!file.exists(plots.folder)){
  dir.create(plots.folder, recursive = TRUE)}

# 1.Plots for prevalence of subtypes in the overall population
SubtypesPrevalenceOverall<- PrevalenceTableSubtypes %>%
  filter(denominator_cohort_id == 3) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .5, color = NA, show.legend = F) +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Prevalence of parkinsonism in the overall population") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

SubtypesPrevalenceOverallName <- paste0("SubtypesPrevalenceOverallPopulation", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesPrevalenceOverallName),
    width = 7, height = 5)
print(SubtypesPrevalenceOverall, newpage = FALSE)
dev.off()

#2. Plots for prevalence of overall population of different age groups
SubtypesPrevalenceBoth<- PrevalenceTableSubtypes %>%
  filter(denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "(18, 150)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "(18, 30)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "(31, 40)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "(41, 50)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "(51, 60)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "(61, 70)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "(71, 80)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "(81, 150)")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .5, color = NA, show.legend = F) +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Prevalence of parkinsonism in the overall population, stratified by age") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

SubtypesPrevalenceBothName <- paste0("SubtypesPrevalenceBoth", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesPrevalenceBothName),
    width = 7, height = 5)
print(SubtypesPrevalenceBoth, newpage = FALSE)
dev.off()

#3. Plots for prevalence of men of different age groups
SubtypesPrevalenceMale<- PrevalenceTableSubtypes %>%
  filter(denominator_sex == "Male") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "(18, 150)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "(18, 30)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "(31, 40)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "(41, 50)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "(51, 60)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "(61, 70)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "(71, 80)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "(81, 150)")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .5, color = NA, show.legend = F) +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Prevalence of parkinsonism in male") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

SubtypesPrevalenceMaleName <- paste0("SubtypesPrevalenceMale", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesPrevalenceMaleName),
    width = 7, height = 5)
print(SubtypesPrevalenceMale, newpage = FALSE)
dev.off()

#4. Plots for prevalence of women of different age groups
SubtypesPrevalenceFemale<- PrevalenceTableSubtypes %>%
  filter(denominator_sex == "Female") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "(18, 150)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "(18, 30)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "(31, 40)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "(41, 50)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "(51, 60)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "(61, 70)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "(71, 80)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "(81, 150)")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .5, color = NA, show.legend = F) +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Prevalence of parkinsonism in female") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

SubtypesPrevalenceFemaleName <- paste0("SubtypesPrevalenceFemale", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesPrevalenceFemaleName),
    width = 7, height = 5)
print(SubtypesPrevalenceFemale, newpage = FALSE)
dev.off()

#5. Plots for prevalence overall
SubtypesPrevalenceStratifiedByAgeAndSex<- PrevalenceTableSubtypes %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "(18, 150)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "(18, 30)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "(31, 40)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "(41, 50)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "(51, 60)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "(61, 70)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "(71, 80)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "(81, 150)")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .5, color = NA, show.legend = F) +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Prevalence of parkinsonism, stratified by both age and sex") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

SubtypesPrevalenceStratifiedByAgeAndSexName <- paste0("SubtypesPrevalenceStratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesPrevalenceStratifiedByAgeAndSexName),
    width = 7, height = 5)
print(SubtypesPrevalenceStratifiedByAgeAndSex, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                                Incidence                                   #
#                                                                            #
##############################################################################

incSubtypes <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = subtype_table_name,
  outcomeCohortId = outcome_cohorts_subtypes$cohortId,
  outcomeCohortName = outcome_cohorts_subtypes$cohortName,
  interval = "years",
  completeDatabaseIntervals = F,
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = T
)

IncidenceTableSubtypes <- incSubtypes %>% left_join(settings(incSubtypes)) 

# 1.Plots for incidence of parkinsonism in the overall population
SubtypesIncidenceOverall<- IncidenceTableSubtypes %>%
  filter(denominator_cohort_id == 3) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  ggtitle("Incidence of parkinsonism in the overall population") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceOverallName <- paste0("SubtypesIncidenceOverallPopulation", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesIncidenceOverallName),
    width = 7, height = 5)
print(SubtypesIncidenceOverall, newpage = FALSE)
dev.off()

#2. Plots for incidence of overall population of different age groups
SubtypesIncidenceBoth<- IncidenceTableSubtypes %>%
  filter(denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "(18, 150)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "(18, 30)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "(31, 40)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "(41, 50)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "(51, 60)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "(61, 70)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "(71, 80)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "(81, 150)")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Incidence of parkinsonism in the overall population, stratified by age") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceBothName <- paste0("SubtypesIncidenceBoth", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesIncidenceBothName),
    width = 7, height = 5)
print(SubtypesIncidenceBoth, newpage = FALSE)
dev.off()

#3. Plots for incidence of men of different age groups
SubtypesIncidenceMale<- IncidenceTableSubtypes %>%
  filter(denominator_sex == "Male") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "(18, 150)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "(18, 30)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "(31, 40)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "(41, 50)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "(51, 60)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "(61, 70)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "(71, 80)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "(81, 150)")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Incidence of parkinsonism in men, stratified by age") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceMaleName <- paste0("SubtypesIncidenceMale", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesIncidenceMaleName),
    width = 7, height = 5)
print(SubtypesIncidenceMale, newpage = FALSE)
dev.off()

#4. Plots for incidence of women of different age groups
SubtypesIncidenceFemale<- IncidenceTableSubtypes %>%
  filter(denominator_sex == "Female") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "(18, 150)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "(18, 30)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "(31, 40)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "(41, 50)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "(51, 60)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "(61, 70)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "(71, 80)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "(81, 150)")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Incidence of parkinsonism in women, stratified by age") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceFemaleName <- paste0("SubtypesIncidenceFemale", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesIncidenceFemaleName),
    width = 7, height = 5)
print(SubtypesIncidenceFemale, newpage = FALSE)
dev.off()

#5. Plots for incidence overall
SubtypesIncidenceStratifiedByAgeAndSex<- IncidenceTableSubtypes %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "(18, 150)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "(18, 30)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "(31, 40)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "(41, 50)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "(51, 60)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "(61, 70)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "(71, 80)")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "(81, 150)")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Prevalence of parkinsonism, stratified by both age and sex") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceStratifiedByAgeAndSexName <- paste0("SubtypesIncidenceStratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesIncidenceStratifiedByAgeAndSexName),
    width = 7, height = 5)
print(SubtypesIncidenceStratifiedByAgeAndSex, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                            Gather & export                                 #
#                                                                            #
##############################################################################
study_results <- gatherIncidencePrevalenceResults(
  cdm = cdm,
  resultList=list(incSubtypes, prevSubtypes),
  databaseName = db.name)

exportIncidencePrevalenceResults(result=study_results,
                                 zipName= paste0(db.name, "IncidencePrevalenceResultsSubtypes"), 
                                 outputFolder=here::here("Results", db.name))
