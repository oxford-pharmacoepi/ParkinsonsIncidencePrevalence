##############################################################################
#                                                                            #
#                              Making Denominators                           #
#                                                                            #
##############################################################################
info(logger, 'MAKING DENOMINATORS FOR THE GENERAL POPULATION')
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_general_pop",
  cohortDateRange = as.Date(c("2007-01-01", "2021-12-31")),
  ageGroup = list(c(18,150), c(18, 30),c(31,40),c(41,50),c(51,60),c(61,70),c(71,80),c(81,150)),
  sex = c("Female", "Male", "Both"),
  daysPriorHistory = 365
)

##############################################################################
#                                                                            #
#                                Prevalence                                  #
#                                                                            #
##############################################################################
info(logger, 'PREVALENCE OF SUBTYPES FOR THE GENERAL POPULATION')
prevSubtypes <- estimatePeriodPrevalence(cdm = cdm,
                                  denominatorTable = "denominator_general_pop",
                                  outcomeTable = "parkinson_subtypes")

###creating a folder for the plots
plots.folder <- here("Results", db.name, "Plots")
if (!file.exists(plots.folder)){
  dir.create(plots.folder, recursive = TRUE)}

# 1.Plots for prevalence of subtypes in the overall population
SubtypesPrevalenceOverall<- prevSubtypes %>%
  filter(denominator_age_group == '18 to 150', denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>% 
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, color = outcome_cohort_name)) +
  scale_color_manual(values = c("DIP" = "black", "PD" = "red", "Parkinsonism" = "blue", "VP" = "#8839A2"), 
                     name = "fill")+
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper), alpha = 0.5, show.legend = F) +
  geom_point(aes(colour = outcome_cohort_name), size = 6) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) + 
  labs(colour = "Parkinsonism and subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face="bold"),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 1) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text=element_text(size=15, face = "bold"),
        legend.title = element_text(size=20, face = "bold"),
        plot.title = element_text(hjust = 1)) +
  xlab("Time") + ylab("Prevalence")

SubtypesPrevalenceOverallName <- paste0("SubtypesPrevalenceOverallPopulation", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceOverallName), width = 18, height = 10, units = "in", res = 1500)
print(SubtypesPrevalenceOverall, newpage = FALSE)
dev.off()

#2. Plots for prevalence - stratified by sex
SubtypesPrevalenceSex<- prevSubtypes %>%
  filter(denominator_age_group == '18 to 150' & (denominator_sex != "Both")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = factor(outcome_cohort_name), color = factor(outcome_cohort_name))) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper), alpha = 0.5, show.legend = F) +
  geom_point(aes(colour = factor(outcome_cohort_name)), size = 6) +
  facet_wrap(~denominator_sex) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) + 
  labs(colour = "Parkinsonism and subtypes") +
  scale_color_manual(values = c("black", "red", "blue", "#8839A2"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face="bold"),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 1) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text=element_text(size=15, face = "bold"),
        legend.title = element_text(size=20, face = "bold"),
        strip.text.x = element_text(size = 15, face = "bold")) +
  xlab("Time") + ylab("Prevalence")

SubtypesPrevalenceSexName <- paste0("SubtypesPrevalenceStratifiedBySex", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceSexName), width = 18, height = 10, units = "in", res = 1500)
print(SubtypesPrevalenceSex, newpage = FALSE)
dev.off()

#3. Plots for prevalence - stratified by age
SubtypesPrevalenceAge<- prevSubtypes %>%
  filter(denominator_sex == "Both") %>%
  filter(!(denominator_age_group %in% c("18 to 150", "18 to 30", "31 to 40"))) %>% 
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18","18 to 30","31 to 40", "41 to 50", "51 to 60", "61 to 70", "71 to 80", "Over 80"))) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = factor(outcome_cohort_name), color = factor(outcome_cohort_name))) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper), alpha = 0.5, show.legend = F) +
  geom_point(aes(colour = factor(outcome_cohort_name)), size = 3) +
  facet_wrap(~denominator_age_group) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) + 
  labs(colour = "Parkinsonism and subtypes") +
  scale_color_manual(values = c("black", "red", "blue", "#8839A2"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face="bold"),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 1) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text=element_text(size=15, face = "bold"),
        legend.title = element_text(size=20, face = "bold"),
        strip.text.x = element_text(size = 15, face = "bold")) +
  xlab("Time") + ylab("Prevalence")

SubtypesPrevalenceAgeName <- paste0("SubtypesPrevalenceStratifiedByAge", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceAgeName), width = 18, height = 10, units = "in", res = 1500)
print(SubtypesPrevalenceAge, newpage = FALSE)
dev.off()

#4. Plots for prevalence - stratified by both age and sex
SubtypesPrevalenceStratifiedByAgeAndSex<- prevSubtypes %>%
  dplyr::filter(!denominator_age_group %in% c("18 to 30", "31 to 40", "18 to 150")) %>% 
  filter(denominator_sex!="Both") %>% 
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
  mutate(across(denominator_age_group, factor, levels=c("41 to 50", "51 to 60", "61 to 70", "71 to 80", "Over 80"))) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper), alpha = .5, color = NA, show.legend = F) +
  geom_point(aes(colour = factor(outcome_cohort_name)), size = 3) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_sex ~denominator_age_group) +
  labs(colour = "Parkinsonism and subtypes") +
  scale_color_manual(values = c("black", "red", "blue", "#8839A2"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face="bold"),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 1) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text=element_text(size=15, face = "bold"),
        legend.title = element_text(size=20, face = "bold"),
        strip.text.x = element_text(size = 15, face = "bold"),
        strip.text.y = element_text(size = 15, face = "bold")) +
  xlab("Time") + ylab("Prevalence")

SubtypesPrevalenceAgeSexName <- paste0("SubtypesPrevalenceStratifiedByAgeAndSex", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceAgeSexName), width = 18, height = 10, units = "in", res = 1500)
print(SubtypesPrevalenceStratifiedByAgeAndSex, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                                Incidence                                   #
#                                                                            #
##############################################################################
info(logger, 'INCIDENCE OF SUBTYPES FOR THE GENERAL POPULATION')
incSubtypes <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_general_pop",
  outcomeTable = "parkinson_subtypes")

# 1.Plots for incidence of parkinsonism in the overall population
SubtypesIncidenceOverall<- incSubtypes %>%
  filter(denominator_age_group == '18 to 150', denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(labels = label_comma()) +
  ggtitle("Incidence of parkinsonism in the overall population") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceOverallName <- paste0("SubtypesIncidenceOverallPopulation", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesIncidenceOverallName),
    width = 10, height = 8)
print(SubtypesIncidenceOverall, newpage = FALSE)
dev.off()

#2. Plots for incidence of overall population of different age groups
SubtypesIncidenceBoth<- incSubtypes %>%
  filter(denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "Over 18")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18","18 to 30","31 to 40", "41 to 50", "51 to 60", "61 to 70", "71 to 80", "Over 80"))) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Incidence of parkinsonism in the overall population, stratified by age") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceBothName <- paste0("SubtypesIncidenceBoth", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesIncidenceBothName),
    width = 10, height = 8)
print(SubtypesIncidenceBoth, newpage = FALSE)
dev.off()

#3. Plots for incidence of men of different age groups
SubtypesIncidenceMale<- incSubtypes %>%
  filter(denominator_sex == "Male") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "Over 18")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18","18 to 30","31 to 40", "41 to 50", "51 to 60", "61 to 70", "71 to 80", "Over 80"))) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Incidence of parkinsonism in men, stratified by age") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceMaleName <- paste0("SubtypesIncidenceMale", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesIncidenceMaleName),
    width = 10, height = 8)
print(SubtypesIncidenceMale, newpage = FALSE)
dev.off()

#4. Plots for incidence of women of different age groups
SubtypesIncidenceFemale<- incSubtypes %>%
  filter(denominator_sex == "Female") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "Over 18")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18","18 to 30","31 to 40", "41 to 50", "51 to 60", "61 to 70", "71 to 80", "Over 80"))) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~denominator_age_group) +
  ggtitle("Incidence of parkinsonism in women, stratified by age") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceFemaleName <- paste0("SubtypesIncidenceFemale", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesIncidenceFemaleName),
    width = 10, height = 8)
print(SubtypesIncidenceFemale, newpage = FALSE)
dev.off()

#5. Plots for incidence overall
SubtypesIncidenceStratifiedByAgeAndSex<- incSubtypes %>%
  dplyr::filter(!denominator_age_group %in% c("18 to 30", "31 to 40")) %>% 
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "Drug Induced Parkinsonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "Vascular Parkisonism")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "Parkinson's Disease")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "Over 18")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18", "41 to 50", "51 to 60", "61 to 70", "71 to 80", "Over 80"))) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  facet_grid(~denominator_sex ~denominator_age_group) +
  ggtitle("Incidence of parkinsonism, stratified by both age and sex") + 
  labs(colour = "Parkinsonism and its subtypes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = label_comma()) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

SubtypesIncidenceStratifiedByAgeAndSexName <- paste0("SubtypesIncidenceStratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", SubtypesIncidenceStratifiedByAgeAndSexName),
    width = 20, height = 8)
print(SubtypesIncidenceStratifiedByAgeAndSex, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                            Gather & export                                 #
#                                                                            #
##############################################################################
info(logger, 'GATHERING RESULTS FOR SUBTYPES FOR THE GENERAL POPULATION')
exportIncidencePrevalenceResults(resultList = list("Prevalence of Subtypes" = prevSubtypes, "Incidence of Subtypes" = incSubtypes),
                                 zipName= paste0(db.name, "_IncidencePrevalenceResultsSubtypes"), 
                                 outputFolder=here::here("Results", db.name))
