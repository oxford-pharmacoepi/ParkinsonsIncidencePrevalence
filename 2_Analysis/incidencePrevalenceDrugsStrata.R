##############################################################################
#                                                                            #
#                               Set up                                       #
#                                                                            #
##############################################################################
install.packages("remotes")
remotes::install_github("darwin-eu/IncidencePrevalence")
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library("IncidencePrevalence")

#This script is tailored for the subtype in the strata table with cohortId = 1.
#Throughout the script, I have named it arbitrarily as subtype 1.

##############################################################################
#                                                                            #
#                              Making Denominators                           #
#                                                                            #
##############################################################################
cdm$denominatorSubtype1 <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2007-01-01"),
  ageGroup = list(c(18,150),c(18, 30),c(31,40),c(41,50),c(51,60),c(61,70),c(71,80),c(81,150)),
  sex = c("Female", "Male", "Both"),
  daysPriorHistory = 365,
  strataTable = "...", #The name of your choice for the table associated to parkinsonism subtypes
  strataCohortId = 1,
  strataCohortName = "...", #Name for the subtype corresponding to CohortId = 1 in strataTable
  verbose = T
)

##############################################################################
#                                                                            #
#                                Prevalence                                  #
#                                                                            #
##############################################################################
prevSubtype1 <- estimatePeriodPrevalence(cdm = cdm,
                                    denominatorTable = "denominatorSubtype1",
                                    outcomeTable = "...", ##The name of your choice for the table associated to antiparkinson drugs
                                    outcomeCohortId = outcome_cohorts_drugs$cohortId,
                                    outcomeCohortName = outcome_cohorts_drugs$cohortName,
                                    interval = "Years",
                                    minCellCount = 5,
                                    completeDatabaseIntervals = F,
                                    verbose = T)
PrevalenceTableSubtype1 <- prevSubtype1 %>%
  left_join(settings(prevSubtype1))

# 1.Plots for prevalence of the drug use in subtype 1
DrugsPrevalenceOverallSubtype1<- PrevalenceTableSubtype1 %>%
  filter(denominator_cohort_id == 3) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Prevalence of antiparkinson drugs in people with subtype 1") +
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceOverallSubtype1Name <- paste0("DrugsPrevalenceSubtype1", ".pdf")

pdf(here("2_AnalysisIncidencePrevalence", "Plots", DrugsPrevalenceOverallSubtype1Name),
    width = 7, height = 5)
print(DrugsPrevalenceOverallSubtype1, newpage = FALSE)
dev.off()

# 2.Plots for prevalence of the drug use in subtype 1, stratified by sex
DrugsPrevalenceBothSubtype1<- PrevalenceTableSubtype1 %>%
  filter(denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "18 + years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "18-30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "31-40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "41-50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "51-60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "61-70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "71-80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "81 + years old")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  labs(title = str_wrap("Prevalence of antiparkinson drugs in people with subtype 1, stratified by age", 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceBothSubtype1Name <- paste0("DrugsPrevalenceSubtype1Both", ".pdf")

pdf(here("2_AnalysisIncidencePrevalence", "Plots", DrugsPrevalenceBothSubtype1Name),
    width = 7, height = 5)
print(DrugsPrevalenceBothSubtype1, newpage = FALSE)
dev.off()

# 3.Plots for prevalence of the drug use in men with subtype 1, stratified by age
DrugsPrevalenceMaleSubtype1<- PrevalenceTableSubtype1 %>%
  filter(denominator_sex == "Male") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "18 + years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "18-30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "31-40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "41-50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "51-60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "61-70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "71-80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "81 + years old")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  labs(title = str_wrap("Prevalence of antiparkinson drugs in men with subtype 1, stratified by age", 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceMaleSubtype1Name <- paste0("DrugsPrevalenceSubtype1Male", ".pdf")

pdf(here("2_AnalysisIncidencePrevalence", "Plots", DrugsPrevalenceMaleSubtype1Name),
    width = 7, height = 5)
print(DrugsPrevalenceMaleSubtype1, newpage = FALSE)
dev.off()

# 4.Plots for prevalence of the drug use in women with subtype 1, stratified by age
DrugsPrevalenceFemaleSubtype1<- PrevalenceTableSubtype1 %>%
  filter(denominator_sex == "Female") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "18 + years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "18-30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "31-40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "41-50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "51-60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "61-70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "71-80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "81 + years old")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  labs(title = str_wrap("Prevalence of antiparkinson drugs in women with subtype 1, stratified by age", 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceFemaleSubtype1Name <- paste0("DrugsPrevalenceSubtype1Female", ".pdf")

pdf(here("2_AnalysisIncidencePrevalence", "Plots", DrugsPrevalenceFemaleSubtype1Name),
    width = 7, height = 5)
print(DrugsPrevalenceFemaleSubtype1, newpage = FALSE)
dev.off()

#5. Plots for prevalence of drug use in subtype 1, stratified by both age and sex
DrugsPrevalenceStratifiedByAgeAndSexSubtype1<- PrevalenceTableSubtype1 %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "18 +")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "18-30")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "31-40")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "41-50")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "51-60")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "61-70")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "71-80")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "81 +")) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  labs(title = str_wrap("Prevalence of antiparkinson drugs in people with subtype 1, stratified by age and sex", 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceStratifiedByAgeAndSexSubtype1Name <- paste0("DrugsPrevalenceSubtype1StratifiedByAgeAndSex", ".pdf")

pdf(here("2_AnalysisIncidencePrevalence", "Plots", DrugsPrevalenceStratifiedByAgeAndSexSubtype1Name),
    width = 7, height = 5)
print(DrugsPrevalenceStratifiedByAgeAndSexSubtype1, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                                Incidence                                   #
#                                                                            #
##############################################################################

incSubtype1 <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominatorSubtype1",
  outcomeTable = "...", #The name of your choice for the table associated to antiparkinson drugs
  outcomeCohortId = outcome_cohorts_drugs$cohortId,
  outcomeCohortName = outcome_cohorts_drugs$cohortName,
  interval = "years",
  completeDatabaseIntervals = F,
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = T
)

IncidenceTableSubtype1 <- incSubtype1 %>% left_join(settings(incSubtype1))

# 1.Plots for incidence of the drug use in subtype 1
DrugsIncidenceOverallSubtype1<- IncidenceTableSubtype1 %>%
  filter(denominator_cohort_id == 3) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  ggtitle("Incidence of antiparkinson drugs in people with subtype 1") +
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceOverallSubtype1Name <- paste0("DrugsIncidenceSubtype1", ".pdf")

pdf(here("2_AnalysisIncidencePrevalence", "Plots", DrugsIncidenceOverallSubtype1Name),
    width = 7, height = 5)
print(DrugsIncidenceOverallSubtype1, newpage = FALSE)
dev.off()

# 2.Plots for incidence of the drug use in subtype 1, stratified by sex
DrugsIncidenceBothSubtype1<- IncidenceTableSubtype1 %>%
  filter(denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "18 + years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "18-30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "31-40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "41-50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "51-60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "61-70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "71-80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "81 + years old")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  labs(title = str_wrap("Incidence of antiparkinson drugs in people with subtype 1, stratified by age", 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceBothSubtype1Name <- paste0("DrugsIncidenceSubtype1Both", ".pdf")

pdf(here("2_AnalysisIncidencePrevalence", "Plots", DrugsIncidenceBothSubtype1Name),
    width = 7, height = 5)
print(DrugsIncidenceBothSubtype1, newpage = FALSE)
dev.off()

# 3.Plots for incidence of the drug use in men with subtype 1, stratified by age
DrugsIncidenceMaleSubtype1<- IncidenceTableSubtype1 %>%
  filter(denominator_sex == "Male") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "18 + years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "18-30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "31-40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "41-50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "51-60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "61-70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "71-80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "81 + years old")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  labs(title = str_wrap("Incidence of antiparkinson drugs in men with subtype 1, stratified by age", 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("PIncidence (per 100,000 person-years)")

DrugsIncidenceMaleSubtype1Name <- paste0("DrugsIncidenceSubtype1Male", ".pdf")

pdf(here("2_AnalysisIncidencePrevalence", "Plots", DrugsIncidenceMaleSubtype1Name),
    width = 7, height = 5)
print(DrugsIncidenceMaleSubtype1, newpage = FALSE)
dev.off()

# 4.Plots for incidence of the drug use in women with subtype 1, stratified by age
DrugsIncidenceFemaleSubtype1<- IncidenceTableSubtype1 %>%
  filter(denominator_sex == "Female") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "18 + years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "18-30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "31-40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "41-50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "51-60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "61-70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "71-80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "81 + years old")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  labs(title = str_wrap("Incidence of antiparkinson drugs in women with subtype 1, stratified by age", 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceFemaleSubtype1Name <- paste0("DrugsIncidenceSubtype1Female", ".pdf")

pdf(here("2_AnalysisIncidencePrevalence", "Plots", DrugsIncidenceFemaleSubtype1Name),
    width = 7, height = 5)
print(DrugsIncidenceFemaleSubtype1, newpage = FALSE)
dev.off()

#5. Plots for incidence of drug use in subtype 1, stratified by both age and sex
DrugsIncidenceStratifiedByAgeAndSexSubtype1<- IncidenceTableSubtype1 %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;150", "18 +")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18;30", "18-30")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31;40", "31-40")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41;50", "41-50")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51;60", "51-60")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61;70", "61-70")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71;80", "71-80")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81;150", "81 +")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  labs(title = str_wrap("Incidence of antiparkinson drugs in people with subtype 1, stratified by age and sex", 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceStratifiedByAgeAndSexSubtype1Name <- paste0("DrugsIncidenceSubtype1StratifiedByAgeAndSex", ".pdf")

pdf(here("2_AnalysisIncidencePrevalence", "Plots", DrugsIncidenceStratifiedByAgeAndSexSubtype1Name),
    width = 7, height = 5)
print(DrugsIncidenceStratifiedByAgeAndSexSubtype1, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                            Gather & export                                 #
#                                                                            #
##############################################################################
study_results_Subtype1 <- gatherIncidencePrevalenceResults(
  cdm = cdm,
  resultList=list(incSubtype1, prevSubtype1),
  databaseName = "...") #Database name

exportIncidencePrevalenceResults(result=study_results_Subtype1,
                                 zipName= paste0("...", "IncidencePrevalenceResultsDrugsInSubtype1"), #Database name
                                 outputFolder=here::here("2_AnalysisIncidencePrevalence"))
