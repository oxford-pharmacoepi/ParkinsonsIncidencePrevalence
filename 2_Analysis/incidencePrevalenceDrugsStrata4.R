##############################################################################
#                                                                            #
#                              Making Denominators                           #
#                                                                            #
##############################################################################
cdm$denominatorSubtype4 <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2007-01-01"),
  ageGroup = list(c(18,150),c(18, 30),c(31,40),c(41,50),c(51,60),c(61,70),c(71,80),c(81,150)),
  sex = c("Female", "Male", "Both"),
  daysPriorHistory = 365,
  strataTable = subtype_table_name, 
  strataCohortId = 4,
  strataCohortName = subtype4name, 
  verbose = T
)

##############################################################################
#                                                                            #
#                                Prevalence                                  #
#                                                                            #
##############################################################################
prevSubtype4 <- estimatePeriodPrevalence(cdm = cdm,
                                    denominatorTable = "denominatorSubtype4",
                                    outcomeTable = drug_table_name,
                                    outcomeCohortId = outcome_cohorts_drugs$cohortId,
                                    outcomeCohortName = outcome_cohorts_drugs$cohortName,
                                    interval = "Years",
                                    minCellCount = 5,
                                    completeDatabaseIntervals = F,
                                    verbose = T)
PrevalenceTableSubtype4 <- prevSubtype4 %>%
  left_join(settings(prevSubtype4))

# 1.Plots for prevalence of the drug use in subtype 4
DrugsPrevalenceOverallSubtype4<- PrevalenceTableSubtype4 %>%
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
  ggtitle(paste0("Prevalence of antiparkinson drugs in people with ", subtype4name)) +
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceOverallSubtype4Name <- paste0("DrugsPrevalence", subtype4name, ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceOverallSubtype4Name),
    width = 7, height = 5)
print(DrugsPrevalenceOverallSubtype4, newpage = FALSE)
dev.off()

# 2.Plots for prevalence of the drug use in subtype 4, stratified by sex
DrugsPrevalenceBothSubtype4<- PrevalenceTableSubtype4 %>%
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
  labs(title = str_wrap(paste0("Prevalence of antiparkinson drugs in people with ", subtype4name, ", stratified by age"), 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceBothSubtype4Name <- paste0("DrugsPrevalence", subtype4name,"Both", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceBothSubtype4Name),
    width = 7, height = 5)
print(DrugsPrevalenceBothSubtype4, newpage = FALSE)
dev.off()

# 3.Plots for prevalence of the drug use in men with subtype 4, stratified by age
DrugsPrevalenceMaleSubtype4<- PrevalenceTableSubtype4 %>%
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
  labs(title = str_wrap(paste0("Prevalence of antiparkinson drugs in men with ", subtype4name, ", stratified by age"), 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceMaleSubtype4Name <- paste0("DrugsPrevalence", subtype4name ,"Male", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceMaleSubtype4Name),
    width = 7, height = 5)
print(DrugsPrevalenceMaleSubtype4, newpage = FALSE)
dev.off()

# 4.Plots for prevalence of the drug use in women with subtype 4, stratified by age
DrugsPrevalenceFemaleSubtype4<- PrevalenceTableSubtype4 %>%
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
  labs(title = str_wrap(paste0("Prevalence of antiparkinson drugs in women with ",subtype4name,", stratified by age"), 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceFemaleSubtype4Name <- paste0("DrugsPrevalence",subtype4name,"Female", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceFemaleSubtype4Name),
    width = 7, height = 5)
print(DrugsPrevalenceFemaleSubtype4, newpage = FALSE)
dev.off()

#5. Plots for prevalence of drug use in subtype 4, stratified by both age and sex
DrugsPrevalenceStratifiedByAgeAndSexSubtype4<- PrevalenceTableSubtype4 %>%
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
  labs(title = str_wrap(paste0("Prevalence of antiparkinson drugs in people with ",subtype4name,", stratified by age and sex"), 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceStratifiedByAgeAndSexSubtype4Name <- paste0("DrugsPrevalence",subtype4name,"StratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceStratifiedByAgeAndSexSubtype4Name),
    width = 7, height = 5)
print(DrugsPrevalenceStratifiedByAgeAndSexSubtype4, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                                Incidence                                   #
#                                                                            #
##############################################################################

incSubtype4 <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominatorSubtype4",
  outcomeTable = drug_table_name,
  outcomeCohortId = outcome_cohorts_drugs$cohortId,
  outcomeCohortName = outcome_cohorts_drugs$cohortName,
  interval = "years",
  completeDatabaseIntervals = F,
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = T
)

IncidenceTableSubtype4 <- incSubtype4 %>% left_join(settings(incSubtype4))

# 1.Plots for incidence of the drug use in subtype 4
DrugsIncidenceOverallSubtype4<- IncidenceTableSubtype4 %>%
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
  ggtitle(paste0("Incidence of antiparkinson drugs in people with ", subtype4name)) +
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceOverallSubtype4Name <- paste0("DrugsIncidence", subtype4name,".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceOverallSubtype4Name),
    width = 7, height = 5)
print(DrugsIncidenceOverallSubtype4, newpage = FALSE)
dev.off()

# 2.Plots for incidence of the drug use in subtype 4, stratified by sex
DrugsIncidenceBothSubtype4<- IncidenceTableSubtype4 %>%
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
  labs(title = str_wrap(paste0("Incidence of antiparkinson drugs in people with ",subtype4name,", stratified by age"), 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceBothSubtype4Name <- paste0("DrugsIncidence",subtype4name,"Both", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceBothSubtype4Name),
    width = 7, height = 5)
print(DrugsIncidenceBothSubtype4, newpage = FALSE)
dev.off()

# 3.Plots for incidence of the drug use in men with subtype 4, stratified by age
DrugsIncidenceMaleSubtype4<- IncidenceTableSubtype4 %>%
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
  labs(title = str_wrap(paste0("Incidence of antiparkinson drugs in men with ",subtype4name,", stratified by age"), 70))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("PIncidence (per 100,000 person-years)")

DrugsIncidenceMaleSubtype4Name <- paste0("DrugsIncidence",subtype4name,"Male", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceMaleSubtype4Name),
    width = 7, height = 5)
print(DrugsIncidenceMaleSubtype4, newpage = FALSE)
dev.off()

# 4.Plots for incidence of the drug use in women with subtype 4, stratified by age
DrugsIncidenceFemaleSubtype4<- IncidenceTableSubtype4 %>%
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
  labs(title = str_wrap(paste0("Incidence of antiparkinson drugs in women with ",subtype4name,", stratified by age"), 70))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceFemaleSubtype4Name <- paste0("DrugsIncidence",subtype4name,"Female", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceFemaleSubtype4Name),
    width = 7, height = 5)
print(DrugsIncidenceFemaleSubtype4, newpage = FALSE)
dev.off()

#5. Plots for incidence of drug use in subtype 4, stratified by both age and sex
DrugsIncidenceStratifiedByAgeAndSexSubtype4<- IncidenceTableSubtype4 %>%
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
  labs(title = str_wrap(paste0("Incidence of antiparkinson drugs in people with ",subtype4name,", stratified by age and sex"), 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceStratifiedByAgeAndSexSubtype4Name <- paste0("DrugsIncidence",subtype4name,"StratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceStratifiedByAgeAndSexSubtype4Name),
    width = 7, height = 5)
print(DrugsIncidenceStratifiedByAgeAndSexSubtype4, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                            Gather & export                                 #
#                                                                            #
##############################################################################
study_results_Subtype4 <- gatherIncidencePrevalenceResults(
  cdm = cdm,
  resultList=list(incSubtype4, prevSubtype4),
  databaseName = db.name) 

exportIncidencePrevalenceResults(result=study_results_Subtype4,
                                 zipName= paste0(db.name, "IncidencePrevalenceResultsDrugsIn", subtype4name),
                                 outputFolder=here::here("Results", db.name))
