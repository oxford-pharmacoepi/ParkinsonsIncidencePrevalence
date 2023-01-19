##############################################################################
#                                                                            #
#                              Making Denominators                           #
#                                                                            #
##############################################################################
cdm$denominatorSubtype3 <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2007-01-01"),
  ageGroup = list(c(18,150),c(18, 30),c(31,40),c(41,50),c(51,60),c(61,70),c(71,80),c(81,150)),
  sex = c("Female", "Male", "Both"),
  daysPriorHistory = 365,
  strataTable = subtype_table_name, 
  strataCohortId = 3,
  strataCohortName = subtype3name, 
  verbose = T
)

##############################################################################
#                                                                            #
#                                Prevalence                                  #
#                                                                            #
##############################################################################
prevSubtype3 <- estimatePeriodPrevalence(cdm = cdm,
                                    denominatorTable = "denominatorSubtype3",
                                    outcomeTable = drug_table_name,
                                    outcomeCohortId = outcome_cohorts_drugs$cohortId,
                                    outcomeCohortName = outcome_cohorts_drugs$cohortName,
                                    interval = "Years",
                                    minCellCount = 5,
                                    completeDatabaseIntervals = F,
                                    verbose = T)
PrevalenceTableSubtype3 <- prevSubtype3 %>%
  left_join(settings(prevSubtype3))

# 1.Plots for prevalence of the drug use in subtype 3
DrugsPrevalenceOverallSubtype3<- PrevalenceTableSubtype3 %>%
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
  ggtitle(paste0("Prevalence of antiparkinson drugs in people with ", subtype3name)) +
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceOverallSubtype3Name <- paste0("DrugsPrevalence", subtype3name, ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceOverallSubtype3Name),
    width = 7, height = 5)
print(DrugsPrevalenceOverallSubtype3, newpage = FALSE)
dev.off()

# 2.Plots for prevalence of the drug use in subtype 3, stratified by sex
DrugsPrevalenceBothSubtype3<- PrevalenceTableSubtype3 %>%
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
  labs(title = str_wrap(paste0("Prevalence of antiparkinson drugs in people with ", subtype3name, ", stratified by age"), 70))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceBothSubtype3Name <- paste0("DrugsPrevalence", subtype3name,"Both", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceBothSubtype3Name),
    width = 7, height = 5)
print(DrugsPrevalenceBothSubtype3, newpage = FALSE)
dev.off()

# 3.Plots for prevalence of the drug use in men with subtype 3, stratified by age
DrugsPrevalenceMaleSubtype3<- PrevalenceTableSubtype3 %>%
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
  labs(title = str_wrap(paste0("Prevalence of antiparkinson drugs in men with ", subtype3name, ", stratified by age"), 70))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceMaleSubtype3Name <- paste0("DrugsPrevalence", subtype3name ,"Male", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceMaleSubtype3Name),
    width = 7, height = 5)
print(DrugsPrevalenceMaleSubtype3, newpage = FALSE)
dev.off()

# 4.Plots for prevalence of the drug use in women with subtype 3, stratified by age
DrugsPrevalenceFemaleSubtype3<- PrevalenceTableSubtype3 %>%
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
  labs(title = str_wrap(paste0("Prevalence of antiparkinson drugs in women with ",subtype3name,", stratified by age"), 70))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceFemaleSubtype3Name <- paste0("DrugsPrevalence",subtype3name,"Female", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceFemaleSubtype3Name),
    width = 7, height = 5)
print(DrugsPrevalenceFemaleSubtype3, newpage = FALSE)
dev.off()

#5. Plots for prevalence of drug use in subtype 3, stratified by both age and sex
DrugsPrevalenceStratifiedByAgeAndSexSubtype3<- PrevalenceTableSubtype3 %>%
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
  labs(title = str_wrap(paste0("Prevalence of antiparkinson drugs in people with ",subtype3name,", stratified by age and sex"), 70))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceStratifiedByAgeAndSexSubtype3Name <- paste0("DrugsPrevalence",subtype3name,"StratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceStratifiedByAgeAndSexSubtype3Name),
    width = 7, height = 5)
print(DrugsPrevalenceStratifiedByAgeAndSexSubtype3, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                                Incidence                                   #
#                                                                            #
##############################################################################

incSubtype3 <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominatorSubtype3",
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

IncidenceTableSubtype3 <- incSubtype3 %>% left_join(settings(incSubtype3))

# 1.Plots for incidence of the drug use in subtype 3
DrugsIncidenceOverallSubtype3<- IncidenceTableSubtype3 %>%
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
  ggtitle(paste0("Incidence of antiparkinson drugs in people with ", subtype3name)) +
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceOverallSubtype3Name <- paste0("DrugsIncidence", subtype3name,".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceOverallSubtype3Name),
    width = 7, height = 5)
print(DrugsIncidenceOverallSubtype3, newpage = FALSE)
dev.off()

# 2.Plots for incidence of the drug use in subtype 3, stratified by sex
DrugsIncidenceBothSubtype3<- IncidenceTableSubtype3 %>%
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
  labs(title = str_wrap(paste0("Incidence of antiparkinson drugs in people with ",subtype3name,", stratified by age"), 70))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceBothSubtype3Name <- paste0("DrugsIncidence",subtype3name,"Both", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceBothSubtype3Name),
    width = 7, height = 5)
print(DrugsIncidenceBothSubtype3, newpage = FALSE)
dev.off()

# 3.Plots for incidence of the drug use in men with subtype 3, stratified by age
DrugsIncidenceMaleSubtype3<- IncidenceTableSubtype3 %>%
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
  labs(title = str_wrap(paste0("Incidence of antiparkinson drugs in men with ",subtype3name,", stratified by age"), 70))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("PIncidence (per 100,000 person-years)")

DrugsIncidenceMaleSubtype3Name <- paste0("DrugsIncidence",subtype3name,"Male", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceMaleSubtype3Name),
    width = 7, height = 5)
print(DrugsIncidenceMaleSubtype3, newpage = FALSE)
dev.off()

# 4.Plots for incidence of the drug use in women with subtype 3, stratified by age
DrugsIncidenceFemaleSubtype3<- IncidenceTableSubtype3 %>%
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
  labs(title = str_wrap(paste0("Incidence of antiparkinson drugs in women with ",subtype3name,", stratified by age"), 70))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceFemaleSubtype3Name <- paste0("DrugsIncidence",subtype3name,"Female", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceFemaleSubtype3Name),
    width = 7, height = 5)
print(DrugsIncidenceFemaleSubtype3, newpage = FALSE)
dev.off()

#5. Plots for incidence of drug use in subtype 3, stratified by both age and sex
DrugsIncidenceStratifiedByAgeAndSexSubtype3<- IncidenceTableSubtype3 %>%
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
  labs(title = str_wrap(paste0("Incidence of antiparkinson drugs in people with ",subtype3name,", stratified by age and sex"), 70))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceStratifiedByAgeAndSexSubtype3Name <- paste0("DrugsIncidence",subtype3name,"StratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceStratifiedByAgeAndSexSubtype3Name),
    width = 7, height = 5)
print(DrugsIncidenceStratifiedByAgeAndSexSubtype3, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                            Gather & export                                 #
#                                                                            #
##############################################################################
study_results_Subtype3 <- gatherIncidencePrevalenceResults(
  cdm = cdm,
  resultList=list(incSubtype3, prevSubtype3),
  databaseName = db.name) 

exportIncidencePrevalenceResults(result=study_results_Subtype3,
                                 zipName= paste0(db.name, "IncidencePrevalenceResultsDrugsIn", subtype3name),
                                 outputFolder=here::here("Results", db.name))
