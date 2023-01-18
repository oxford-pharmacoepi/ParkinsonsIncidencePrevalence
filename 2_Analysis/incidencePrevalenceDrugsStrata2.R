##############################################################################
#                                                                            #
#                              Making Denominators                           #
#                                                                            #
##############################################################################
cdm$denominatorSubtype2 <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2007-01-01"),
  ageGroup = list(c(18,150),c(18, 30),c(31,40),c(41,50),c(51,60),c(61,70),c(71,80),c(81,150)),
  sex = c("Female", "Male", "Both"),
  daysPriorHistory = 365,
  strataTable = subtype_table_name, 
  strataCohortId = 2,
  strataCohortName = subtype2name, 
  verbose = T
)

##############################################################################
#                                                                            #
#                                Prevalence                                  #
#                                                                            #
##############################################################################
prevSubtype2 <- estimatePeriodPrevalence(cdm = cdm,
                                    denominatorTable = "denominatorSubtype2",
                                    outcomeTable = drug_table_name,
                                    outcomeCohortId = outcome_cohorts_drugs$cohortId,
                                    outcomeCohortName = outcome_cohorts_drugs$cohortName,
                                    interval = "Years",
                                    minCellCount = 5,
                                    completeDatabaseIntervals = F,
                                    verbose = T)
PrevalenceTableSubtype2 <- prevSubtype2 %>%
  left_join(settings(prevSubtype2))

# 1.Plots for prevalence of the drug use in subtype 2
DrugsPrevalenceOverallSubtype2<- PrevalenceTableSubtype2 %>%
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
  ggtitle(paste0("Prevalence of antiparkinson drugs in people with ", subtype2name)) +
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceOverallSubtype2Name <- paste0("DrugsPrevalence", subtype2name, ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceOverallSubtype2Name),
    width = 7, height = 5)
print(DrugsPrevalenceOverallSubtype2, newpage = FALSE)
dev.off()

# 2.Plots for prevalence of the drug use in subtype 2, stratified by sex
DrugsPrevalenceBothSubtype2<- PrevalenceTableSubtype2 %>%
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
  labs(title = str_wrap(paste0("Prevalence of antiparkinson drugs in people with ", subtype2name, ", stratified by age"), 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceBothSubtype2Name <- paste0("DrugsPrevalence", subtype2name,"Both", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceBothSubtype2Name),
    width = 7, height = 5)
print(DrugsPrevalenceBothSubtype2, newpage = FALSE)
dev.off()

# 3.Plots for prevalence of the drug use in men with subtype 2, stratified by age
DrugsPrevalenceMaleSubtype2<- PrevalenceTableSubtype2 %>%
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
  labs(title = str_wrap(paste0("Prevalence of antiparkinson drugs in men with ", subtype2name, ", stratified by age"), 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceMaleSubtype2Name <- paste0("DrugsPrevalence", subtype2name ,"Male", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceMaleSubtype2Name),
    width = 7, height = 5)
print(DrugsPrevalenceMaleSubtype2, newpage = FALSE)
dev.off()

# 4.Plots for prevalence of the drug use in women with subtype 2, stratified by age
DrugsPrevalenceFemaleSubtype2<- PrevalenceTableSubtype2 %>%
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
  labs(title = str_wrap(paste0("Prevalence of antiparkinson drugs in women with ",subtype2name,", stratified by age"), 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceFemaleSubtype2Name <- paste0("DrugsPrevalence",subtype2name,"Female", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceFemaleSubtype2Name),
    width = 7, height = 5)
print(DrugsPrevalenceFemaleSubtype2, newpage = FALSE)
dev.off()

#5. Plots for prevalence of drug use in subtype 2, stratified by both age and sex
DrugsPrevalenceStratifiedByAgeAndSexSubtype2<- PrevalenceTableSubtype2 %>%
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
  labs(title = str_wrap(paste0("Prevalence of antiparkinson drugs in people with ",subtype2name,", stratified by age and sex"), 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceStratifiedByAgeAndSexSubtype2Name <- paste0("DrugsPrevalence",subtype2name,"StratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceStratifiedByAgeAndSexSubtype2Name),
    width = 7, height = 5)
print(DrugsPrevalenceStratifiedByAgeAndSexSubtype2, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                                Incidence                                   #
#                                                                            #
##############################################################################

incSubtype2 <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominatorSubtype2",
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

IncidenceTableSubtype2 <- incSubtype2 %>% left_join(settings(incSubtype2))

# 1.Plots for incidence of the drug use in subtype 2
DrugsIncidenceOverallSubtype2<- IncidenceTableSubtype2 %>%
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
  ggtitle(paste0("Incidence of antiparkinson drugs in people with ", subtype2name)) +
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceOverallSubtype2Name <- paste0("DrugsIncidence", subtype2name,".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceOverallSubtype2Name),
    width = 7, height = 5)
print(DrugsIncidenceOverallSubtype2, newpage = FALSE)
dev.off()

# 2.Plots for incidence of the drug use in subtype 2, stratified by sex
DrugsIncidenceBothSubtype2<- IncidenceTableSubtype2 %>%
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
  labs(title = str_wrap(paste0("Incidence of antiparkinson drugs in people with ",subtype2name,", stratified by age"), 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceBothSubtype2Name <- paste0("DrugsIncidence",subtype2name,"Both", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceBothSubtype2Name),
    width = 7, height = 5)
print(DrugsIncidenceBothSubtype2, newpage = FALSE)
dev.off()

# 3.Plots for incidence of the drug use in men with subtype 2, stratified by age
DrugsIncidenceMaleSubtype2<- IncidenceTableSubtype2 %>%
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
  labs(title = str_wrap(paste0("Incidence of antiparkinson drugs in men with ",subtype2name,", stratified by age"), 70))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("PIncidence (per 100,000 person-years)")

DrugsIncidenceMaleSubtype2Name <- paste0("DrugsIncidence",subtype2name,"Male", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceMaleSubtype2Name),
    width = 7, height = 5)
print(DrugsIncidenceMaleSubtype2, newpage = FALSE)
dev.off()

# 4.Plots for incidence of the drug use in women with subtype 2, stratified by age
DrugsIncidenceFemaleSubtype2<- IncidenceTableSubtype2 %>%
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
  labs(title = str_wrap(paste0("Incidence of antiparkinson drugs in women with ",subtype2name,", stratified by age"), 70))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceFemaleSubtype2Name <- paste0("DrugsIncidence",subtype2name,"Female", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceFemaleSubtype2Name),
    width = 7, height = 5)
print(DrugsIncidenceFemaleSubtype2, newpage = FALSE)
dev.off()

#5. Plots for incidence of drug use in subtype 2, stratified by both age and sex
DrugsIncidenceStratifiedByAgeAndSexSubtype2<- IncidenceTableSubtype2 %>%
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
  labs(title = str_wrap(paste0("Incidence of antiparkinson drugs in people with ",subtype2name,", stratified by age and sex"), 80))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceStratifiedByAgeAndSexSubtype2Name <- paste0("DrugsIncidence",subtype2name,"StratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceStratifiedByAgeAndSexSubtype2Name),
    width = 7, height = 5)
print(DrugsIncidenceStratifiedByAgeAndSexSubtype2, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                            Gather & export                                 #
#                                                                            #
##############################################################################
study_results_Subtype2 <- gatherIncidencePrevalenceResults(
  cdm = cdm,
  resultList=list(incSubtype2, prevSubtype2),
  databaseName = db.name) 

exportIncidencePrevalenceResults(result=study_results_Subtype2,
                                 zipName= paste0(db.name, "IncidencePrevalenceResultsDrugsIn", subtype2name),
                                 outputFolder=here::here("Results", db.name))
