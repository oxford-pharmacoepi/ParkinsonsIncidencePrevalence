##############################################################################
#                                                                            #
#                              Making Denominators                           #
#                                                                            #
##############################################################################
info(logger, paste0('Making', subtypesCohortSet %>% filter(cohort_definition_id=4) %>% pull(cohort_name), "into denominators" ))
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name),
  cohortDateRange = as.Date(c("2007-01-01", NA)),
  ageGroup = list(c(18,150),c(18, 30),c(31,40),c(41,50),c(51,60),c(61,70),c(71,80),c(81,150)),
  sex = c("Female", "Male", "Both"),
  daysPriorHistory = 365,
  strataTable = subtype_table_name, 
  strataCohortId = 4
)

##############################################################################
#                                                                            #
#                                Prevalence                                  #
#                                                                            #
##############################################################################
prevSubtype4 <- estimatePeriodPrevalence(cdm = cdm,
                                         denominatorTable = subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name),
                                         outcomeTable = drug_table_name,
                                         outcomeLookbackDays = 30)

# 1.Plots for prevalence of the drug use in subtype 4
DrugsPrevalenceOverallSubtype4<- prevSubtype4 %>%
  filter(denominator_age_group == '18 to 150', denominator_sex == "Both") %>%
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
  ggtitle(paste0("Prevalence of antiparkinson drugs in people with ", subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name))) +
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceOverallSubtype4Name <- paste0("DrugsPrevalence", subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name_camel), ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceOverallSubtype4Name),
    width = 10, height = 8)
print(DrugsPrevalenceOverallSubtype4, newpage = FALSE)
dev.off()

# 2.Plots for prevalence of the drug use in subtype 4, stratified by age
DrugsPrevalenceBothSubtype4<- prevSubtype4 %>%
  filter(denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31 to 40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41 to 50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51 to 60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61 to 70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71 to 80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  labs(title = paste0("Prevalence of antiparkinson drugs in people with ", subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name), ", stratified by age"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceBothSubtype4Name <- paste0("DrugsPrevalence", subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name_camel),"Both", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceBothSubtype4Name),
    width = 10, height = 8)
print(DrugsPrevalenceBothSubtype4, newpage = FALSE)
dev.off()

# 3.Plots for prevalence of the drug use in men with subtype 4, stratified by age
DrugsPrevalenceMaleSubtype4<- prevSubtype4 %>%
  filter(denominator_sex == "Male") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31 to 40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41 to 50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51 to 60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61 to 70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71 to 80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  labs(title = paste0("Prevalence of antiparkinson drugs in men with ", subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name), ", stratified by age"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceMaleSubtype4Name <- paste0("DrugsPrevalence", subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name_camel) ,"Male", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceMaleSubtype4Name),
    width = 10, height = 8)
print(DrugsPrevalenceMaleSubtype4, newpage = FALSE)
dev.off()

# 4.Plots for prevalence of the drug use in women with subtype 4, stratified by age
DrugsPrevalenceFemaleSubtype4<- prevSubtype4 %>%
  filter(denominator_sex == "Female") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31 to 40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41 to 50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51 to 60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61 to 70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71 to 80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  labs(title = paste0("Prevalence of antiparkinson drugs in women with ",subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name),", stratified by age"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceFemaleSubtype4Name <- paste0("DrugsPrevalence",subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name_camel),"Female", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceFemaleSubtype4Name),
    width = 10, height = 8)
print(DrugsPrevalenceFemaleSubtype4, newpage = FALSE)
dev.off()

#5. Plots for prevalence of drug use in subtype 4, stratified by both age and sex
DrugsPrevalenceStratifiedByAgeAndSexSubtype4<- prevSubtype4 %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31 to 40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41 to 50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51 to 60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61 to 70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71 to 80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = prevalence_start_date, y=prevalence, ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_sex ~denominator_age_group) +
  labs(title = paste0("Prevalence of antiparkinson drugs in people with ",subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name),", stratified by age and sex"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceStratifiedByAgeAndSexSubtype4Name <- paste0("DrugsPrevalence",subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name_camel),"StratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceStratifiedByAgeAndSexSubtype4Name),
    width = 20, height = 8)
print(DrugsPrevalenceStratifiedByAgeAndSexSubtype4, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                                Incidence                                   #
#                                                                            #
##############################################################################

incSubtype4 <- estimateIncidence(
  cdm = cdm,
  denominatorTable = subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name),
  outcomeTable = drug_table_name,
  interval = "years",
  completeDatabaseIntervals = F,
  outcomeWashout = 30,
  repeatedEvents = TRUE
)

# 1.Plots for incidence of the drug use in subtype 4
DrugsIncidenceOverallSubtype4<- incSubtype4 %>%
  filter(denominator_age_group == '18 to 150', denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  ggtitle(paste0("Incidence of antiparkinson drugs in people with ", subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name))) +
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceOverallSubtype4Name <- paste0("DrugsIncidence", subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name_camel),".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceOverallSubtype4Name),
    width = 10, height = 8)
print(DrugsIncidenceOverallSubtype4, newpage = FALSE)
dev.off()

# 2.Plots for incidence of the drug use in subtype 4, stratified by sex
DrugsIncidenceBothSubtype4<- incSubtype4 %>%
  filter(denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31 to 40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41 to 50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51 to 60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61 to 70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71 to 80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  labs(title = paste0("Incidence of antiparkinson drugs in people with ",subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name),", stratified by age"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceBothSubtype4Name <- paste0("DrugsIncidence",subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name_camel),"Both", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceBothSubtype4Name),
    width = 10, height = 8)
print(DrugsIncidenceBothSubtype4, newpage = FALSE)
dev.off()

# 3.Plots for incidence of the drug use in men with subtype 4, stratified by age
DrugsIncidenceMaleSubtype4<- incSubtype4 %>%
  filter(denominator_sex == "Male") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31 to 40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41 to 50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51 to 60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61 to 70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71 to 80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  labs(title = paste0("Incidence of antiparkinson drugs in men with ",subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name),", stratified by age"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceMaleSubtype4Name <- paste0("DrugsIncidence",subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name_camel),"Male", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceMaleSubtype4Name),
    width = 10, height = 8)
print(DrugsIncidenceMaleSubtype4, newpage = FALSE)
dev.off()

# 4.Plots for incidence of the drug use in women with subtype 4, stratified by age
DrugsIncidenceFemaleSubtype4<- incSubtype4 %>%
  filter(denominator_sex == "Female") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31 to 40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41 to 50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51 to 60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61 to 70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71 to 80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_wrap(~denominator_age_group) +
  labs(title = paste0("Incidence of antiparkinson drugs in women with ",subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name),", stratified by age"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceFemaleSubtype4Name <- paste0("DrugsIncidence",subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name_camel),"Female", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceFemaleSubtype4Name),
    width = 10, height = 8)
print(DrugsIncidenceFemaleSubtype4, newpage = FALSE)
dev.off()

#5. Plots for incidence of drug use in subtype 4, stratified by both age and sex
DrugsIncidenceStratifiedByAgeAndSexSubtype4<- incSubtype4 %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "Over 18 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 30", "Between 18 and 30 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "31 to 40", "Between 30 and 40 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "41 to 50", "Between 40 and 50 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "51 to 60", "Between 50 and 60 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "61 to 70", "Between 60 and 70 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "71 to 80", "Between 70 and 80 years old")) %>%
  mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80 years old")) %>%
  mutate(across(denominator_age_group, factor, levels=c("Over 18 years old","Between 18 and 30 years old","Between 30 and 40 years old", "Between 40 and 50 years old", "Between 50 and 60 years old", "Between 60 and 70 years old", "Between 70 and 80 years old", "Over 80 years old"))) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_sex ~denominator_age_group) +
  labs(title = paste0("Incidence of antiparkinson drugs in people with ",subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name),", stratified by age and sex"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceStratifiedByAgeAndSexSubtype4Name <- paste0("DrugsIncidence",subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name_camel),"StratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceStratifiedByAgeAndSexSubtype4Name),
    width = 20, height = 8)
print(DrugsIncidenceStratifiedByAgeAndSexSubtype4, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                            Gather & export                                 #
#                                                                            #
##############################################################################
info(logger, paste0('Gathering results for antiparkinson drugs in ', subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name)))
exportIncidencePrevalenceResults(resultList = list(paste0("Prevalence of antiparkinson drugs in ", subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name)) = prevSubtype4,paste0("Incidence of antiparkinson drugs in ", subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name)) = incSubtype4),
                                 zipName= paste0(db.name, "_IncidencePrevalenceResultsDrugsIn", subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name_camel)),
                                 outputFolder=here::here("Results", db.name))
