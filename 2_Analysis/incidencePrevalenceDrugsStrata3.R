##############################################################################
#                                                                            #
#                              Making Denominators                           #
#                                                                            #
##############################################################################
info(logger, paste0('Making ', subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name), "into denominators" ))
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name),
  cohortDateRange = as.Date(c("2007-01-01", NA)),
  ageGroup = list(c(18,150),c(18, 30),c(31,40),c(41,50),c(51,60),c(61,70),c(71,80),c(81,150)),
  sex = c("Female", "Male", "Both"),
  daysPriorHistory = 365,
  strataTable = subtype_table_name, 
  strataCohortId = 3
)

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = paste0(subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name),"1y"),
  cohortDateRange = as.Date(c("2007-01-01", NA)),
  ageGroup = list(c(18,150),c(18, 30),c(31,40),c(41,50),c(51,60),c(61,70),c(71,80),c(81,150)),
  sex = c("Female", "Male", "Both"),
  daysPriorHistory = 365,
  strataTable = subtype_table_name_1y, 
  strataCohortId = subtypesCohortSet_1y %>% filter(cohort_name == subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name)) %>% pull(cohort_definition_id)
)

##############################################################################
#                                                                            #
#                                Prevalence                                  #
#                                                                            #
##############################################################################
prevSubtype3 <- estimatePeriodPrevalence(cdm = cdm,
                                         denominatorTable = subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name),
                                         outcomeTable = drug_table_name)

# 1.Plots for prevalence of the drug use in subtype 3
DrugsPrevalenceOverallSubtype3<- prevSubtype3 %>%
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
  ggtitle(paste0("Prevalence of antiparkinson drugs in people with ", subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name))) +
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceOverallSubtype3Name <- paste0("DrugsPrevalence", subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name_camel), ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceOverallSubtype3Name),
    width = 10, height = 8)
print(DrugsPrevalenceOverallSubtype3, newpage = FALSE)
dev.off()

# 2.Plots for prevalence of the drug use in subtype 3, stratified by age
DrugsPrevalenceBothSubtype3<- prevSubtype3 %>%
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
  labs(title = paste0("Prevalence of antiparkinson drugs in people with ", subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name), ", stratified by age"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceBothSubtype3Name <- paste0("DrugsPrevalence", subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name_camel),"Both", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceBothSubtype3Name),
    width = 10, height = 8)
print(DrugsPrevalenceBothSubtype3, newpage = FALSE)
dev.off()

# 3.Plots for prevalence of the drug use in men with subtype 3, stratified by age
DrugsPrevalenceMaleSubtype3<- prevSubtype3 %>%
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
  labs(title = paste0("Prevalence of antiparkinson drugs in men with ", subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name), ", stratified by age"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceMaleSubtype3Name <- paste0("DrugsPrevalence", subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name_camel) ,"Male", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceMaleSubtype3Name),
    width = 10, height = 8)
print(DrugsPrevalenceMaleSubtype3, newpage = FALSE)
dev.off()

# 4.Plots for prevalence of the drug use in women with subtype 3, stratified by age
DrugsPrevalenceFemaleSubtype3<- prevSubtype3 %>%
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
  labs(title = paste0("Prevalence of antiparkinson drugs in women with ",subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name),", stratified by age"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceFemaleSubtype3Name <- paste0("DrugsPrevalence",subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name_camel),"Female", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceFemaleSubtype3Name),
    width = 10, height = 8)
print(DrugsPrevalenceFemaleSubtype3, newpage = FALSE)
dev.off()

#5. Plots for prevalence of drug use in subtype 3, stratified by both age and sex
DrugsPrevalenceStratifiedByAgeAndSexSubtype3<- prevSubtype3 %>%
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
  labs(title = paste0("Prevalence of antiparkinson drugs in people with ",subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name),", stratified by age and sex"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Prevalence")

DrugsPrevalenceStratifiedByAgeAndSexSubtype3Name <- paste0("DrugsPrevalence",subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name_camel),"StratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsPrevalenceStratifiedByAgeAndSexSubtype3Name),
    width = 20, height = 8)
print(DrugsPrevalenceStratifiedByAgeAndSexSubtype3, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                                Incidence                                   #
#                                                                            #
##############################################################################

incSubtype3 <- estimateIncidence(
  cdm = cdm,
  denominatorTable = paste0(subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name),"1y"),
  outcomeTable = drug_table_name
)

# 1.Plots for incidence of the drug use in subtype 3
DrugsIncidenceOverallSubtype3<- incSubtype3 %>%
  filter(denominator_age_group == '18 to 150', denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "COMTInhibitor", "COMT Inhibitors")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DopamineAgonists", "Dopamine Agonists")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MAOBInhibitors", "MAO-B Inhibitors")) %>%
  ggplot(aes(x = incidence_start_date, y=incidence_100000_pys, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, group = outcome_cohort_name, color = outcome_cohort_name)) +
  geom_errorbar(width=0)+
  geom_point() +
  scale_y_continuous(labels = label_comma()) +
  ggtitle(paste0("Incidence of antiparkinson drugs in people with ", subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name))) +
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceOverallSubtype3Name <- paste0("DrugsIncidence", subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name_camel),".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceOverallSubtype3Name),
    width = 10, height = 8)
print(DrugsIncidenceOverallSubtype3, newpage = FALSE)
dev.off()

# 2.Plots for incidence of the drug use in subtype 3, stratified by sex
DrugsIncidenceBothSubtype3<- incSubtype3 %>%
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
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~denominator_age_group) +
  labs(title = paste0("Incidence of antiparkinson drugs in people with ",subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name),", stratified by age"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceBothSubtype3Name <- paste0("DrugsIncidence",subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name_camel),"Both", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceBothSubtype3Name),
    width = 10, height = 8)
print(DrugsIncidenceBothSubtype3, newpage = FALSE)
dev.off()

# 3.Plots for incidence of the drug use in men with subtype 3, stratified by age
DrugsIncidenceMaleSubtype3<- incSubtype3 %>%
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
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~denominator_age_group) +
  labs(title = paste0("Incidence of antiparkinson drugs in men with ",subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name),", stratified by age"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("PIncidence (per 100,000 person-years)")

DrugsIncidenceMaleSubtype3Name <- paste0("DrugsIncidence",subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name_camel),"Male", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceMaleSubtype3Name),
    width = 10, height = 8)
print(DrugsIncidenceMaleSubtype3, newpage = FALSE)
dev.off()

# 4.Plots for incidence of the drug use in women with subtype 3, stratified by age
DrugsIncidenceFemaleSubtype3<- incSubtype3 %>%
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
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~denominator_age_group) +
  labs(title = paste0("Incidence of antiparkinson drugs in women with ",subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name),", stratified by age"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceFemaleSubtype3Name <- paste0("DrugsIncidence",subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name_camel),"Female", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceFemaleSubtype3Name),
    width = 10, height = 8)
print(DrugsIncidenceFemaleSubtype3, newpage = FALSE)
dev.off()

#5. Plots for incidence of drug use in subtype 3, stratified by both age and sex
DrugsIncidenceStratifiedByAgeAndSexSubtype3<- incSubtype3 %>%
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
  scale_y_continuous(labels = label_comma()) +
  facet_grid(~denominator_sex ~denominator_age_group) +
  labs(title = paste0("Incidence of antiparkinson drugs in people with ",subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name),", stratified by age and sex"))+
  labs(colour = "Parkinsonism Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  xlab("Time") + ylab("Incidence (per 100,000 person-years)")

DrugsIncidenceStratifiedByAgeAndSexSubtype3Name <- paste0("DrugsIncidence",subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name_camel),"StratifiedByAgeAndSex", ".pdf")

pdf(here("Results", db.name, "Plots", DrugsIncidenceStratifiedByAgeAndSexSubtype3Name),
    width = 20, height = 8)
print(DrugsIncidenceStratifiedByAgeAndSexSubtype3, newpage = FALSE)
dev.off()

##############################################################################
#                                                                            #
#                            Gather & export                                 #
#                                                                            #
##############################################################################
info(logger, paste0('Gathering results for antiparkinson drugs in ', subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name)))
exportIncidencePrevalenceResults(resultList = list("Prevalence of antiparkinson drugs" = prevSubtype3, "Incidence of antiparkinson drugs" = incSubtype3),
                                 zipName= paste0(db.name, "_IncidencePrevalenceResultsDrugsIn", subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name_camel)),
                                 outputFolder=here::here("Results", db.name))
