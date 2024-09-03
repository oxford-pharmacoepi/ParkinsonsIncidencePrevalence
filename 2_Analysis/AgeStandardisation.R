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
  ageGroup = list(c(18, 19), c(20, 24), 
                  c(25, 29), c(30, 34),
                  c(35, 39), c(40, 44),
                  c(45, 49), c(50, 54),
                  c(55, 59), c(60, 64),
                  c(65, 69), c(70, 74),
                  c(75, 79), c(80, 84),
                  c(85, 89), c(91, 150)),
  sex = c("Female", "Male", "Both"),
  daysPriorHistory = 365
)

info(logger, 'Age Standardisation for prevalence')
prevSubtypes <- estimatePeriodPrevalence(cdm = cdm,
                                         denominatorTable = "denominator_general_pop",
                                         outcomeTable = "parkinson_subtypes",
                                         minCellCount = 0)

ESP13 <- readr::read_csv(here("2_Analysis", "AgeStandards", "ESP13.csv"), 
                         show_col_types = FALSE) |>
  dplyr::filter(!(age_group %in% c("0 to 17", "all ages"))) |>
  dplyr::rename("pop" = "count",
                "denominator_age_group" = "age_group")

prevSubtypesOverallStd <- prevSubtypes %>% 
  filter(denominator_sex == "Both",
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    prevalence_start_date,            
    n_cases,                       
    n_population,                
    prevalence,
    prevalence_95CI_lower,
    prevalence_95CI_upper,
    outcome_cohort_name,            
    cdm_name,                  
    denominator_sex,                   
    denominator_age_group,
    age_standard))

prevSubtypesFemaleStd <- prevSubtypes %>% 
  filter(denominator_sex == "Female",
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    prevalence_start_date,            
    n_cases,                       
    n_population,                
    prevalence,
    prevalence_95CI_lower,
    prevalence_95CI_upper,
    outcome_cohort_name,            
    cdm_name,                  
    denominator_sex,                   
    denominator_age_group,
    age_standard))

prevSubtypesMaleStd <- prevSubtypes %>% 
  filter(denominator_sex == "Male",
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    prevalence_start_date,            
    n_cases,                       
    n_population,                
    prevalence,
    prevalence_95CI_lower,
    prevalence_95CI_upper,
    outcome_cohort_name,            
    cdm_name,                  
    denominator_sex,                   
    denominator_age_group,
    age_standard))

outcomes <- prevSubtypesOverallStd$outcome_cohort_name %>% unique()

agestandardizedprev <- list()

for(i in 1:length(outcomes)){
  
  prevalence_estimates_i <- prevSubtypesOverallStd %>%
    filter(outcome_cohort_name == outcomes[[i]])
  
  agestandardizedprev[[i]] <- dsr::dsr(
    data = prevalence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_cases,       # column containing number of deaths per stratum 
    fu = n_population, # column containing number of population per stratum person years
    subgroup = prevalence_start_date,   
    refdata = ESP13, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 1,           # we want rates per 100.000 population
    decimals = 10) 
  
  agestandardizedprev[[i]] <- agestandardizedprev[[i]] %>%
    select("Subgroup", "Numerator", "Denominator", 
           `Crude Rate (per 1)`, `95% LCL (Crude)`, `95% UCL (Crude)`) %>% 
    mutate(outcome_cohort_name = outcomes[[i]]) %>% 
    rename("prevalence_start_date" = "Subgroup",
           "n_events" = "Numerator",
           "person-years" = "Denominator",
           "standardised_prevalence" = `Crude Rate (per 1)`,
           "standardised_prevalence_lower" = `95% LCL (Crude)`,
           "standardised_prevalence_upper" = `95% UCL (Crude)`) %>% 
    mutate(denominator_sex = "Both",
           denominator_age_group = "18 to 150",
           cdm_name = db.name,
           age_standard = "European Standard Population")
}

agestandardizedprevf <- list()

for(i in 1:length(outcomes)){
  
  prevalence_estimates_i <- prevSubtypesFemaleStd %>%
    filter(outcome_cohort_name == outcomes[[i]])
  
  agestandardizedprevf[[i]] <- dsr::dsr(
    data = prevalence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_cases,       # column containing number of deaths per stratum 
    fu = n_population, # column containing number of population per stratum person years
    subgroup = prevalence_start_date,   
    refdata = ESP13, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 1,           # we want rates per 100.000 population
    decimals = 10) 
  
  agestandardizedprevf[[i]] <- agestandardizedprevf[[i]] %>% 
    select("Subgroup", "Numerator", "Denominator", 
           `Crude Rate (per 1)`, `95% LCL (Crude)`, `95% UCL (Crude)`) %>% 
    mutate(outcome_cohort_name = outcomes[[i]]) %>% 
    rename("prevalence_start_date" = "Subgroup",
           "n_events" = "Numerator",
           "person-years" = "Denominator",
           "standardised_prevalence" = `Crude Rate (per 1)`,
           "standardised_prevalence_lower" = `95% LCL (Crude)`,
           "standardised_prevalence_upper" = `95% UCL (Crude)`) %>% 
    mutate(denominator_sex = "Female",
           denominator_age_group = "18 to 150",
           cdm_name = db.name,
           age_standard = "European Standard Population")
}

agestandardizedprevm <- list()

for(i in 1:length(outcomes)){
  
  prevalence_estimates_i <- prevSubtypesMaleStd %>%
    filter(outcome_cohort_name == outcomes[[i]])
  
  agestandardizedprevm[[i]] <- dsr::dsr(
    data = prevalence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_cases,       # column containing number of deaths per stratum 
    fu = n_population, # column containing number of population per stratum person years
    subgroup = prevalence_start_date,   
    refdata = ESP13, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 1,           # we want rates per 100.000 population
    decimals = 10) 
  
  agestandardizedprevm[[i]] <- agestandardizedprevm[[i]] %>%
    select("Subgroup", "Numerator", "Denominator", 
           `Crude Rate (per 1)`, `95% LCL (Crude)`, `95% UCL (Crude)`) %>% 
    mutate(outcome_cohort_name = outcomes[[i]]) %>% 
    rename("prevalence_start_date" = "Subgroup",
           "n_events" = "Numerator",
           "person-years" = "Denominator",
           "standardised_prevalence" = `Crude Rate (per 1)`,
           "standardised_prevalence_lower" = `95% LCL (Crude)`,
           "standardised_prevalence_upper" = `95% UCL (Crude)`) %>% 
    mutate(denominator_sex = "Male",
           denominator_age_group = "18 to 150",
           cdm_name = db.name,
           age_standard = "European Standard Population")
}
std_prev <- bind_rows(agestandardizedprev, agestandardizedprevm, agestandardizedprevf)

info(logger, 'Age Standardisation for incidence')
incSubtypes <- estimateIncidence(cdm = cdm,
                                 denominatorTable = "denominator_general_pop",
                                 outcomeTable = "parkinson_subtypes",
                                 minCellCount = 0)

incSubtypesOverallStd <- incSubtypes %>% 
  filter(denominator_sex == "Both",
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    incidence_start_date,            
    n_events,                       
    person_years,                
    incidence_100000_pys,
    incidence_100000_pys_95CI_lower,
    incidence_100000_pys_95CI_upper,
    outcome_cohort_name,            
    cdm_name,                  
    denominator_sex,                   
    denominator_age_group,
    age_standard))

incSubtypesFemaleStd <- incSubtypes %>% 
  filter(denominator_sex == "Female",
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    incidence_start_date,            
    n_events,                       
    person_years,                
    incidence_100000_pys,
    incidence_100000_pys_95CI_lower,
    incidence_100000_pys_95CI_upper,
    outcome_cohort_name,            
    cdm_name,                  
    denominator_sex,                   
    denominator_age_group,
    age_standard))

incSubtypesMaleStd <- incSubtypes %>% 
  filter(denominator_sex == "Male",
         analysis_interval == "years") %>% 
  mutate(age_standard = "Crude") %>% 
  select(c(
    incidence_start_date,            
    n_events,                       
    person_years,                
    incidence_100000_pys,
    incidence_100000_pys_95CI_lower,
    incidence_100000_pys_95CI_upper,
    outcome_cohort_name,            
    cdm_name,                  
    denominator_sex,                   
    denominator_age_group,
    age_standard))

outcomes <- incSubtypesOverallStd$outcome_cohort_name %>% unique()

agestandardizedinc <- list()

for(i in 1:length(outcomes)){
  
  incidence_estimates_i <- incSubtypesOverallStd %>%
    filter(outcome_cohort_name == outcomes[[i]])
  
  agestandardizedinc[[i]] <- dsr::dsr(
    data = incidence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_events,       # column containing number of deaths per stratum 
    fu = person_years, # column containing number of population per stratum person years
    subgroup = incidence_start_date,   
    refdata = ESP13, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 100000,           # we want rates per 100.000 population
    decimals = 2) 
  
  agestandardizedinc[[i]] <- agestandardizedinc[[i]] %>% 
    mutate(outcome_cohort_name = outcomes[[i]]) %>% 
    select("Subgroup", "Numerator", "Denominator", 
           `Crude Rate (per 1e+05)`, `95% LCL (Crude)`, `95% UCL (Crude)`, "outcome_cohort_name") %>% 
    rename("incidence_start_date" = "Subgroup",
           "n_events" = "Numerator",
           "person-years" = "Denominator",
           "standardised_incidence" = `Crude Rate (per 1e+05)`,
           "standardised_incidence_lower" = `95% LCL (Crude)`,
           "standardised_incidence_upper" = `95% UCL (Crude)`) %>% 
    mutate(denominator_sex = "Both",
           denominator_age_group = "18 to 150",
           cdm_name = db.name,
           age_standard = "European Standard Population")
}

agestandardizedincf <- list()

for(i in 1:length(outcomes)){
  
  incidence_estimates_i <- incSubtypesFemaleStd %>%
    filter(outcome_cohort_name == outcomes[[i]])
  
  agestandardizedincf[[i]] <- dsr::dsr(
    data = incidence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_events,       # column containing number of deaths per stratum 
    fu = person_years, # column containing number of population per stratum person years
    subgroup = incidence_start_date,   
    refdata = ESP13, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 100000,           # we want rates per 100.000 population
    decimals = 2) 
  
  agestandardizedincf[[i]] <- agestandardizedincf[[i]] %>% 
    mutate(outcome_cohort_name = outcomes[[i]]) %>% 
    select("Subgroup", "Numerator", "Denominator", 
           `Crude Rate (per 1e+05)`, `95% LCL (Crude)`, `95% UCL (Crude)`, "outcome_cohort_name") %>% 
    rename("incidence_start_date" = "Subgroup",
           "n_events" = "Numerator",
           "person-years" = "Denominator",
           "standardised_incidence" = `Crude Rate (per 1e+05)`,
           "standardised_incidence_lower" = `95% LCL (Crude)`,
           "standardised_incidence_upper" = `95% UCL (Crude)`) %>% 
    mutate(denominator_sex = "Female",
           denominator_age_group = "18 to 150",
           cdm_name = db.name,
           age_standard = "European Standard Population")
}

agestandardizedincm <- list()

for(i in 1:length(outcomes)){
  
  incidence_estimates_i <- incSubtypesMaleStd %>%
    filter(outcome_cohort_name == outcomes[[i]])
  
  agestandardizedincm[[i]] <- dsr::dsr(
    data = incidence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_events,       # column containing number of deaths per stratum 
    fu = person_years, # column containing number of population per stratum person years
    subgroup = incidence_start_date,   
    refdata = ESP13, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 100000,           # we want rates per 100.000 population
    decimals = 2) 
  
  agestandardizedincm[[i]] <- agestandardizedincm[[i]] %>% 
    mutate(outcome_cohort_name = outcomes[[i]]) %>% 
    select("Subgroup", "Numerator", "Denominator", 
           `Crude Rate (per 1e+05)`, `95% LCL (Crude)`, `95% UCL (Crude)`, "outcome_cohort_name") %>% 
    rename("incidence_start_date" = "Subgroup",
           "n_events" = "Numerator",
           "person-years" = "Denominator",
           "standardised_incidence" = `Crude Rate (per 1e+05)`,
           "standardised_incidence_lower" = `95% LCL (Crude)`,
           "standardised_incidence_upper" = `95% UCL (Crude)`) %>% 
    mutate(denominator_sex = "Male",
           denominator_age_group = "18 to 150",
           cdm_name = db.name,
           age_standard = "European Standard Population")
}

std_inc <- bind_rows(agestandardizedinc, agestandardizedincm, agestandardizedincf)
write.csv(std_inc, here::here("Results", paste0(db.name, "/", cdmName(cdm), "standardised_incidence_estimates.csv")), row.names = FALSE)
write.csv(std_prev, here::here("Results", paste0(db.name, "/", cdmName(cdm), "standardised_prevalence_estimates.csv")), row.names = FALSE)

info(logger, 'Age Standardisation plots')

std_inc %>%
  filter(denominator_age_group == '18 to 150', denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  mutate(outcome_cohort_name = factor(outcome_cohort_name, c("Parkinsonism", "PD", "VP", "DIP"))) %>% 
  ggplot(aes(x = incidence_start_date, y=standardised_incidence, ymin = standardised_incidence_lower, ymax = standardised_incidence_upper)) +
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
  xlab("Time") + ylab("Age Standardised Incidence (per 100,000 person-years)")

StdSubtypesIncidenceOverallName <- paste0("StandardisedSubtypesIncidenceOverallPopulation", ".png")
ggsave(file = here(ip_subtypes_paper_plots, StdSubtypesIncidenceOverallName), width = 18, height = 10, dpi = 600)

for (subtype in outcomes){
  std_inc %>%
    filter(!(denominator_sex == "Both") & outcome_cohort_name == subtype) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
    ggplot(aes(x = incidence_start_date, y=standardised_incidence, ymin = standardised_incidence_lower, ymax = standardised_incidence_upper, group = denominator_age_group, color = denominator_sex)) +
    scale_color_manual(values = c("Female" = "red", "Male" = "blue"))+
    geom_point(aes(), size = 4) +
    geom_errorbar(width=100)+
    labs(colour = "Sex") +
    scale_y_continuous(labels = label_comma()) +
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
          legend.title = element_text(size=20, face = "bold"))+
    xlab("Time") + ylab("Age Standardised Incidence (per 100,000 person-years)")
  
  SubtypesIncidenceStdByAge <- paste0("SubtypesIncidenceAgeStandardised", subtype, ".png")
  ggsave(file = here(ip_subtypes_paper_plots, SubtypesIncidenceStdByAge), width = 18, height = 10, dpi = 600)
}

std_prev %>%
  filter(denominator_age_group == '18 to 150', denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
  mutate(outcome_cohort_name = factor(outcome_cohort_name, c("Parkinsonism", "PD", "VP", "DIP"))) %>% 
  ggplot(aes(x = prevalence_start_date, y=standardised_prevalence, ymin = standardised_prevalence_lower, ymax = standardised_prevalence_upper)) +
  geom_point(aes(), size = 4) +
  geom_errorbar(width=100)+
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
        legend.position="none",
        plot.title = element_text(hjust = 1)) +
  xlab("Time") + ylab("Age Standardised Prevalence (per 100,000 person-years)")

StdSubtypesPrevalenceOverallName <- paste0("StandardisedSubtypesPrevalenceOverallPopulation", ".png")
ggsave(file = here(ip_subtypes_paper_plots, StdSubtypesPrevalenceOverallName), width = 18, height = 10, dpi = 600)

for (subtype in outcomes){
  std_prev %>%
    filter(!(denominator_sex == "Both") & outcome_cohort_name == subtype) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "DrugInducedParkinsonism", "DIP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "VascularParkinsonism", "VP")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ParkinsonsDisease", "PD")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "Parkinsonism", "Parkinsonism")) %>%
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "81 to 150", "Over 80")) %>%
    ggplot(aes(x = prevalence_start_date, y=standardised_prevalence, ymin = standardised_prevalence_lower, ymax = standardised_prevalence_upper, group = denominator_age_group, color = denominator_sex)) +
    scale_color_manual(values = c("Female" = "red", "Male" = "blue"))+
    geom_point(aes(), size = 4) +
    geom_errorbar(width=100)+
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, NA)
    ) +
    labs(colour = "Sex") +
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
          legend.title = element_text(size=20, face = "bold"))+
    xlab("Time") + ylab("Age Standardised Prevalence (per 100,000 person-years)")
  
  SubtypesPrevalenceStdByAge <- paste0("SubtypesPrevalenceAgeStandardised", subtype, ".png")
  ggsave(file = here(ip_subtypes_paper_plots, SubtypesPrevalenceStdByAge), width = 18, height = 10, dpi = 600)
}
