################################################################################
#                                                                              #
#                              Setting up for Drugs                            #
#                                                                              #
################################################################################

outcome_cohorts_drugs <- CDMConnector::readCohortSet(here(
  "1_InstantiateCohorts",
  "OutcomeCohortsDrugs"
))
                                  
cdm <- CDMConnector::generateCohortSet(cdm = cdm,
                                       cohortSet = outcome_cohorts_drugs,
                                       name = drug_table_name, 
                                       overwrite = TRUE
) 
################################################################################
#                                                                              #
#                              Setting up for Subtypes                         #
#                                                                              #
################################################################################

outcome_cohorts_subtypes_incidence <- readCohortSet(here(
  "1_InstantiateCohorts",
  "OutcomeCohortsSubtypes",
  "Incidence"
))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_subtypes_incidence,
                         name = subtype_table_inc, 
                         overwrite = TRUE
)

outcome_cohorts_subtypes_prevalence <- readCohortSet(here(
  "1_InstantiateCohorts",
  "OutcomeCohortsSubtypes",
  "Prevalence"
))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_subtypes_prevalence,
                         name = subtype_table_prev, 
                         overwrite = TRUE
)


subtypesCohortSet<-cohortSet(cdm[[subtype_table_inc]]) %>% 
  mutate(cohort_name = case_when(cohort_name == "DrugInducedParkinsonismIncident" ~ "Drug Induced Parkinsonism",
                                 cohort_name == "ParkinsonismIncident" ~ "Parkinsonism",
                                 cohort_name == "ParkinsonsDiseaseIncident" ~ "Parkinson's Disease",
                                 cohort_name == "VascularParkinsonismIncident" ~ "Vascular Parkinsonism")) %>%
  mutate(cohort_name_camel = case_when(cohort_name == "Drug Induced Parkinsonism" ~ "DrugInducedParkinsonism",
                                       cohort_name == "Parkinsonism" ~ "Parkinsonism",
                                       cohort_name == "Parkinson's Disease" ~ "ParkinsonsDisease",
                                       cohort_name == "Vascular Parkinsonism" ~ "VascularParkinsonism"))