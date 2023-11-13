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
                                       name = "parkinson_drugs", 
                                       overwrite = TRUE
) 
################################################################################
#                                                                              #
#                              Setting up for Subtypes                         #
#                                                                              #
################################################################################

outcome_cohorts_subtypes <- readCohortSet(here(
  "1_InstantiateCohorts",
  "OutcomeCohortsSubtypes"
))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_subtypes,
                         name = "parkinson_subtypes", 
                         overwrite = TRUE
)

outcome_cohorts_subtypes_1y <- readCohortSet(here(
  "1_InstantiateCohorts",
  "OutcomeCohortsSubtypes_1y"
))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_subtypes_1y,
                         name = "parkinson_subtypes_1y", 
                         overwrite = TRUE
)

subtypesCohortSet<-cohortSet(cdm[["parkinson_subtypes"]]) %>% 
  mutate(cohort_name = case_when(cohort_name == "DrugInducedParkinsonism" ~ "Drug Induced Parkinsonism",
                                 cohort_name == "Parkinsonism" ~ "Parkinsonism",
                                 cohort_name == "ParkinsonsDisease" ~ "Parkinson's Disease",
                                 cohort_name == "VascularParkinsonism" ~ "Vascular Parkinsonism")) %>%
  mutate(cohort_name_camel = case_when(cohort_name == "Drug Induced Parkinsonism" ~ "DrugInducedParkinsonism",
                                       cohort_name == "Parkinsonism" ~ "Parkinsonism",
                                       cohort_name == "Parkinson's Disease" ~ "ParkinsonsDisease",
                                       cohort_name == "Vascular Parkinsonism" ~ "VascularParkinsonism"))

subtypesCohortSet_1y<-cohortSet(cdm[["parkinson_subtypes_1y"]]) %>% 
  mutate(cohort_name = case_when(cohort_name == "DrugInducedParkinsonism" ~ "Drug Induced Parkinsonism",
                                 cohort_name == "Parkinsonism" ~ "Parkinsonism",
                                 cohort_name == "ParkinsonsDisease" ~ "Parkinson's Disease",
                                 cohort_name == "VascularParkinsonism" ~ "Vascular Parkinsonism")) %>%
  mutate(cohort_name_camel = case_when(cohort_name == "Drug Induced Parkinsonism" ~ "DrugInducedParkinsonism",
                                       cohort_name == "Parkinsonism" ~ "Parkinsonism",
                                       cohort_name == "Parkinson's Disease" ~ "ParkinsonsDisease",
                                       cohort_name == "Vascular Parkinsonism" ~ "VascularParkinsonism"))
