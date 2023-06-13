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
