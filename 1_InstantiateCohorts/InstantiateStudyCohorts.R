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
                                       cohortTableName = subtype_table_name, 
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
                         cohortSet =  outcome_cohorts_subtypes,
                         cohortTableName = subtype_table_name, 
                         overwrite = TRUE
)
