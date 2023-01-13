library(CDMConnector)
library(here)

################################################################################
#                                                                              #
#                              Setting up for Drugs                            #
#                                                                              #
################################################################################

outcome_cohorts_drugs <- CDMConnector::readCohortSet(here(
  "1_InstantiateCohorts",
  "OutcomeCohortsDrugs"
))

databaseName <- "..."
server     <- Sys.getenv("DB_SERVER_name_database")
server_dbi <- Sys.getenv("DB_SERVER_DBI_name_database")
user       <- Sys.getenv("DB_USER")
password   <- Sys.getenv("DB_PASSWORD")
port       <- Sys.getenv("DB_PORT")
host       <- Sys.getenv("DB_HOST")
vocabularyDatabaseSchema <- "..."
dbmsName <- "..."
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = server_dbi, port = port, host = host, user = user,
                      password = password)
cdm <- CDMConnector::cdm_from_con(con = con,
                                  cdm_schema = '...',
                                  write_schema = '...')
                                  
cdm <- CDMConnector::generateCohortSet(cdm = cdm,
                                       cohortSet = outcome_cohorts_drugs,
                                       cohortTableName = '...', #The name of your choice for this table associated to antiparkinson drugs
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
                         cohortTableName = '...', #The name of your choice for this table associated to parkinsonism subtypes
                         overwrite = TRUE
)
