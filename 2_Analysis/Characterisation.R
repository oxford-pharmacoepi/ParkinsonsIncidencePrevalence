### pulling out instantiated tables
cdm <- CDMConnector::cdm_from_con(con = db, 
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema,
                                  cohort_tables = c(subtype_table_name,
                                                    subtype_table_name_1y,
                                                    drug_table_name))

### denominators
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_general_pop",
  cohortDateRange = as.Date(c("2007-01-01", NA)),
  ageGroup = list(c(18,150), c(18, 30),c(31,40),c(41,50),c(51,60),c(61,70),c(71,80),c(81,150)),
  sex = c("Female", "Male", "Both"),
  daysPriorHistory = 365
)

### overall inc
overall_denominator_id <- cohortSet(cdm$denominator_general_pop) %>% 
  dplyr::filter(age_group == "18 to 150") %>% 
  dplyr::filter(sex == "Both") %>% 
  dplyr::pull(cohort_definition_id)

inc_overall <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_general_pop",
  denominatorCohortId = overall_denominator_id,
  outcomeTable = subtype_table_name,
  interval = "overall",
  temporary = F,
  returnParticipants = T
)
