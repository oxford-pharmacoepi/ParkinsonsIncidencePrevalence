cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema,
                                  cohort_tables = c("parkinson_subtypes"))
### denominators
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = as.Date(c("2007-01-01", "2021-12-31")),
  ageGroup = list(c(18,150)),
  sex = "Both",
  daysPriorHistory = 365
)

### overall inc
inc_overall <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "parkinson_subtypes",
  interval = "overall",
  temporary = F,
  returnParticipants = T
)

participants_parkinsonism <- list()

for (i in (1:length(cohortSet(cdm$parkinson_subtypes) %>% dplyr::pull(cohort_definition_id)))){
  participants_parkinsonism[[i]] <- participants(inc_overall, analysisId = i) %>%
    dplyr::filter(!is.na(outcome_start_date)) %>%
    dplyr::mutate(cohort_definition_id = i) %>%
    dplyr::inner_join(cohortSet(cdm$parkinson_subtypes), by = "cohort_definition_id", copy = T) %>%
    dplyr::select(subject_id, outcome_start_date, cohort_definition_id, cohort_name) %>%
    dplyr::rename(index_date = outcome_start_date)
}

char_parkinsonism <- Reduce(dplyr::union_all, participants_parkinsonism)

cdm[["parkinson_total"]] <- 
  char_parkinsonism %>%
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "parkinson_total", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort 
cdm[["parkinsonism"]] <- cdm[["parkinson_total"]] %>%
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>%
  computeQuery(
    name = "parkinsonism", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort set
parkinsonism_set <- cdm[["parkinson_total"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "parkinsonism_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
parkinsonism_count <- cdm[["parkinson_total"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "parkinsonism_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

stem_table <- "pchar"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)
cdm_char[["parkinsonism"]] <- newGeneratedCohortSet(cohortRef = cdm[["parkinsonism"]],
                                                                    cohortSetRef = parkinsonism_set,
                                                                    cohortCountRef = parkinsonism_count,
                                                    overwrite = T)
cdm_char <- cdmSubsetCohort(cdm_char, "parkinsonism", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - IMMINENT COHORT")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char)
cdm_char <- generateDrugUtilisationCohortSet(cdm = cdm_char, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - IMMINENT COHORT")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - IMMINENT COHORT")
result_pip <- cdm_char[["parkinsonism"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(18, 30), c(31, 40), c(41, 50), c(51, 60), c(61, 70), c(71,80), c(81,90), c(91,100), c(101,150)),
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence", value = "count", window = c(-365, 0)
      ),
      "Medications" = list(
        tableName = "drug_era", value = "count", window = c(-365, 0)
      )
    ), 
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = medications, value = "flag", window = c(-365, 0)
      ),
      "Conditions" = list(
        targetCohortTable = conditions, value = "flag", window = c(-Inf, 0)
      )
    )
  )
write_csv(result_pip, here(output.folder, "table_one_parkinsonsIP.csv"))
info(logger, "TABLE 1 IS DONE")

### reformat 
table_one_1 <- result_pip %>%
  dplyr::filter(group_level == "Druginducedparkinsonism", strata_level == "Overall")

table_one_2 <- result_pip %>%
  dplyr::filter(group_level == "Parkinsonism", strata_level == "Overall")

table_one_3 <- result_pip %>%
  dplyr::filter(group_level == "Parkinsonsdisease", strata_level == "Overall")

table_one_4 <- result_pip %>%
  dplyr::filter(group_level == "Vascularparkinsonism", strata_level == "Overall")

reformatted_table_1 <- reformat_table_one(table_one_1 = table_one_1,
                   table_one_2 = table_one_2,
                   table_one_3 = table_one_3,
                   table_one_4 = table_one_4)

write_csv(reformatted_table_1, here(output.folder, "table_one_parkinsonsIP_reformatted.csv"))
info(logger, "REFORMATTING TABLE 1 IS DONE")
####### attrition reports
write_csv(inc_attr, here(output.folder, "inc_attr.csv"))
