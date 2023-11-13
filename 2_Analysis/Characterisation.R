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
                                                                    cohortCountRef = parkinsonism_count)
cdm_char <- cdmSubsetCohort(cdm_char, "parkinsonism", verbose = T)
