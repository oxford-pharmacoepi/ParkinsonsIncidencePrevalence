# table names----
subtype_table_name<-paste0(outcome_table_stem,"_subtype")
drug_table_name<-paste0(outcome_table_stem,"_drug")

# output files ----
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

tictoc::tic()

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# instantiate study cohorts ----
info(logger, 'INSTANTIATING STUDY COHORTS')
source(here("1_InstantiateCohorts","InstantiateStudyCohorts.R"))
info(logger, 'GOT STUDY COHORTS')

# Run incidence prevalence analysis ----
info(logger, 'RUNNING PREVALENCE AND INCIDENCE RATE ANALYSIS FOR PARKINSONISM AND ITS SUBTYPES')
source(here("2_Analysis","IncidencePrevalenceSubtypes(GeneralPopulation).R"))
info(logger, 'ANALYSIS COMPLETE!')

info(logger, 'RUNNING PREVALENCE AND INCIDENCE RATE ANALYSIS FOR DRUG USE IN THE GENERAL POPULATION')
source(here("2_Analysis","IncidencePrevalenceDrugs(GeneralPopulation).R"))
info(logger, 'ANALYSIS COMPLETE')

info(logger, 'RUNNING PREVALENCE AND INCIDENCE RATE ANALYSIS FOR DRUG USE IN THE DISEASE STRATA')
source(here("2_Analysis","incidencePrevalenceDrugsStrata1.R"))
info(logger, paste0("Incidence and prevalence of drugs in ", subtypesCohortSet %>% filter(cohort_definition_id==1) %>% pull(cohort_name), " is done"))

source(here("2_Analysis","incidencePrevalenceDrugsStrata2.R"))
info(logger, paste0("Incidence and prevalence of drugs in ", subtypesCohortSet %>% filter(cohort_definition_id==2) %>% pull(cohort_name), " is done"))

source(here("2_Analysis","incidencePrevalenceDrugsStrata3.R"))
info(logger, paste0("Incidence and prevalence of drugs in ", subtypesCohortSet %>% filter(cohort_definition_id==3) %>% pull(cohort_name), " is done"))

source(here("2_Analysis","incidencePrevalenceDrugsStrata4.R"))
info(logger, paste0("Incidence and prevalence of drugs in ", subtypesCohortSet %>% filter(cohort_definition_id==4) %>% pull(cohort_name), " is done"))

# add code for combining and exporting results ---

print("Done!")
print("-- If all has worked, there should now be zip folders with your results as well as some default plots in the output folder (Results folder) to share")
print("-- Thank you for running the study!")
tictoc::toc()
readLines(log_file)
