# table names----
subtype_table_inc<-paste0(outcome_table_stem,"_subtype_incidence")
subtype_table_prev<-paste0(outcome_table_stem,"_subtype_prevalence")

drug_table_name<-paste0(outcome_table_stem,"_drug")

# output files ----
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

start<-Sys.time()

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
source(here("2_Analysis","incidencePrevalenceDrugsStrata2.R"))
source(here("2_Analysis","incidencePrevalenceDrugsStrata3.R"))
source(here("2_Analysis","incidencePrevalenceDrugsStrata4.R"))
info(logger, 'ANALYSIS COMPLETE')
# add code for combining and exporting results ---

print("Done!")
print("-- If all has worked, there should now be zip folders with your results as well as some default plots in the output folder (Results folder) to share")
print("-- Thank you for running the study!")
time_taken<-Sys.time()-start
readLines(log_file)

