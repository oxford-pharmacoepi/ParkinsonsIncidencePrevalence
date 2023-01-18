# table names----
subtype_table_name<-paste0(outcome_table_stem,"_subtype")
drug_table_name<-paste0(outcome_table_stem,"_drug")

# subtype names
subtype1 <- outcome_cohorts_subtypes %>% filter (cohortId == "1")
subtype1name <- paste0(subtype1[,"cohortName"])

subtype2 <- outcome_cohorts_subtypes %>% filter (cohortId == "2")
subtype2name <- paste0(subtype2[,"cohortName"])

subtype3 <- outcome_cohorts_subtypes %>% filter (cohortId == "3")
subtype3name <- paste0(subtype3[,"cohortName"])

subtype4 <- outcome_cohorts_subtypes %>% filter (cohortId == "4")
subtype4name <- paste0(subtype4[,"cohortName"])
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

# Run incidence rate analysis ----
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
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the study!")
Sys.time()-start
readLines(log_file)

