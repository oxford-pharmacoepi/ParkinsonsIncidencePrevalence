# ADD NECESSARY PACKAGES
#install.packages("renv") # if not already installed, install renv from CRAN
renv::activate() 
renv::restore() # this should prompt you to install the various packages required for the study

library(CirceR)
library(IncidencePrevalence)
library(here)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(readr)
library(log4r)
library(stringr)
library(CDMConnector)
library(ggplot2)
library(tictoc)
library(testthat)
library(scales)
library(PatientProfiles)
library(DrugUtilisation)
library(CodelistGenerator)

# database metadata and connection details -----
# The name/ acronym for the database
db.name<-"CPRD_GOLD"

# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
# to set the location within the project with folder called "ouput, we can use: here("output")
# but this file path could be set to somewhere else
output.folder<-here("Results", db.name)
output.folder.plots<-here("Results", db.name, "Plots")

# Specify databaseConnector connection details -----
# database connection details
# connect to database
user<-Sys.getenv("DB_USER")
password<- Sys.getenv("DB_PASSWORD")
port<-Sys.getenv("DB_PORT") 
host<-Sys.getenv("DB_HOST") 
server_dbi<-Sys.getenv("DB_SERVER_cdm_gold_202207_dbi") 
dbmsName <- "postgresql"

# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"public"

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema<-cdm_database_schema

# The name of the schema where results tables will be created 
results_database_schema<-c(schema = "results", prefix = "xc_parkinson_")

# Name of outcome table in the result table where the outcome cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten 
# outcome_table_stem<-"p_ip"

# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(con = db, 
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema)

# to check whether the DBI connection is correct, 
# running the next line should give you a count of your person table
cdm$person %>% 
  tally()

# Run the study ------
source(here("RunStudy.R"))
# after the study is run you should have zip folders in your output folder (Results) to share

dbDisconnect(db)