# This script is used to generate all of the figures for the survey report
# It can be run via an interactive session by sourcing or via the command line

# Command line usage: Rscript generate_all_figures.R work_dir survey_csv
# work_dir: parent level folder for survey analysis
# survey_csv: relative location and name of the raw survey data to be used (ex: data/2018_survey.csv)


# setting up environment --------------------------------------------------

# Allowing script to run from the command line
args <- commandArgs(trailingOnly = TRUE)

# Setting variables based on command line inputs (if available)
# Variables have values of "NA" unless assigned via the command line
work_dir <- args[1] # location of working directory (should set to main directory for project)
survey_csv <- args[2] # location of survey data to be used for analysis (used during data import as part of tidy_survey.R)

# Setting the working directory if provided via the command line
if (!is.na(work_dir)){
  setwd(work_dir)
} 



# running scripts to generate the various plots ---------------------------

# creating all the plots
source("code/plot_degree_locations.R")
source("code/plot_multichoice_responses.R")
source("code/plot_text_responses.R")