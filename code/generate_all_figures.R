# This script is used to generate all of the figures for the survey report

# setting survey data to use ----------------------------------------------

# importing survey data
survey_data <- read_csv("data/raw_data/UMPDA_2018_cross-section_survey.csv")



# running scripts to generate the various plots ---------------------------

# creating all the plots
source("code/plot_degree_locations.R")
source("code/plot_multichoice_responses.R")
source("code/plot_text_responses.R")