# checking working directory ----------------------------------------------

# Checking to make sure the working directory is set to the correct location for downstream analyses
# If using RStudio, double-click the Rproj file to start a new session with the proper working directory
# If using the command line, set manually using setwd() prior to proceeding
# If setting while running generate_all_figures.R via the command line, make sure your specifying the parent directory
# for your first argument
if (!any(grepl("\\.Rproj", list.files()))) {
  stop("Please set the working directory to the parent folder containing the R project file (.Rproj).")
}



# checking packages -------------------------------------------------------

# Checking to see if all required packages are installed and letting user know if any are misisng
# List of packages used in this analysis
survey_packages <- c("tidytext", "ggwordcloud", "grid", "gridExtra", "ggpubr",
                     "tidyverse", "viridis", "mapproj", "fiftystater", "ggalt")

# Checks to see which required packages are missing
packages_to_install <- survey_packages[!(survey_packages %in% installed.packages()[,"Package"])]

# Errors out script and tells user which packages are missing
if (length(packages_to_install) > 0) {
  stop(paste("Please install the following packages required for downstream analyses:",
             paste(packages_to_install, collapse = ", ")))
}


