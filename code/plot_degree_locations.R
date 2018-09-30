# loading dependencies ----------------------------------------------------

# loading tidyverse and dataset
source("code/tidy_survey.R")



# reading in data ---------------------------------------------------------

# built in data sets don't include us territories so made new set
# reading in list of us states/territories with abbreviations
us_states_territories <- read_csv("data/us_state_territories.csv")

# converting df to be all lower case for easier use later
us_states_territories <- us_states_territories %>%
  mutate_all(tolower)



# maps of degree regions ------------------------------------------------

# NOTE: need to adjust question to apply to Ph.D.s, M.D.s, and J.D.s (do J.D.s count as postdocs?)
# NOTE: some US citizens completed degrees outside the US, need to adjust question if/else flow for citizenship question
# NOTE: need to give a list of states/countries to select from



########## generating map of regions within the US ##########

# creating function to extract the us state from the responses and convert all abbreviations to full names for uniformity and plotting
# added in step to filter data to only relevant question about regions within us
extract_us_region <- function(df) {
  us_region <- c(us_states_territories$abb, us_states_territories$name) # creating vector of states and abbreviations
  us_abb_to_region <- us_states_territories$name %>% # creates a named vector to enable extraction based on full name
    set_names(us_states_territories$abb)
  data <- df %>%
    filter(question_no == "Q10" & !is.na(response)) %>% 
    mutate(response = str_replace_all(response, "_", " "),
           region = tolower(case_when(str_detect(response, regex(paste(paste0("\\b", us_region, "\\b"), collapse = "|"), ignore_case = T)) ~ # detecting anything in any case that matches string with word boundaries on both ends
                                          str_extract(response, regex(paste(paste0("\\b", us_region, "\\b"), collapse = "|"), ignore_case = T)),  # extracting the state information to a new col
                                        TRUE ~ NA_character_)), 
           region = ifelse(region %in% names(us_abb_to_region), us_abb_to_region[region], region)) # converts region to state abbreviation based on presence of state name
  return(data)
}

# extracting us region data and adding as a new col creating state_data df
us_data <- extract_us_region(tidy_survey_data)

# # getting counts for each state/territory
# us_data %>% 
#   group_by(question_no, subquestion_no, question, region) %>% # groups by question and response to provide summary stats
#   summarize(n = n()) # creates col for number of a given response (n)



# making function to generate counts of regions
get_region_counts <- function(region_df, parent_df=tidy_survey_data) {
  # requires parent df
  region_number <- parent_df %>% 
    filter(question_no == "Q10" & !is.na(response) | question_no == "Q11" & !is.na(response)) %>% 
    select(response_id) %>%
    unique(.) %>%
    nrow()
  # requires us only df
  data <- region_df %>% 
    group_by(question_no, subquestion_no, question, region) %>% # groups by question and response to provide summary stats
    summarize(n = n()) %>% # creates col for number of a given response (n)
    mutate(percent_freq = n/region_number*100) %>% 
    ungroup()
  return(data)
}

get_region_counts(us_data)





# # testing get_region_counts
# get_location_counts(us_data)


##### turn this into a function ####
# saving count data for us states and filling in missing states
us_region_counts <- get_region_counts(us_data) %>% 
  select(region, percent_freq) %>% # selects only the region and count cols
  right_join(select(us_states_territories, name), by = c("region" = "name")) # joining with list of states/territories to add any missing


# # working on plotting location information
# crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
# crimesm <- reshape2::melt(crimes, id = 1)
# if (require(maps)) {
#   states_map <- map_data("state")
#   ggplot(crimes, aes(map_id = state)) +
#     geom_map(aes(fill = Murder), map = states_map) +
#     expand_limits(x = states_map$long, y = states_map$lat)
#   
#   last_plot() + coord_map()
#   ggplot(crimesm, aes(map_id = state)) +
#     geom_map(aes(fill = value), map = states_map) +
#     expand_limits(x = states_map$long, y = states_map$lat) +
#     facet_wrap( ~ variable)
# }

# packages needed to plot map
library(mapproj) # needed to maintain dimensions
library(fiftystater) # needed to plot HI and AK in insets
library(viridis) # needed for color scaling

# NOTE: only plots us states, still need to work on including us territories
# NOTE: need to move alaska and hawaii to be on plot and draw box around

# making function for mapping locations within the us
plot_us_degree_regions <- function(df) {
  states_map <- map_data("state")
  plot <- df %>% 
    ggplot(aes(map_id = region)) +
    geom_map(aes(fill = percent_freq), map = states_map, color = "black", size = 0.2) + # plots the map
    expand_limits(x = states_map$long, y = states_map$lat) +
    scale_fill_continuous(type = "viridis", na.value = "white") + # making NA values = white and scaling using viridis palette
    labs(title = "Locations of U.S. Ph.D. Granting Institutions for University of Michigan Postdocs",
         fill = "Percent of Respondents") +
    coord_map() + # gives map proper dimensions
    #fifty_states_inset_boxes() + # package function to add boxes around insets for AK and HI
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"))
  return(plot)
}

# plotting the data
#us_degree_regions <- 
plot_us_degree_regions(us_region_counts)


ggsave(filename = "results/us_degree_locations.png", plot = us_degree_regions, width = 20, dpi = 300)


# # playing around with more customized maps
# library(units)
# library(tigris)
# us.map <- tigris::states(cb = T, year = 2015)
# test <- us_location_counts
# test <- fortify(us.map, region = "GEOID")

# Things to work on:
# Include map of world around it
# Include us territories
# Change percent of respondents to be based on total dataset


########## END ##########



########## generating map of locations outside the US ##########

# list of countries available for mapping
countries <- map_data("world") %>% # making list of countries with available mapping information
  pull(region) %>% # pulls out list of countries
  unique(.) # makes list of unique countries

# creating function to extract country names from responses
extract_int_region <- function(df) {
  countries <- map_data("world") %>% # making list of countries with available mapping information
    pull(region) %>% # pulls out list of countries
    unique(.) # makes list of unique countries
  data <- df %>%
    filter(question_no == "Q11" & !is.na(response)) %>% # filters down to country info
    mutate(response = str_replace_all(response, "_", " "), # replaces all "_" with spaces to allow text parsing
           region = tolower(case_when(str_detect(response, regex(paste(paste0("\\b", countries, "\\b"), collapse = "|"), ignore_case = T)) ~ # detecting anything in any case that matches country names with word boundaries on both ends
                                          str_extract(response, regex(paste(paste0("\\b", countries, "\\b"), collapse = "|"), ignore_case = T)),  # extracting the country information to a new col
                                        TRUE ~ NA_character_))) # any country not in the list of available countries is labelled with NA
    return(data)
}

# creating df of locations
int_data <- extract_int_region(tidy_survey_data)

library(ggalt) # coord_proj

# calculating freqs of countries
int_region_counts <- get_region_counts(int_data) %>% 
  select(region, percent_freq) %>% # selects only the region and count cols 
  right_join(select(as_data_frame(tolower(countries)), value), by = c("region" = "value")) # joining with list of countries to add missing ones

# world_map <- map_data("world") %>% 
#   mutate(region = tolower(region)) 
# 
# int_region_counts %>% 
#   ggplot(aes(map_id = region)) +
#   geom_map(map=world_map, aes(fill=percent_freq), colour="black", size=0.5) +
#   expand_limits(x = world_map$long, y = world_map$lat) +
#   scale_fill_continuous(type = "viridis", na.value = "white") +
#   coord_proj("+proj=wintri")

plot_int_degree_regions <- function(df) {
  world_map <- map_data("world") %>% 
    mutate(region = tolower(region))
  
  plot <- df %>% 
    ggplot(aes(map_id = region)) +
    geom_map(aes(fill = percent_freq), map = world_map, colour = "black", size = 0.5) + # plots the map
    expand_limits(x = world_map$long, y = world_map$lat) +
    scale_fill_continuous(type = "viridis", na.value = "white") + # making NA values = white and scaling using viridis palette
    labs(title = "Locations of Ph.D. Granting Institutions for University of Michigan Postdocs",
         fill = "Percent of Respondents") +
    coord_proj("+proj=wintri")
  # #fifty_states_inset_boxes() + # package function to add boxes around insets for AK and HI
  #   theme(axis.title = element_blank(),
  #         axis.text = element_blank(),
  #         axis.ticks = element_blank(),
  #         panel.grid = element_blank(),
  #         panel.background = element_rect(fill = "white"))
  return(plot)
}


plot_int_degree_regions(int_region_counts)








########## END ##########

detach("package:mapproj", unload = TRUE)
detach("package:maps", unload = TRUE)

