# loading dependencies ----------------------------------------------------

# loading tidyverse and dataset
source("code/tidy_survey.R")

# packages needed to plot maps
library(mapproj) # needed to maintain dimensions
library(fiftystater) # needed to plot HI and AK in insets of USA maps
library(viridis) # needed for color scaling
library(ggalt) # needed for coord_proj


# reading in data ---------------------------------------------------------

# built in data sets don't include us territories so made new set
# reading in list of us states/territories with abbreviations and
# converting df to be all lower case for easier use later
us_states_territories <- read_csv("data/us_state_territories.csv") %>% 
  mutate_all(tolower)



# response freq functions -------------------------------------------------

# making function to calc region percent from responses
get_region_counts <- function(region_df) {
  
  # requires us only df
  data <- region_df %>% 
    group_by(question_no, region) %>% # groups by question and response to provide summary stats
    summarize(n = n()) %>% # creates col for number of a given response (n)
    mutate(percent_freq = n/sum(n)*100) %>% # calculates percent of total responses
    ungroup()
  
  return(data)
}



# # used spread/gather to change responses for domestic location based on world location and vice versa
# # will be used for better calculation of statistics
# test <- tidy_survey_data %>% 
#   filter(question_no == "Q10" | question_no == "Q11") %>% 
#   mutate(response = str_replace_all(response, "_", " "),
#          region = tolower(case_when(question_no == "Q10" & str_detect(response, regex(paste(paste0("\\b", us_region, "\\b"), collapse = "|"), ignore_case = T)) ~ 
#                                       str_extract(response, regex(paste(paste0("\\b", us_region, "\\b"), collapse = "|"), ignore_case = T)),
#                                     question_no == "Q11" & str_detect(response, regex(paste(paste0("\\b", countries, "\\b"), collapse = "|"), ignore_case = T)) ~ 
#                                       str_extract(response, regex(paste(paste0("\\b", countries, "\\b"), collapse = "|"), ignore_case = T)),
#                                     TRUE ~ NA_character_)),
#          region = ifelse(region %in% names(us_abb_to_region), us_abb_to_region[region], region)) %>% 
#   select(response_id, question_no, region) %>% 
#   spread(key = question_no, value = region) %>% 
#   mutate(Q10 = case_when(!is.na(Q11) & is.na(Q10) ~ "international",
#                          TRUE ~ Q10),
#          Q11 = case_when(!is.na(Q10) & is.na(Q11) ~ "usa",
#                          TRUE ~ Q11)) %>% 
#   gather(key = "question_no", value = "region", Q10:Q11)
# 
# test2 <- test %>% 
#   filter(question_no == "Q10") %>% 
#   get_region_counts() %>% # calculates percent of each region
#   select(-question_no) %>% # selects only the region and count cols
#   right_join(select(us_states_territories, name), by = c("region" = "name")) # joining with list of states/territories to add any missing



# master function for generating response frequencies
# region should be set to either "state" for USA specific state mapping or "world" for country mapping
calc_degree_freq <- function(region, survey_df) {
  
  # creating vector of states and abbreviations
  us_region <- c(us_states_territories$abb, us_states_territories$name) 
  
  # creates a named vector for converting state abbreviations to full names
  us_abb_to_region <- us_states_territories$name %>% 
    set_names(us_states_territories$abb)
  
  # creating a list of all countries with data available for plotting
  countries <- map_data("world") %>% # making list of countries with available mapping information
    pull(region) %>% # pulls out list of countries
    unique(.) # makes list of unique countries
  
  # creating unified df for us regions and international regions
  # will be used for better calculation of statistics
  mapping_regions <- survey_df %>% 
    filter(question_no == "Q10" | question_no == "Q11") %>% 
    mutate(response = str_replace_all(response, "_", " "), # replaces all "_" with spaces to allow text parsing
           region = tolower(case_when(question_no == "Q10" & str_detect(response, regex(paste(paste0("\\b", us_region, "\\b"), collapse = "|"), ignore_case = T)) ~ # looking for any text matching us region names or abbreviations
                                        str_extract(response, regex(paste(paste0("\\b", us_region, "\\b"), collapse = "|"), ignore_case = T)), # pulling out any matched us region names/abbreviations
                                      question_no == "Q11" & str_detect(response, regex(paste(paste0("\\b", countries, "\\b"), collapse = "|"), ignore_case = T)) ~ # looking for any text matching world countries
                                        str_extract(response, regex(paste(paste0("\\b", countries, "\\b"), collapse = "|"), ignore_case = T)), # pulling out any matched country names
                                      TRUE ~ NA_character_)), # anything not matched is labelled as NA
           region = ifelse(region %in% names(us_abb_to_region), us_abb_to_region[region], region)) %>% # converts any us region abbreviations to full names for proper plotting on map 
    # following code is using spread/gather to change responses for domestic location based on world location and vice versa
    select(response_id, question_no, region) %>% # only maintaining cols with relevant data for plotting
    spread(key = question_no, value = region) %>% # making question_no values into separate cols
    mutate(Q10 = case_when(!is.na(Q11) & is.na(Q10) ~ "international", # if q11 was answered and q10 was NA, makes q10 "international" for use calculating response freqs
                           TRUE ~ Q10),
           Q11 = case_when(!is.na(Q10) & is.na(Q11) ~ "usa", # if q10 was answered and q11 was NA, makes q11 "usa" to account for people receiving degress from us institutions (missed otherwsie because of question split)
                           TRUE ~ Q11)) %>% 
    gather(key = "question_no", value = "region", Q10:Q11) # bringing the df back to a tidy format for plotting
  
  
  
  # calculating state responses
  if (region == "state") { 
    
    print("Calculating USA region frequencies")
    
    us_region_freqs <- mapping_regions %>% 
      filter(question_no == "Q10") %>% 
      get_region_counts() %>% # calculates percent of each region
      select(-question_no) %>% # selects only the region and count cols
      right_join(select(us_states_territories, name), by = c("region" = "name")) # joining with list of states/territories to add any missing
    
    return(us_region_freqs)
    
    
    
  # calculating world responses
  } else if (region == "world") { 
    
    print("Calculating international region frequencies")
    
    # calculating the frequency of responses for individual regions
    world_region_freqs <- mapping_regions %>% 
      filter(question_no == "Q11") %>% 
      get_region_counts() %>% # calculating world response rates by country
      select(-question_no) %>% # selects only the region and count cols
      right_join(select(as_data_frame(tolower(countries)), value), by = c("region" = "value")) # joining with list of countries to add missing ones
    
    # returning freq df
    return(world_region_freqs)
    
    
    
  } else {
    
    # if the function isn't used correctly, prints error message
    print("ERROR: Incorrect region assignment")
    
    
    
  }
}



# plotting functions ------------------------------------------------------

# making function for mapping locations within the us
plot_us_degree_freq <- function(df) {
  
  # plotting points are contained within the fifty_states df of the fifty_stater pkg (tried to do myself but not worth the effort)
  # using as alternative to map_data("state") because fifty_states includes hawaii and alaska as an inset
  
  # plotting function for us state data
  plot <- df %>% 
    ggplot(aes(map_id = region)) + # plotting by region using coordinate df and response freq
    geom_map(aes(fill = percent_freq), map = fifty_states, color = "black", size = 0.2) + # plots the map using fifty_states for coordinates
    fifty_states_inset_boxes() + # package function to add boxes around insets for AK and HI
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    scale_fill_continuous(type = "viridis", na.value = "white") + # making NA values = white and scaling using viridis palette
    labs(title = "Locations of U.S. Ph.D. Granting Institutions for University of Michigan Postdocs",
         fill = "Respondents (%)") +
    coord_map() + # gives map proper dimensions
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"))
  
  # returning plotted us map
  return(plot)
}



# creating function for plotting world map information
plot_world_degree_freq <- function(df) {
  
  # creating df of plotting coordinates for world map
  world_map <- map_data("world") %>% # getting data
    mutate(region = tolower(region)) # transforming all country names to lower case for matching with survey data
  
  # main plotting function to make world map
  plot <- df %>% 
    ggplot(aes(map_id = region)) + # map_id is required aes for geom_map
    geom_map(aes(fill = percent_freq), map = world_map, colour = "black", size = 0.25) + # plots the world map
    expand_limits(x = c(-179,179), y = c(-75,89)) + # expands plot limits to encompass data, x = longitude, y = latitude
    scale_fill_continuous(type = "viridis", na.value = "white") + # making NA values = white and scaling using viridis palette
    labs(title = "Locations of Ph.D. Granting Institutions for University of Michigan Postdocs", # setting chart labels
         fill = "Respondents (%)") +
    coord_proj(proj = "+proj=wintri") + # correcting for Earth curvature using Winkel tripel projection
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"))
  
  # returning the map
  return(plot)
}



# generating plots of degrees from USA ------------------------------------

# calculating response rates for individual states in USA
us_degree_freq <- calc_degree_freq("state", tidy_survey_data)

# plotting the data
us_degree_map <- plot_us_degree_freq(us_degree_freq)

# saving us degree freq map
# ggsave(filename = "results/us_degree_map.png", plot = us_degree_map, width = 20, dpi = 300)



# generating plots of degrees from world ----------------------------------

# calculating response rates for individual countries
world_degree_freq <- calc_degree_freq("world", tidy_survey_data)

# generating world map with postdoc degree freqs
world_degree_map <- plot_world_degree_freq(world_degree_freq)

# saving world degree freq map
# ggsave(filename = "results/world_degree_map.png", plot = world_degree_map, width = 20, dpi = 300)



# detaching conflicting packages ------------------------------------------

# detach("package:mapproj", unload = TRUE)
# detach("package:maps", unload = TRUE)


# notes -------------------------------------------------------------------

# Things to work on:
# Include map of world around us
# Include us territories
# Change percent of us respondents to be based on total dataset

# NOTE: need to adjust question to apply to Ph.D.s, M.D.s, and J.D.s (do J.D.s count as postdocs?)
# NOTE: some US citizens completed degrees outside the US, need to adjust question if/else flow for citizenship question
# NOTE: need to give a list of states/countries to select from
# NOTE: only plots us states, still need to work on including us territories
# NOTE: need to move alaska and hawaii to be on plot and draw box around
