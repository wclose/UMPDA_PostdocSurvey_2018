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



# master function for generating response frequencies
# region should be set to either "state" for USA specific state mapping or "world" for country mapping
# df is the dataframe containing the survey data
calc_degree_freq <- function(region, survey_df) {
  
  # creating vector of states and abbreviations
  us_region <- c(us_states_territories$abb, us_states_territories$name) 
  
  # creates a named vector for converting state abbreviations to full names
  us_abb_to_region <- us_states_territories$name %>% 
    set_names(us_states_territories$abb)
  
  # creating df of us regions (states)
  us_regions <- survey_df %>%
    filter(question_no == "Q10" & !is.na(response)) %>% # filters out non-us data
    mutate(response = str_replace_all(response, "_", " "), # replaces symbols from tidying data
           region = tolower(case_when(str_detect(response, regex(paste(paste0("\\b", us_region, "\\b"), collapse = "|"), ignore_case = T)) ~ # detecting anything in any case that matches string with word boundaries on both ends
                                        str_extract(response, regex(paste(paste0("\\b", us_region, "\\b"), collapse = "|"), ignore_case = T)),  # extracting the state information to a new col
                                      TRUE ~ NA_character_)), # anything not in us_region is labelled with NA
           region = ifelse(region %in% names(us_abb_to_region), us_abb_to_region[region], region)) # converts region to state abbreviation based on presence of state name
  
  # calculating state responses
  if (region == "state") { 
    
    us_region_freqs <- get_region_counts(us_regions) %>% # calculates percent of each region
      select(-question_no) %>% # selects only the region and count cols
      right_join(select(us_states_territories, name), by = c("region" = "name")) # joining with list of states/territories to add any missing
    
    return(us_region_freqs)
    
  # calculating world responses
  } else if (region == "world") { 
    
    # creating a list of all countries with data available for plotting
    countries <- map_data("world") %>% # making list of countries with available mapping information
      pull(region) %>% # pulls out list of countries
      unique(.) # makes list of unique countries
    
    # calculating number of respondents with degrees from USA
    n_usa <- us_regions %>% # using df from earlier in function
      filter(!is.na(region)) %>% # removing any NA responses
      nrow(.) # counting rows
    
    # extracting region information from responses
    world_regions <- survey_df %>%
      filter(question_no == "Q11" & !is.na(response)) %>% # filters down to country info
      add_row(question_no = "Q11", response = rep("USA", times = n_usa)) %>% # adds rows for respondents that went to school in USA (not counted otherwise)
      mutate(response = str_replace_all(response, "_", " "), # replaces all "_" with spaces to allow text parsing
             region = tolower(case_when(str_detect(response, regex(paste(paste0("\\b", countries, "\\b"), collapse = "|"), ignore_case = T)) ~ # detecting anything in any case that matches country names with word boundaries on both ends
                                          str_extract(response, regex(paste(paste0("\\b", countries, "\\b"), collapse = "|"), ignore_case = T)),  # extracting the country information to a new col
                                        TRUE ~ NA_character_))) # any country not in the list of available countries is labelled with NA
    
    # calculating the frequency of responses for individual regions
    world_region_freqs <- get_region_counts(world_regions) %>% # calculating world response rates by country
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
  
  # plotting points are contained within the fifty_states df of the fifty_stater pkg
  # using as alternative to map_data("state") because fifty_states includes hawaii and alaska as an inset
  
  # plotting function for us state data
  plot <- df %>% 
    ggplot(aes(map_id = region)) + # plotting by region using coordinate df and response freq
    geom_map(aes(fill = percent_freq), map = fifty_states, color = "black", size = 0.2) + # plots the map using fifty_states for coordinates
    fifty_states_inset_boxes() + # package function to add boxes around insets for AK and HI
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    scale_fill_continuous(type = "viridis", na.value = "white") + # making NA values = white and scaling using viridis palette
    labs(title = "Locations of U.S. Ph.D. Granting Institutions for University of Michigan Postdocs",
         fill = "Percent of Respondents") +
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
