
# built in data sets don't include us territories so made new set
# reading in list of us states/territories with abbreviations
us_states_territories <- read_csv("data/us_state_territories.csv")

# converting df to be all lower case
us_states_territories <- us_states_territories %>%
  mutate_all(tolower)



# maps of degree locations ------------------------------------------------

# NOTE: need to adjust question to apply to Ph.D.s, M.D.s, and J.D.s (do J.D.s count as postdocs?)
# NOTE: some US citizens completed degrees outside the US, need to adjust question if/else flow for citizenship question



########## generating map of locations within the US ##########

# creating function to extract the us state from the responses and convert all abbreviations to full names for uniformity and plotting
# added in step to filter data to only relevant question about locations within us
extract_us_region <- function(df) {
  us_region <- c(us_states_territories$abb, us_states_territories$name) # creating vector of states and abbreviations
  us_abb_to_region <- us_states_territories$name %>% # creates a named vector to enable extraction based on full name
    set_names(us_states_territories$abb)
  data <- df %>%
    filter(question_no == "Q10" & !is.na(response)) %>% 
    mutate(response = str_replace_all(response, "_", " "),
           location = tolower(case_when(str_detect(response, regex(paste(paste0("\\b", us_region, "\\b"), collapse = "|"), ignore_case = T)) ~ # detecting anything in any case that matches string with word boundaries on both ends
                                          str_extract(response, regex(paste(paste0("\\b", us_region, "\\b"), collapse = "|"), ignore_case = T)),  # extracting the state information to a new col
                                        TRUE ~ NA_character_)), 
           location = ifelse(location %in% names(us_abb_to_region), us_abb_to_region[location], location)) # converts location to state abbreviation based on presence of state name
  return(data)
}

# extracting us region data and adding as a new col creating state_data df
us_data <- extract_us_region(tidy_survey_data)

# # getting counts for each state/territory
# us_data %>% 
#   group_by(question_no, subquestion_no, question, location) %>% # groups by question and response to provide summary stats
#   summarize(n = n()) # creates col for number of a given response (n)

# counting the number of people that responded to the location questions for use in calculating percents later
location_number <- tidy_survey_data %>% 
  filter(question_no == "Q10" & !is.na(response) | question_no == "Q11" & !is.na(response)) %>% 
  select(response_id) %>%
  unique(.) %>%
  nrow()

# making function to generate counts of locations/regions
get_location_counts <- function(df) {
  df %>% 
    group_by(question_no, subquestion_no, question, location) %>% # groups by question and response to provide summary stats
    summarize(n = n()) %>% # creates col for number of a given response (n)
    mutate(percent_freq = n/location_number*100) %>% 
    ungroup()
}

# # testing get_location_counts
# get_location_counts(us_data)


##### turn this into a function ####
# saving count data for us states and filling in missing states
us_location_counts <- get_location_counts(us_data) %>% 
  select(location, percent_freq) %>% # selects only the location and count cols
  right_join(select(us_states_territories, name), by = c("location" = "name")) #%>% # joining with list of states/territories to add any missing
# mutate(n = ifelse(is.na(n), 0, n)) # making any missing counts for a state/territory = 0


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

# making function for mapping locations within the us
plot_us_degree_locations <- function(df) {
  states_map <- map_data("state")
  plot <- df %>% 
    ggplot(aes(map_id = location)) +
    geom_map(aes(fill = percent_freq), map = fifty_states, color = "black", size = 0.2) + # plots the map
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    scale_fill_continuous(type = "viridis", na.value = "white") + # making NA values = white and scaling using viridis palette
    labs(title = "Locations of U.S. Ph.D. Granting Institutions for University of Michigan Postdocs",
         fill = "Percent of Respondents") +
    coord_map() + # gives map proper dimensions
    fifty_states_inset_boxes() + # package function to add boxes around insets for AK and HI
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"))
  return(plot)
}

# plotting the data
us_degree_locations <- plot_us_degree_locations(us_location_counts)


ggsave(filename = "results/us_degree_locations.png", plot = us_degree_locations, width = 20, dpi = 300)


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

test_int <- tidy_survey_data %>% 
  filter(question_no == "Q11" & !is.na(response))





########## END ##########

detach("package:mapproj", unload = TRUE)
detach("package:maps", unload = TRUE)

