# loading dependencies ----------------------------------------------------

# loading tidyverse and dataset
source("code/calc_response_stats.R")

# built in data sets don't include us territories so made new set
# reading in list of us states/territories with abbreviations
us_states_territories <- read_csv("data/us_state_territories.csv")

# converting df to be all lower case
us_states_territories <- us_states_territories %>%
  mutate_all(tolower)


# misc functions ----------------------------------------------------------

# creating a function to wrap the text of long titles by setting the max number of chrs per line (width = ##)
# ex: wrapper("Super long title", width = 50)
text_wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}



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

# making function to generate counts of locations/regions
get_location_counts <- function(df) {
  df %>% 
    group_by(question_no, subquestion_no, question, location) %>% # groups by question and response to provide summary stats
    summarize(n = n()) %>% # creates col for number of a given response (n)
    mutate(percent_freq = n/sum(n)*100) %>% 
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

# need mapproj package to display proper dimensions
library(mapproj)
library(fiftystater)
library(viridis)

# making function for mapping locations within the us
plot_us_degree_locations <- function(df) {
  states_map <- map_data("state")
  plot <- df %>% 
    ggplot(aes(map_id = location)) +
    geom_map(aes(fill = percent_freq), map = states_map, color = "black", size = 0.2) + # plots the map
    expand_limits(x = states_map$long, y = states_map$lat) +
    scale_fill_continuous(type = "viridis", na.value = "white") + # making NA values = white and scaling using viridis palette
    labs(title = "Locations of Ph.D. Granting Institutions for University of Michigan Postdocs",
         fill = "Percent of Respondents") +
    coord_map() + # gives map proper dimensions
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"))
  return(plot)
}

# plotting the data
plot_us_degree_locations(us_location_counts)




########## END ##########



########## generating map of locations outside the US ##########

test_int <- tidy_survey_data %>% 
  filter(question_no == "Q11" & !is.na(response))





########## END ##########



# multiple choice plots ---------------------------------------------------

# NOTE: Q22 responses need to be broken up and retabulated before graphing



########## generating plots for entire dataset ##########

# creating a plotting function that makes new graph for each question for entire dataset
make_response_plot <- function(df, question_no_chr) {
  response_plot <- df %>% 
    filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer") %>% # removing ambiguous answers from plots
    ggplot(aes(x = question_no, y = percent_freq, fill = response)) + # plotting the stratified categories by response
    geom_bar(stat = "identity", show.legend = F, color = "black") + # bar plot
    geom_text(aes(x = question_no, y = percent_freq, label = paste0(format(round(percent_freq, digits = 1), nsmall = 1), "")), # adding response freq over bars
              hjust = -0.25, size = 2.82) + # size is given in mm so need to convert to pts = 1/72*25.4*desired_pt_size
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # formatting y axis
    facet_grid(str_replace_all(question, "_", " ") ~ str_replace_all(response, c("_" = " ", "," = ", ")), # plots each question/group of subquestions
               switch = "y", # moves y axis strip to opposite side of plot
               labeller = label_wrap_gen(width = 50, multi_line = TRUE)) + # allows text wrapping in strip labels
    labs(x = "", # removing x label since the facet labels are the new x labels
         y = "Proportion of postdoctoral respondents (%)") +
    coord_flip(clip = "off") + # rotating the plots and allowing plotting outside of plot area
    # NOTE: x and y commands are now swapped due to coord_flip rotating the axes
    theme(axis.line = element_line(size = 0.5, colour = "black"), # formatting axis lines as desired
          axis.title = element_text(size = 11), # making all chart titles a consistent size
          axis.title.x = element_text(margin = margin(10,0,0,0)), # adding space between x axis title and axis labels
          axis.text.y = element_blank(), # removing y axis text since there's only one group
          axis.ticks.y = element_blank(), # removing y axis tick marks since there's only one group
          plot.margin = margin(20,30,20,20), # giving plot a bit of padding on edges in case something is plotted out of bounds
          # formatting elements of the facets/strips (facet labels)
          panel.background = element_rect(fill = "white"), # making panels have white background
          panel.spacing = unit(1, "lines"), # increasing spacing between panels
          panel.spacing.x = unit(2.5, "lines"), # adding a bit more horizontal space between panels
          strip.text = element_text(size = 11), # setting strip labels to same size as other plot labels
          strip.text.y = element_text(angle = 180, margin = margin(0,20,0,-10)), # formatting and positioning new y labels
          strip.background.x = element_rect(fill = "white", color = "black"), # formatting strip col labels
          strip.background.y = element_rect(fill = "white", color = NA), # removing border from strip row labels (new y labels)
          strip.placement = "outside", # moving strip row labels outside y axis labels to make them the new y labels
          # formatting plots to have a consistent size
          aspect.ratio = 0.075) # making size of bars compared to plot consistent
  return(response_plot)
}

# iterating through the list of question numbers over the df response_freq which contains all the data
response_plots <- map(.x = multi_choice_question_list, .f = make_response_plot, df = response_freq) %>% 
  set_names(multi_choice_question_list)

# # testing the output
# response_plots$Q12



########## END ##########



########## generating plots based on stratifications ##########

# creating a plotting function that makes new graph for each question based on strat_id
make_strat_response_plot <- function(df, question_no_chr) {
  response_plot <- df %>% 
    filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer") %>% # removing ambiguous answers from plots
    ggplot(aes(x = strat_id, y = percent_freq, fill = response)) + # plotting the stratified categories by response
    geom_bar(stat = "identity", show.legend = F, color = "black") + # bar plot
    geom_text(aes(x = strat_id, y = percent_freq, label = paste0(format(round(percent_freq, digits = 1), nsmall = 1), "")), # adding response freq over bars
              hjust = -0.25, size = 2.82) + # size is given in mm so need to convert to pts = 1/72*25.4*desired_pt_size
    scale_x_discrete(labels = c(str_replace_all(unique(df$strat_id), "_", " "))) + # reformatting axis labels to look nice
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # formatting y axis
    facet_grid(str_replace_all(question, "_", " ") ~ str_replace_all(response, c("_" = " ", "," = ", ")), # plots each question/group of subquestions
               switch = "y", # moves y axis strip to opposite side of plot
               labeller = label_wrap_gen(width = 50, multi_line = TRUE)) + # allows text wrapping in strip labels
    labs(x = "", # removing x label since the facet labels are the new x labels
         y = "Proportion of postdoctoral respondents (%)") +
    coord_flip(clip = "off") + # rotating the plots and allowing plotting outside of plot area
    # NOTE: x and y commands are now swapped due to coord_flip rotating the axes
    theme(axis.line = element_line(size = 0.5, colour = "black"), # formatting axis lines as desired
          axis.title = element_text(size = 11), # making all chart titles a consistent size
          axis.title.x = element_text(margin = margin(10,0,0,0)), # adding space between x axis title and axis labels
          plot.margin = margin(20,30,20,20), # giving plot a bit of padding on edges in case something is plotted out of bounds
          # formatting elements of the facets/strips (facet labels)
          panel.background = element_rect(fill = "white"), # making panels have white background
          panel.spacing = unit(1, "lines"), # increasing spacing between panels
          panel.spacing.x = unit(2.5, "lines"), # adding a bit more horizontal space between panels
          strip.text = element_text(size = 11), # setting strip labels to same size as other plot labels
          strip.text.y = element_text(angle = 180, margin = margin(0,20,0,-10)), # formatting and positioning new y labels
          strip.background.x = element_rect(fill = "white", color = "black"), # formatting strip col labels
          strip.background.y = element_rect(fill = "white", color = NA), # removing border from strip row labels (new y labels)
          strip.placement = "outside", # moving strip row labels outside y axis labels to make them the new y labels
          # formatting plots to have a consistent size
          aspect.ratio = (0.075*length(unique(df$strat_id)))) # making size of bars compared to plot consistent
  return(response_plot)
}

# # testing make_strat_response_plot
# make_strat_response_plot(strat_response_freq$language, "Q16")
# make_strat_response_plot(strat_response_freq$language, "Q35")
# make_strat_response_plot(strat_response_freq$college_school, "Q35")

# making function to cycle through each question for each data frame
make_all_strat_response_plots <- function(strat_response_freq_df, question_no_chr_list) {
  arguments <- data_frame(df = list(strat_response_freq_df), # 1st col = input df repeated to be same length as number of questions
                          question_no_chr = c(question_no_chr_list)) # 2nd col = list of question numbers in chr format
  data <- pmap(arguments, make_strat_response_plot) %>% # collating data during map and assigning output
    set_names(question_no_chr_list) # naming the output for easier indexing
  return(data)
}

# # testing make_all_response_plots()
# test_output <- make_all_response_plots(strat_response_freq$language, multi_choice_question_list)
# test_output[[1]]
# test_output$Q27

# making plots for all of the stratified data sets
# NOTE: question_no_chr_list is passed on to make_all_strat_response_plots()
strat_response_plots <- map(.x = strat_response_freq, .f = make_all_strat_response_plots, question_no_chr_list = multi_choice_question_list)

# # verifying the plots
# strat_response_plots$residency$Q35
# strat_response_plots$college_school[[1]]
# strat_response_plots$residency$Q16

########## END ##########



# typed plots -------------------------------------------------------------






# look into ideas to plot responses to typed questions








# notes -------------------------------------------------------------------

########## generating map of locations within the US ##########

# string_test <- test_us$response
# test_names <- c("MI", "Michigan")
# 
# test_string <- c("this_is,my_test string.ok", "this")
# 
# # can use "\\b" as regex to recognize boundaries of words
# str_replace_all(test_string, "\\b", "-")
# str_extract(string_test, "\\bMaryland\\b")
# 
# # by changing "_" back to " ", it creates word boundaries that can be used for parsing the data
# test <- test_us %>%
#   mutate(new = str_replace_all(response, "_", " "),
#          new2 = case_when(str_detect(new, "\\bMaryland\\b") ~ str_extract(new, "\\bMaryland\\b"),
#                           str_detect(new, "\\bMI\\b") ~ str_extract(new, "\\bMI\\b")))

# created function to search for states in a specific df
# need to supply it with a list of state abbreviations and names
# NOTE: need to convert all names to one type (abbreviation or spelled out)
# test_function <- function(df, state) {
#   data <- df %>% 
#     mutate(response = str_replace_all(response, "_", " "),
#            location = case_when(str_detect(response, regex(paste0("\\b", state, "\\b"), ignore_case = T)) ~ # detecting anything in any case that matches string with word boundaries on both ends
#                                   str_extract(response, regex(paste0("\\b", state, "\\b"), ignore_case = T)))) # extracting the state information to a new col
#   return(data)
# }
# 
# test_function(test_us, us_states)


# # testing out test_function()
# test_function(test_us, "mi") %>% 
#   group_by(location) %>% 
#   summarize(count = n())

# map(.x = us_states, .f = test_function, df = test_us)

# # creating list of states and abbreviations for testing
# us_states <- c(state.name, state.abb)
# 
# # instead of creating a named list, a named vector works better for indexing within a function
# test_names <- tolower(state.abb) %>% 
#   set_names(tolower(state.name))
# 
# # testing the indexing functionality
# test_names["alaska"]
# 
# # seeing if it works in the dataframe
# test %>%
#   mutate(location = ifelse(location %in% names(test_names), test_names[location], location))

# # checking to make sure pasting the list of states and abbreviations works properly
# paste0("\\b", us_states, "\\b")
# paste(paste0("\\b", us_states, "\\b"), collapse = "|")

# testing the output
#test <- test_function2(test_us)

# # a bunch of for loops that kind of worked but not quite what I wanted
# for (i in unique(test$location)) {
#   if (i %in% tolower(state.name)) {
#     print(i)
#   }
# }
# 
# 
# change_name_to_abb <- function(df) {
#   for (i in df$location) {
#     if (i %in% names(test_names)) {
#       return(test_names[[i]])
#     }
#   }
# }
# 
# 
# if (test$location %in% names(test_names)) {
#   print(test_names)
# }

# ifelse(test$location %in% names(test_names), test_names[[as.character(test$location)]], NA_character_)

# test_names$alabama
# test_names[["alabama"]]
# as.character(test$location)
# 
# 
# change_name_to_abb(test)
# test %>% 
#   mutate(loc_test = change_name_to_abb(.))
# 
# test2 <- for (i in test$location) {
#   if (i %in% names(test_names)) {
#     return(test_names[[i]])
#   }
# }
# 
# for (i in test$location) {
#   if (i %in% names(test_names)) {
#     print(i)
#   }
# }

# # creating test df of only domestic locations
# test_us <- tidy_survey_data %>%
#   filter(question_no == "Q10" & !is.na(response))

# # creating function to extract the us state from the responses and convert all to abbreviations for uniformity and plotting
# extract_us_state <- function(df) {
#   us_state <- c(state.abb, state.name) # creating vector of states and abbreviations
#   us_state_name_to_abb <- tolower(state.abb) %>% # creates a named vector to enable extraction based on full name
#     set_names(tolower(state.name))
#   data <- df %>%
#     mutate(response = str_replace_all(response, "_", " "),
#            location = tolower(case_when(str_detect(response, regex(paste(paste0("\\b", us_state, "\\b"), collapse = "|"), ignore_case = T)) ~ # detecting anything in any case that matches string with word boundaries on both ends
#                                   str_extract(response, regex(paste(paste0("\\b", us_state, "\\b"), collapse = "|"), ignore_case = T)),  # extracting the state information to a new col
#                                 TRUE ~ NA_character_)), 
#            location = ifelse(location %in% names(us_state_name_to_abb), us_state_name_to_abb[location], location)) # converts location to state abbreviation based on presence of state name
#   return(data)
# }

# # testing extract_us_state()
# extract_us_state(test_us)

########## END ##########



########## generating map of locations outside the US ##########



########## END ##########


########## generating plots for entire dataset ##########

# # making function to generate plots for multi choice questions
# make_response_plot <- function(df) { # df = properly formatted data frame with data
#   freq_plot <- df %>% # assigning the output
#     ggplot() +
#     geom_bar(aes(x = response, y = percent_freq, fill = response, group = subquestion_no), # adding bars
#              color = "black", show.legend = F, stat = "identity", width = 0.75) +
#     geom_text(aes(x = response, y = percent_freq, label = paste0(round(percent_freq, 1), "%")), vjust = -0.5) + # adding response freq over bars
#     scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # formatting y axis
#     scale_x_discrete(labels = c(str_replace_all(df$response, "_", " "))) + # reformatting axis labels to look nice
#     labs(title = text_wrapper(str_replace_all(unique(df$question), "_", " "), width = 60), # reformatting remainging labels to look nice
#          y = "Proportion of Postdoctoral Respondents (%)",
#          x = "Responses") +
#     theme_classic() + # changing color scheme, etc.
#     theme(plot.title = element_text(hjust = 0.5)) # centering the title over the plot
#   return(freq_plot) # returning the plot to environment
# }
# 
# # testing response plotting function
# make_response_plot(response_freq$Q6)
# 
# # using map() to generate plots for each data frame contained within response_freq list
# response_plots <- map(response_freq, make_response_plot) # don't need to specify names b/c preserves names from response_freq
# 
# # verifying indexing ability and plot appearance
# response_plots$Q12



# # making function to generate plots for multi choice questions
# make_stack_response_plot <- function(df) { # df = properly formatted data frame with data
#   freq_plot <- df %>% # assigning the output
#     ggplot() +
#     geom_bar(aes(x = question_no, y = percent_freq, fill = response), # adding bars
#              color = "black", show.legend = T, stat = "identity", width = 0.75, position = "stack") +
#     #geom_text(aes(x = response, y = percent_freq, label = paste0(round(percent_freq, 1), "%")), vjust = -0.5) + # adding response freq over bars
#     scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # formatting y axis
#     #scale_x_discrete(labels = c(str_replace_all(df$question, "_", " "))) + # reformatting axis labels to look nice
#     labs(#title = text_wrapper(str_replace_all(unique(df$question), "_", " "), width = 60), # reformatting remainging labels to look nice
#          y = "Proportion of Postdoctoral Respondents (%)",
#          x = "Responses") +
#     #coord_flip() +
#     theme_classic() + # changing color scheme, etc.
#     theme(plot.title = element_text(hjust = 0.5)) + # centering the title over the plot
#     facet_wrap( ~ question)
#   return(freq_plot) # returning the plot to environment
# }

# make_stack_response_plot(response_freq$Q35)
# 
# # toy data set
# test <- strat_response_freq$language %>% 
#   filter(question_no == "Q35" & !is.na(response) & response != "Prefer_not_to_answer")
# 
# test_2 <- strat_response_freq$language %>% 
#   filter(question_no == "Q14" & !is.na(response) & response != "Prefer_not_to_answer")
# 
# # basic layout for the plots
# test %>% 
#   ggplot(aes(x = strat_id, y = percent_freq, fill = response)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~ subquestion_no)

