# loading dependencies ----------------------------------------------------

# loading tidyverse and dataset
source("code/stratify_data.R")



# functions ---------------------------------------------------------------

# regional maps -----------------------------------------------------------

# master function for generating response frequencies
# region should be set to either "state" for USA specific state mapping or "world" for country mapping
# df is the dataframe containing the survey data
calc_degree_freq <- function(region, survey_df) {
  
  # creating vector of states and abbreviations
  us_region <- c(us_states_territories$abb, us_states_territories$name) 
  
  # creates a named vector to enable extraction based on full name
  us_abb_to_region <- us_states_territories$name %>% 
    set_names(us_states_territories$abb)
  
  # creating df of us regions (states)
  us_regions <- survey_df %>%
    filter(question_no == "Q10" & !is.na(response)) %>% # filters out non-us data
    mutate(response = str_replace_all(response, "_", " "),
           region = tolower(case_when(str_detect(response, regex(paste(paste0("\\b", us_region, "\\b"), collapse = "|"), ignore_case = T)) ~ # detecting anything in any case that matches string with word boundaries on both ends
                                        str_extract(response, regex(paste(paste0("\\b", us_region, "\\b"), collapse = "|"), ignore_case = T)),  # extracting the state information to a new col
                                      TRUE ~ NA_character_)), 
           region = ifelse(region %in% names(us_abb_to_region), us_abb_to_region[region], region)) # converts region to state abbreviation based on presence of state name
  
  if (region == "state") {
    
    us_region_freqs <- get_region_counts(us_regions) %>%
      select(-question_no) %>% # selects only the region and count cols
      right_join(select(us_states_territories, name), by = c("region" = "name")) # joining with list of states/territories to add any missing
    
    return(us_region_freqs)
    
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
    world_region_freqs <- get_region_counts(world_regions) %>%
      select(-question_no) %>% # selects only the region and count cols
      right_join(select(as_data_frame(tolower(countries)), value), by = c("region" = "value")) # joining with list of countries to add missing ones
    
    # returning freq df
    return(world_region_freqs)
    
  } else {
    
    print("ERROR: Incorrect region assignment")
    
  }
}


# creating question lists -------------------------------------------------

# creating list of all the questions
question_list <- unique(tidy_survey_data$question_no)

# making list of questions to exclude from graph generation because they were typed response
# to change the questions included in this list, add/subtract/change the numbers in paste0() as desired
typed_question_list <- question_list[grepl(paste(paste0("Q", c(10,11,23,29,42,43,44,45,46,51)), collapse = "|"), question_list)]

# list of the remaining questions to be used for graph generation
multi_choice_question_list <- setdiff(question_list, typed_question_list)



# multiple choice statistics ----------------------------------------------

########## generating responses for entire dataset ##########

# function for generating individual response frequencies
get_response_freq <- function(question_no_chr) { # question_no_chr = question number in character format (aka need quotes)
  freq <- tidy_survey_data %>% # assigning the output to a variable
    filter(question_no == question_no_chr) %>% # selects rows containing responses for specific questions
    group_by(question_no, subquestion_no, question, response) %>% # groups by question and response to provide summary stats
    summarize(n = n()) %>% # creates col for number of a given response (n)
    mutate(percent_freq = n/sum(n)*100) %>% # creates col for percent of total responses attributed to a given response
    ungroup()
  return(freq) # returns the calculated/modified data frame
}

# use map() to generate a named list containing response freq data frames for muliple choice questions
response_freq <- map_df(multi_choice_question_list, get_response_freq) # running get_response_freq for each item in question list



########## END ##########



########## generating responses based on stratifications ##########

# altering the function from before to incorporate group_by strat_id which is assigned from stratify_data.R
get_strat_response_freq <- function(df, question_no_chr) { # question_no_chr = question number in character format (aka need quotes)
  freq <- df %>% # assigning the output to a variable
    filter(question_no == question_no_chr) %>% # selects rows containing responses for specific questions
    group_by(strat_id, question_no, subquestion_no, question, response) %>% # groups by question and response to provide summary stats
    summarize(n = n()) %>% # creates col for number of a given response (n)
    mutate(percent_freq = n/sum(n)*100) %>% # creates col for percent of total responses attributed to a given response
    ungroup()
  return(freq) # returns the calculated/modified data frame
}

# creating a function formatted for use with map
aggregate_strat_response_freq <- function(list_of_strat_df) {
  # creating a dataframe for use with pmap
  arguments <- data_frame(df = list(list_of_strat_df), # 1st col = input df repeated to be same length as number of questions
                   question_no_chr = c(multi_choice_question_list)) # 2nd col = question names for use in filtering
  data <- pmap_df(arguments, get_strat_response_freq) # collating data during map and assigning output
  return(data)
}

# checking the function with the strat_id classified data
strat_response_freq <- map(strat_data, aggregate_strat_response_freq)



########## END ########## 



# typed statistics --------------------------------------------------------







# use this section to generate tables for plotting written responses













# notes -------------------------------------------------------------------

# # playing around with mapping function as possible alternative
# test <- tidy_survey_data %>% # assigning the output to a variable
#   filter(question_no == "Q37") %>% # selects rows containing responses for specific questions
#   group_by(question_no, subquestion_no, response) %>% # groups by question and response to provide summary stats
#   nest() %>%
#   mutate(n = map_int(data, nrow),
#          percent_freq = n/sum(n)*100) %>% 
#   unnest()
# 
# test$data[[22]]
# 
#   summarize(n = n()) %>% # creates col for number of a given response (n)
#   mutate(percent_freq = n/sum(n)*100) 

# # test to make sure function works when run on entire dataset
# get_response_freq("Q37")

# # testing indexing ability of map() output
# response_freq[[1]]
# response_freq$Q12

# # example of flow for generating stratified response freqs and graphing (df and question_no would be fed in to function)
# strat_data$satisfaction %>% 
#   filter(question_no == "Q12") %>% 
#   group_by(strat_id, response) %>% 
#   summarize(n = n()) %>% 
#   mutate(percent_freq = n/sum(n)*100) %>% 
#   ggplot(aes(x = strat_id, y = percent_freq, fill = response)) +
#   geom_bar(stat = "identity")

# checking the output of the function (need to make sure group_by is in correct order to generated desired response freqs)
# check <- get_strat_response_freq(strat_data$satisfaction, "Q12")

# checking the dataframe via graphing
# check %>%
#   ggplot(aes(x = strat_id, y = percent_freq, fill = response)) +
#   geom_bar(stat = "identity")

# # doesn't work on original dataset because there isn't a strat_id col (need to use original form of get_response_freq for regular dataset)
# aggregate_strat_response_freq(tidy_survey_data)
