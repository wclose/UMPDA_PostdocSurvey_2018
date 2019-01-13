# loading dependencies ----------------------------------------------------

# loads upstream scripts/variables if not already loaded
# checks for variables generated at end of previous script in pipeline and sources if not found
if (!exists("strat_data")){
  source("code/stratify_data.R")
}



# creating functions for entire dataset -----------------------------------

# function for generating individual response frequencies
get_response_freq <- function(question_no_chr) { # question_no_chr = question number in character format (aka need quotes)
  freq <- question_data %>% # assigning the output to a variable
    filter(question_no == question_no_chr) %>% # selects rows containing responses for specific questions
    group_by(question_no, sorted_question_no, subquestion_no, question, response) %>% # groups by question and response to provide summary stats
    summarize(n = n()) %>% # creates col for number of a given response (n)
    mutate(percent_freq = n/sum(n)*100) %>% # creates col for percent of total responses attributed to a given response
    ungroup()
  return(freq) # returns the calculated/modified data frame
}



# creating functions for stratified dataset -------------------------------

# altering the function from before to incorporate group_by strat_id which is assigned from stratify_data.R
get_strat_response_freq <- function(df, question_no_chr) { # question_no_chr = question number in character format (aka need quotes)
  freq <- df %>% # assigning the output to a variable
    filter(question_no == question_no_chr) %>% # selects rows containing responses for specific questions
    group_by(strat_id, question_no, sorted_question_no, subquestion_no, question, response) %>% # groups by question and response to provide summary stats
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



# generating responses for entire dataset ---------------------------------

# calculating response frequencies for the unstratified dataset
# creates a single dataframe
response_freq <- map_df(multi_choice_question_list, get_response_freq) # running get_response_freq for each item in question list



# generating responses based on stratifications ---------------------------

# calculating response frequencies for each stratified dataset
# creates a list of dataframes named by stratification category
strat_response_freq <- map(strat_data, aggregate_strat_response_freq)



########## END ########## 



# notes -------------------------------------------------------------------


