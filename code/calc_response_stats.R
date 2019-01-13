# loading dependencies ----------------------------------------------------

# loads upstream scripts/variables if not already loaded
# checks for variables generated at end of previous script in pipeline and sources if not found
if (!exists("strat_data")){
  source("code/stratify_data.R")
}



# multiple choice statistics ----------------------------------------------

########## generating responses for entire dataset ##########

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

# use map() to generate a named list containing response freq data frames for muliple choice questions
response_freq <- map_df(multi_choice_question_list, get_response_freq) # running get_response_freq for each item in question list



########## END ##########



########## generating responses based on stratifications ##########

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

# checking the function with the strat_id classified data
strat_response_freq <- map(strat_data, aggregate_strat_response_freq)



########## END ########## 



# typed statistics --------------------------------------------------------







# use this section to generate tables for plotting written responses













# notes -------------------------------------------------------------------

# # playing around with mapping function as possible alternative
# test <- question_data %>% # assigning the output to a variable
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
# aggregate_strat_response_freq(question_data)
