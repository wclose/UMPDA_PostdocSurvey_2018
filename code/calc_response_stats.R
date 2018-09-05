# loading dependencies ----------------------------------------------------

# loading tidyverse and dataset
source("code/stratify_data.R")



# creating question lists -------------------------------------------------

# creating list of all the questions
question_list <- unique(tidy_survey_data$question_no)

# making list of questions to exclude from graph generation because they were typed response
# to change the questions included in this list, add/subtract/change the numbers in paste0() as desired
typed_question_list <- question_list[grepl(paste(paste0("Q", c(10,11,23,29,42,43,44,45,46,51)), collapse = "|"), question_list)]

# list of the remaining questions to be used for graph generation
multi_choice_question_list <- setdiff(question_list, typed_question_list)



# multiple choice statistics ----------------------------------------------

# function for generating individual response frequencies
get_response_freq <- function(question_no_chr) { # question_no_chr = question number in character format (aka need quotes)
  freq <- tidy_survey_data %>% # assigning the output to a variable
    filter(question_no == question_no_chr) %>% # selects rows containing responses for specific questions
    group_by(question_no, subquestion_no, question, response) %>% # groups by question and response to provide summary stats
    summarize(n = n()) %>% # creates col for number of a given response (n)
    mutate(percent_freq = n/sum(n)*100) # creates col for percent of total responses attributed to a given response
  return(freq) # returns the calculated/modified data frame
}

# # test to make sure function works
# get_response_freq("Q37")

# use map() to generate a named list containing response freq data frames for muliple choice questions
response_freq <- map(multi_choice_question_list, get_response_freq) %>% # running get_response_freq for each item in question list
  set_names(multi_choice_question_list) # naming the items in the list by question number

# # testing indexing ability of map() output
# response_freq[[1]]
# response_freq$Q12



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