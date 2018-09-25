# loading dependencies ----------------------------------------------------

# loading tidyverse and dataset
source("code/tidy_survey.R")



# classifying data based on desired stratifications -----------------------


############ is there a cleaner/easier way to classify and design the functions?


### stratification categories ###
# 1 UMMS, LSA, Eng, other
# 2 Domestic v. International and/or EFL v. EAL
# 3 Dependents v. no dependents
# 4 Partner v. no partner
# 5 Male v. female (only 1 non-binary responder)
# 6 Well v. Under-represented
# 7 Academic v. non-academic career track
# 8 First v. multiple PD position
# 9 Satisfied v. not satisfied

classified_survey_data <- tidy_survey_data %>% 
  mutate(stratifications = case_when(question_no == "Q6" & response == "Medicine" ~ "UMMS", # school of medicine
                                     question_no == "Q6" & response == "Literature,Science,and_the_Arts" ~ "LSA", # lit, science, and arts
                                     question_no == "Q6" & response == "Engineering" ~ "ENG", # engineering
                                     question_no == "Q6" & !is.na(response) & response != "Prefer_not_to_answer" ~ "Other", # classifying other schools as "Other"
                                     question_no == "Q10" & !is.na(response) ~ "Domestic", # anything other than NA = citizen
                                     question_no == "Q11" & !is.na(response) ~ "International", # anything other NA = non-citizen
                                     question_no == "Q12" & response == "Yes" ~ "EFL",
                                     question_no == "Q12" & !is.na(response) & response != "Prefer_not_to_answer" ~ "EAL",
                                     question_no == "Q17" & response == "Yes" ~ "Dependents",
                                     question_no == "Q17" & response == "No" ~ "No_dependents",
                                     question_no == "Q16" & response == "Single,never_married" ~ "Single_(unmarried)",
                                     question_no == "Q16" & response == "Married_or_in_domestic_partnership" ~ "Married/domestic_partnership",
                                     question_no == "Q16" & response == "Widowed" ~ "Widowed",
                                     question_no == "Q16" & response == "Divorced_or_separated" ~ "Divorced/separated",
                                     question_no == "Q14" & response == "Female" ~ "Female",
                                     question_no == "Q14" & response == "Male" ~ "Male",
                                     question_no == "Q14" & response == "Non-binary" ~ "Non-binary",
                                     question_no == "Q15" & response == "Yes" ~ "Under-represented",
                                     question_no == "Q15" & response == "No" ~ "Well-represented",
                                     question_no == "Q31" & response == "Academic_-_research" ~ "Academic_(research)",
                                     question_no == "Q31" & response == "Academic_-_teaching" ~ "Academic_(teaching)",
                                     question_no == "Q31" & response == "Non-academic" ~ "Non-academic",
                                     question_no == "Q31" & response == "Unsure" ~ "Unsure",
                                     question_no == "Q8" & response == "Yes" ~ "First_postdoc",
                                     question_no == "Q8" & response == "No" ~ ">1_postdoc",
                                     question_no == "Q39" & response == "Agree" ~ "Satisfied",
                                     question_no == "Q39" & response == "Disagree" ~ "Unsatisfied",
                                     question_no == "Q39" & response == "Neutral" ~ "Neutral",
                                     TRUE ~ NA_character_))



# breaking up data by stratification categories ---------------------------

# creating the function to separate the data into different data frames based on stratification categories
make_stratified_data <- function(strat_col_values) {
  resp_id_list <- classified_survey_data %>% 
    filter(stratifications == strat_col_values) %>%
    pull(response_id) # extracts all values from col as text string
  stratified_data <- tidy_survey_data %>% 
    filter(response_id %in% resp_id_list) %>%  # extracts responses to all questions for each response_id in id_list
    mutate(strat_id = strat_col_values)
  return(stratified_data)
}

# creating list of names for naming list of comparison classifications
strat_list_names <- c("college_school", "postdoc_no", "residency", "language", "gender", "pop_representation",
                "relationship_status", "dependents", "career_track", "satisfaction")

# creating a named list of all of the desired breakdowns for comparisons then naming the output list
# the output will maintain the names given to the items in the input list which is why naming at this step is so important
strat_list <- list(c("UMMS", "ENG", "LSA", "Other"), 
                   c("First_postdoc", ">1_postdoc"),
                   c("Domestic", "International"),
                   c("EFL", "EAL"),
                   c("Male", "Female", "Non-binary"),
                   c("Under-represented", "Well-represented"),
                   c("Single_(unmarried)", "Married/domestic_partnership", "Widowed", "Divorced/separated"),
                   c("Dependents", "No_dependents"),
                   c("Academic_(research)", "Academic_(teaching)", "Non-academic", "Unsure"),
                   c("Satisfied", "Unsatisfied", "Neutral")) %>% 
  set_names(strat_list_names)

# setting up a function to allow use of map to iterate through each set of stratification classifications
get_strat_data <- function(x) {
  data <- map_df(x, make_stratified_data) # map_df iterates through each item in input list then collates output as a single dataframe
  return(data) 
}

# using map to run the functions using the different classifications of data
strat_data <- map(strat_list, get_strat_data) # running a nested map series takes collated data and outputs as one dataframe in the list per stratification category

names(strat_data) # checking to make sure the names were propagated through


# notes -------------------------------------------------------------------

# create fn: 
# id_list <- for _ in unique(!is.na(college)), filter( == paste(i)), pull(response_id)
# paste(dfname_i) <- tidy_survey_data %>% filter response_id %in% id_list
# return(paste(dfname_i))
# map to create named list of new dataframes
# map to do stats
# map to make plots
# integrate into Rmd

# list <- postdoc_college %>% 
#   filter(college == "UMMS") %>% 
#   pull(response_id)
# 
# tidy_survey_data %>% 
#   filter(response_id %in% list)
# 
# paste(unique((postdoc_college$college)), "test", sep = "_")
# 
# college_list <- c("UMMS", "ENG", "LSA", "Other")
# 
# test_function <- function(colleges) {
#   id_list <- postdoc_college %>% 
#     filter(college == colleges) %>% 
#     pull(response_id)
#   relevant_info <- tidy_survey_data %>% 
#     filter(response_id %in% unique(id_list))
#   return(relevant_info)
# }
# 
# test_function("Other")
# 
# map(college_list, test_function)

# # function to extract stratified data based on individual cols for each classification type
# test_function <- function(strat_col, strat_col_values) {
#   id_list <- big_data %>% 
#     filter((!!as.symbol(strat_col)) == strat_col_values) %>% # converts identification col name from chr (input) to var name (symbol) without evaluating, need () around !! expression to work
#     pull(response_id) # extracts all values from col as text string
#   relevant_info <- tidy_survey_data %>% 
#     filter(response_id %in% id_list) # extracts responses to all questions for each response_id in id_list
#   return(relevant_info)
# }

# # simple test of function
# test_function("college", "Other")

# # need to use pmap to iteratively break up data based on classifications and test_function
# # creating df for testing pmap (1st col = classification col, 2nd col = classification label)
# df <- data_frame(
#   strat_col = c(rep("college", length(unique(na.omit(big_data$college)))), rep("residency", length(unique(na.omit(big_data$residency))))),
#   strat_col_values = c(unique(na.omit(big_data$college)), unique(na.omit(big_data$residency)))
# )

# # testing the function
# stratification_categories <- gender #unique(na.omit(classified_survey_data$stratifications))
# 
# # using map to test and label all of the 
# stratified_data <- map_df(stratification_categories, make_stratified_data)

# list(college_school <- c("umms", "eng", "lsa", "other"), 
#      postdoc_no <- c("first_postdoc", "not_first_postdoc"),
#      residency <- c("domestic", "international"),
#      language <- c("english_first", "english_acquired"),
#      gender <- c("male", "female", "non_binary"),
#      pop_representation <- c("under_represented", "well_represented"),
#      relationship_status <- c("single_unmarried", "married_partnership", "widowed", "divorced_separated"),
#      dependents <- c("dependents", "no_dependents"),
#      career_track <- c("academic_research", "academic_teaching", "non_academic", "career_unsure"),
#      satisfaction <- c("satisfied", "unsatisfied", "neutral_satisfaction"))

# get_response_freq2 <- function(question_no_chr) { # question_no_chr = question number in character format (aka need quotes)
#   freq <- stratified_data$domestic %>% # assigning the output to a variable
#     filter(question_no == question_no_chr) %>% # selects rows containing responses for specific questions
#     group_by(question_no, subquestion_no, question, response) %>% # groups by question and response to provide summary stats
#     summarize(n = n()) %>% # creates col for number of a given response (n)
#     mutate(percent_freq = n/sum(n)*100) # creates col for percent of total responses attributed to a given response
#   return(freq) # returns the calculated/modified data frame
# }
# 
# get_response_freq2(stratified_data$domestic, "Q37")
# 
# map(multi_choice_question_list, get_response_freq2) %>% 
#   set_names(paste(names(stratified_data)[[1]], multi_choice_question_list, sep = "_"))
# 
# 
# 
# names(stratified_data)[[1]]
# 
# pmap(list(df = stratified_data, question_no_chr = multi_choice_question_list), get_response_freq2)