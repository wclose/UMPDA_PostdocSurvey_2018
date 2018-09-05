# loading dependencies ----------------------------------------------------

# loading tidyverse and dataset
source("code/tidy_survey.R")



# classifying data based on desired stratifications -----------------------

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
  mutate(stratification_type = case_when(question_no == "Q6" ~ "college_school",
                                         question_no == "Q10" | question_no == "Q11" ~ "residency",
                                         question_no == "Q12" ~ "first_language",
                                         question_no == "Q17" ~ "dependents",
                                         question_no == "Q16" ~ "relationship_status",
                                         question_no == "Q14" ~ "gender",
                                         question_no == "Q15" ~ "pop_representation",
                                         question_no == "Q31" ~ "career_track",
                                         question_no == "Q8" ~ "postdoc_no",
                                         question_no == "Q39" ~ "satisfaction",
                                         TRUE ~ NA_character_),
         stratifications = case_when(question_no == "Q6" & response == "Medicine" ~ "umms", # school of medicine
                                     question_no == "Q6" & response == "Literature,Science,and_the_Arts" ~ "lsa", # lit, science, and arts
                                     question_no == "Q6" & response == "Engineering" ~ "eng", # engineering
                                     question_no == "Q6" & !is.na(response) & response != "Prefer_not_to_answer" ~ "other_school", # classifying other schools as "Other"
                                     question_no == "Q10" & !is.na(response) ~ "domestic", # anything other than NA = citizen
                                     question_no == "Q11" & !is.na(response) ~ "international", # anything other NA = non-citizen
                                     question_no == "Q12" & response == "Yes" ~ "english_first",
                                     question_no == "Q12" & !is.na(response) & response != "Prefer_not_to_answer" ~ "english_acquired",
                                     question_no == "Q17" & response == "Yes" ~ "dependents",
                                     question_no == "Q17" & response == "No" ~ "no_dependents",
                                     question_no == "Q16" & response == "Single,never_married" ~ "single_unmarried",
                                     question_no == "Q16" & response == "Married_or_in_domestic_partnership" ~ "married_partnership",
                                     question_no == "Q16" & response == "Widowed" ~ "widowed",
                                     question_no == "Q16" & response == "Divorced_or_separated" ~ "divorced_separated",
                                     question_no == "Q14" & response == "Female" ~ "female",
                                     question_no == "Q14" & response == "Male" ~ "male",
                                     question_no == "Q14" & response == "Non-binary" ~ "non_binary",
                                     question_no == "Q15" & response == "Yes" ~ "under_represented",
                                     question_no == "Q15" & response == "No" ~ "well_represented",
                                     question_no == "Q31" & response == "Academic_–_research" ~ "academic_research",
                                     question_no == "Q31" & response == "Academic_–_teaching" ~ "academic_teaching",
                                     question_no == "Q31" & response == "Non-academic" ~ "non_academic",
                                     question_no == "Q31" & response == "Unsure" ~ "career_unsure",
                                     question_no == "Q8" & response == "Yes" ~ "first_postdoc",
                                     question_no == "Q8" & response == "No" ~ "not_first_postdoc",
                                     question_no == "Q39" & response == "Agree" ~ "satisfied",
                                     question_no == "Q39" & response == "Disagree" ~ "unsatisfied",
                                     question_no == "Q39" & response == "Neutral" ~ "neutral_satisfaction",
                                     TRUE ~ NA_character_))



# breaking up data by stratification categories ---------------------------

# creating the function to separate the data into different data frames based on individual stratification categories
make_stratified_data <- function(strat_col_values) {
  resp_id_list <- classified_survey_data %>% 
    filter(stratifications == strat_col_values) %>%
    pull(response_id) # extracts all values from col as text string
  stratified_data <- tidy_survey_data %>% 
    filter(response_id %in% resp_id_list) # extracts responses to all questions for each response_id in id_list
  return(stratified_data)
}

stratification_categories <- unique(na.omit(classified_survey_data$stratifications))

stratified_data <- map(stratification_categories, make_stratified_data) %>% 
  set_names(stratification_categories)



get_response_freq2 <- function(question_no_chr) { # question_no_chr = question number in character format (aka need quotes)
  freq <- stratified_data$domestic %>% # assigning the output to a variable
    filter(question_no == question_no_chr) %>% # selects rows containing responses for specific questions
    group_by(question_no, subquestion_no, question, response) %>% # groups by question and response to provide summary stats
    summarize(n = n()) %>% # creates col for number of a given response (n)
    mutate(percent_freq = n/sum(n)*100) # creates col for percent of total responses attributed to a given response
  return(freq) # returns the calculated/modified data frame
}

get_response_freq2(stratified_data$domestic, "Q37")

map(multi_choice_question_list, get_response_freq2) %>% 
  set_names(paste(names(stratified_data)[[1]], multi_choice_question_list, sep = "_"))



names(stratified_data)[[1]]

pmap(list(df = stratified_data, question_no_chr = multi_choice_question_list), get_response_freq2)

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
