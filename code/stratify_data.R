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



# stratifying data based on responses
# NOTE: need to find way to streamline process instead of hard coding everything (also need to clean up code in places)
classified_survey_data <- question_data %>% 
  mutate(stratifications = case_when(question_no == "Q6" & response == "Medicine" ~ "UMMS", # school of medicine
                                     question_no == "Q6" & response == "Literature,Science,and_the_Arts" ~ "LSA", # lit, science, and arts
                                     question_no == "Q6" & response == "Engineering" ~ "ENG", # engineering
                                     question_no == "Q6" & !is.na(response) & response != "Prefer_not_to_answer" ~ "Other", # classifying other schools as "Other"
                                     question_no == "Q9" & response == "Yes" ~ "Domestic", # anything other than NA = citizen
                                     question_no == "Q9" & response == "No" ~ "International", # anything other NA = non-citizen
                                     question_no == "Q12" & response == "Yes" ~ "EFL",
                                     question_no == "Q12" & !is.na(response) & response != "Prefer_not_to_answer" ~ "EAL",
                                     question_no == "Q17" & response == "Yes" ~ "Dependents",
                                     question_no == "Q17" & response == "No" ~ "No_dependents",
                                     question_no == "Q16" & response == "Single,never_married" ~ "Single",
                                     question_no == "Q16" & response == "Married_or_in_domestic_partnership" ~ "Married/partnered",
                                     question_no == "Q16" & response == "Widowed" ~ "Widowed",
                                     question_no == "Q16" & response == "Divorced_or_separated" ~ "Divorced/separated",
                                     question_no == "Q14" & response == "Female" ~ "Female",
                                     question_no == "Q14" & response == "Male" ~ "Male",
                                     question_no == "Q14" & response == "Non-binary" ~ "Non-binary",
                                     question_no == "Q15" & response == "Yes" ~ "Under-represented",
                                     question_no == "Q15" & response == "No" ~ "Well-represented",
                                     question_no == "Q31" & response == "Academic_(research)" ~ "Academic_(research)",
                                     question_no == "Q31" & response == "Academic_(teaching)" ~ "Academic_(teaching)",
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
# pulls data one strat category (ex: "UMMS") at a time, need to use map_df to join data from all strat categories for a given strat_id (ex: "college_school")
make_stratified_data <- function(strat_col_value) {
  
  resp_id_list <- classified_survey_data %>% 
    filter(stratifications == strat_col_value) %>% # filters rows to contain only data for specified stratification category
    pull(response_id) # extracts all response_ids from col as chr vector
  
  strat_question_no <- classified_survey_data %>% 
    filter(stratifications == strat_col_value) %>% # filters rows to contain only data for specified stratification category
    pull(question_no) %>% # extracts question number used to stratify category
    unique() # narrows down to a single chr string question number
  
  # adding in a filter that removes and strat_id classifications if less than two people share that classification
  # prevents potentially identifying information from being included in downstream analyses
  if (length(resp_id_list) > 2) {
    
    # creating df of data to be labeled by strat category
    # also sorts and renumbers questions so question used for stratification becomes Q1 while making plots
    stratified_data <- question_data %>% 
      filter(response_id %in% resp_id_list) %>%  # extracts responses to all questions for each response_id in resp_id_list
      mutate(question_no = case_when(question_no == "Q49" ~ "Q35.5", # changing position of Q49 to be near questions of similar topics
                                     question_no == strat_question_no ~ "Q0.1", # reassigns question used for stratification as question 1 so it'll always be the first item when grouped later
                                     TRUE ~ question_no), # all other questions maintain same question number
             strat_id = strat_col_value) %>%  # creating a new col to label the responses with the specified stratification category
      mutate(question_no = as.numeric(str_extract(question_no, "\\d+\\.?\\d*\\b"))) %>% # converting question numbers to numerics for proper sorting
      mutate(sorted_question_no = group_indices(., question_no)) %>% # renumbering questions so there aren't any gaps in the question sequence but leaves original for comparison
      arrange(response_id, sorted_question_no) %>% # rearranges order of questions in resulting df (makes it more human readable)
      mutate(question_no = paste("Q", question_no, sep = ""), # pastes a "Q" onto the front of each question number for ease of filtering later
             sorted_question_no = paste("Q", sorted_question_no, sep = "")) %>%  # pastes a "Q" onto the front of each question number to make it more visible
      mutate(question_no = case_when(sorted_question_no == "Q1" ~ strat_question_no, # changes the original strat_question_no (should be Q1 in sorted) back to what it was in the original data
                                     question_no == "Q35.5" ~ "Q49", # changes the original question_no back to what it was in the original data
                                     TRUE ~ question_no)) # leaves all the rest of the question_no's unchanged
    
    return(stratified_data)
    
  }
  
}

# creating a named list of all of the desired breakdowns for comparisons then naming the output list
# the output will maintain the names given to the items in the input list which is why naming at this step is so important
strat_list <- list(college_school = c("UMMS", "ENG", "LSA", "Other"), 
                   postdoc_no = c("First_postdoc", ">1_postdoc"),
                   residency = c("Domestic", "International"),
                   language = c("EFL", "EAL"),
                   gender = c("Male", "Female", "Non-binary"),
                   pop_representation = c("Under-represented", "Well-represented"),
                   relationship_status = c("Single", "Married/partnered", "Widowed", "Divorced/separated"),
                   dependents = c("Dependents", "No_dependents"),
                   career_track = c("Academic_(research)", "Academic_(teaching)", "Non-academic", "Unsure"),
                   satisfaction = c("Satisfied", "Unsatisfied", "Neutral")) 
  
# setting up a function to allow use of map to iterate through each set of stratification classifications
get_strat_data <- function(x) {
  data <- map_df(x, make_stratified_data) # map_df iterates through each item in input list then collates output as a single dataframe
  return(data) 
}

# using map to run the functions using the different classifications of data
# due to removal of "prefer not to answer", output dfs will be slightly smaller than input tidied df
strat_data <- map(strat_list, get_strat_data) # running a nested map series takes collated data and outputs as one dataframe in the list per stratification category



# notes -------------------------------------------------------------------
# strat_data$career_track
# question_data
# strat_data$career_track %>%
#   filter(question_no == "Q31") %>%
#   pull(strat_id) %>% 
#   unique()
