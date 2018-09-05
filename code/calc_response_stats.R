# loading dependencies ----------------------------------------------------

# loading tidyverse and dataset
source("code/tidy_survey.R")



# creating juxtaposition categories ---------------------------------------

# NOTE: need to filter out NA responses to stratification questions (unless it's valid)


# # breakdown categories
# UMMS, LSA, Eng, other
# Domestic v. International and/or EFL v. EAL
# Dependents v. no dependents
# Partner v. no partner
# Male v. female (only 1 non-binary responder)
# Well v. Under-represented
# Academic v. non-academic career track
# First v. multiple PD position
# Satisfied v. not satisfied

# school/college (UMMS, LSA, ENG, Other)
postdoc_college <- tidy_survey_data %>% 
  mutate(college = case_when(question_no != "Q6" ~ NA_character_, # makes college col "NA" for all questions other than Q6
                             question_no == "Q6" & response == "Medicine" ~ "UMMS", # school of medicine
                             question_no == "Q6" & response == "Literature,Science,and_the_Arts" ~ "LSA", # lit, science, and arts
                             question_no == "Q6" & response == "Engineering" ~ "ENG", # engineering
                             TRUE ~ "Other")) # makes remaining responses for college category "Other" to aggregate remaining colleges

list <- postdoc_college %>% 
  filter(college == "UMMS") %>% 
  pull(response_id)

tidy_survey_data %>% 
  filter(response_id %in% list)

# create fn: 
# id_list <- for _ in unique(!is.na(college)), filter( == paste(i)), pull(response_id)
# paste(dfname_i) <- tidy_survey_data %>% filter response_id %in% id_list
# return(paste(dfname_i))
# map to create named list of new dataframes
# map to do stats
# map to make plots
# integrate into Rmd
paste(unique((postdoc_college$college)), "test", sep = "_")

college_list <- c("UMMS", "ENG", "LSA", "Other")

test_function <- function(colleges) {
  id_list <- postdoc_college %>% 
    filter(college == colleges) %>% 
    pull(response_id)
  relevant_info <- tidy_survey_data %>% 
    filter(response_id %in% unique(id_list))
  return(relevant_info)
}

test_function("Other")

map(college_list, test_function)



# domestic v. international
# Q10 = only answered if US citizen
# Q11 = only answered if NOT US citizen
# classifying responses to both just in case
tidy_survey_data %>% 
  mutate(residency = case_when(question_no == "Q10" & !is.na(response) ~ "domestic", # anything other than NA = citizen
                               question_no == "Q10" & is.na(response) ~ "international", # anything NA = non-citizen
                               question_no == "Q11" & !is.na(response) ~ "international", # anything other NA = non-citizen
                               question_no == "Q11" & is.na(response) ~ "domestic", # anything NA = citizen
                               TRUE ~ NA_character_)) # makes all other rows "NA"

residency_status <- c("domestic","international")



big_data <- tidy_survey_data %>% 
  mutate(college = case_when(question_no != "Q6" ~ NA_character_, # makes college col "NA" for all questions other than Q6
                             question_no == "Q6" & response == "Medicine" ~ "UMMS", # school of medicine
                             question_no == "Q6" & response == "Literature,Science,and_the_Arts" ~ "LSA", # lit, science, and arts
                             question_no == "Q6" & response == "Engineering" ~ "ENG", # engineering
                             TRUE ~ "Other"), # makes remaining responses for college category "Other" to aggregate remaining colleges
         residency = case_when(question_no == "Q10" & !is.na(response) ~ "domestic", # anything other than NA = citizen
                               question_no == "Q10" & is.na(response) ~ "international", # anything NA = non-citizen
                               question_no == "Q11" & !is.na(response) ~ "international", # anything other NA = non-citizen
                               question_no == "Q11" & is.na(response) ~ "domestic", # anything NA = citizen
                               TRUE ~ NA_character_)) # makes all other rows "NA"





# english as first language v. english as acquired language
tidy_survey_data %>% 
  mutate(first_language = case_when(question_no != "Q12" ~ NA_character_, # makes col "NA" for all questions other than Q6
                                    question_no == "Q12" & response == "Yes" ~ "english",
                                    TRUE ~ "other"))

# dependents v. no dependents
tidy_survey_data %>% 
  mutate(dependents = case_when(question_no != "Q17" ~ NA_character_, # makes col "NA" for all questions other than Q6
                                question_no == "Q17" & response == "Yes" ~ "dependents",
                                TRUE ~ "none"))

# partner v. no partner
tidy_survey_data %>% 
  mutate(relationship = case_when(question_no != "Q16" ~ NA_character_, # makes col "NA" for all questions other than Q6
                                  question_no == "Q16" & response == "Single,never_married" ~ "single",
                                  question_no == "Q16" & response == "Married_or_in_domestic_partnership" ~ "married_partnership",
                                  question_no == "Q16" & response == "Widowed" ~ "widowed",
                                  question_no == "Q16" & response == "Divorced_or_separated" ~ "divorced_separated",
                                  TRUE ~ "no_answer"))

# male v. female
tidy_survey_data %>% 
  mutate(gender = case_when(question_no != "Q14" ~ NA_character_, # makes col "NA" for all questions other than Q6
                            question_no == "Q14" & response == "Female" ~ "female",
                            question_no == "Q14" & response == "Male" ~ "male",
                            TRUE ~ "other"))

# well-represented v. under-represented
tidy_survey_data %>% 
  mutate(cultural_representation = case_when(question_no != "Q15" ~ NA_character_, # makes col "NA" for all questions other than Q6
                                             question_no == "Q15" & response == "Yes" ~ "under_represented",
                                             TRUE ~ "well-represented"))

# career track (academic - research, academic - teaching, non-academic)
tidy_survey_data %>% 
  mutate(career_track = case_when(question_no != "Q31" ~ NA_character_, # makes col "NA" for all questions other than Q6
                                  question_no == "Q31" & response == "Academic_–_research" ~ "academic-research",
                                  question_no == "Q31" & response == "Academic_–_teaching" ~ "academic-teaching",
                                  question_no == "Q31" & response == "Non-academic" ~ "non-academic",
                                  TRUE ~ "unsure"))

# first postdoc v. >= 2 postdocs
tidy_survey_data %>% 
  mutate(postdoc_no = case_when(question_no != "Q8" ~ NA_character_, # makes col "NA" for all questions other than Q6
                                question_no == "Q8" & response == "Yes" ~ "first",
                                TRUE ~ "second_plus"))

# satisfied v. unsatisfied w/ postdoc
tidy_survey_data %>% 
  mutate(satisfaction = case_when(question_no != "Q39" ~ NA_character_, # makes col "NA" for all questions other than Q6
                                  question_no == "Q39" & response == "Agree" ~ "satisfied",
                                  question_no == "Q39" & response == "Disagree" ~ "unsatisfied",
                                  TRUE ~ "neutral"))



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



# typed statistics --------------------------------------------------------

# use this section to generate tables for plotting written responses



# notes -------------------------------------------------------------------


