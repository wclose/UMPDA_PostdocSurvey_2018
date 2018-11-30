# loading packages --------------------------------------------------------

# loading packages
library(tidyverse)



# importing data and tidying ----------------------------------------------

# importing survey data
survey_data <- read_csv("data/UMPDA_2018_cross-section_survey.csv")

# removing unnecessary data and tidying
tidy_survey_data <- survey_data %>% 
  select(ResponseId, contains("Q"), -matches("Q47|Q50")) %>% # removes excess columns/metadata including cols for opting in for t shirts
  filter(!str_detect(Q6, "ImportId")) %>% # removing extra row
  gather(question_no, response, 2:ncol(.)) %>% # gathering data to make cols for question number and responses
  spread(ResponseId, response) %>% # spreading the table back out to separate question text from responses
  rename(question = "Response ID") %>% # renaming col containing question text
  gather(response_id, response, -contains("question")) %>% # re-gathering data putting all response IDs in one col and answers in another
  separate(question_no, into = c("question_no", "subquestion_no"), sep = "_", extra = "merge", fill = "right") %>% # separating the subquestion numbers into a different col
  select(response_id, question_no, subquestion_no, question, response) %>% # rearranging the cols to be human readable
  mutate_all(funs(str_replace_all(., "–", "-"))) %>% # replacing long hyphens with regular sized ones
  mutate_all(funs(str_replace_all(., " \\, | \\,|\\, ", ","))) %>% # removing spaces around commas specifically to make lists easier
  mutate_all(funs(str_replace_all(., "  | |\\n", "_"))) %>% # getting rid of spaces and line breaks ("\n") in questions
  mutate_all(funs(str_replace_all(., "\\)|\\(", ""))) %>% # getting rid of symbols that may cause problems
  mutate_all(funs(str_replace_all(., "The_.*_-_|Please_.*_-_", ""))) %>% # getting rid of repeated phrasing for multi-part questions
  mutate_all(funs(str_replace(., "\\t", ""))) %>% # runs str_replace on each variable to remove aberrant "\t" at beginning of each line
  mutate_all(funs(str_replace_all(., "\\t", ","))) %>% # replaces any internal "\t" with "," to make it easier to separate lists
  mutate_all(funs(str_replace_all(., "\\,\\,", ","))) %>%  # replaces any double commas with a single comma (tried finding reason for ",," in code but couldn't, probably something to do with "\t")
  mutate(response = ifelse(question_no == "Q22", str_split(response, ","), response)) %>% # splits concatenated answers for Q22 into a list of strings
  unnest(response) # breaks col of lists into separate rows (does not affect other answers)

# creating df of all survey data minus specific location data (will be plotted elsewhere/differently)
# also reassigns position of Q49 in order so it's nearer similar questions before renumbering the questions
question_data <- tidy_survey_data %>% 
  filter(question_no != "Q10" & question_no != "Q11") %>% 
  mutate(question_no = case_when(question_no == "Q49" ~ "Q35.5", # changing position of Q49 to be near questions of similar topics
                                 TRUE ~ question_no)) %>%  # all other questions maintain same question number
  mutate(question_no = as.numeric(str_extract(question_no, "\\d+\\.?\\d*\\b"))) %>% # converting question numbers to numerics for proper sorting
  mutate(sorted_question_no = group_indices(., question_no)) %>% # renumbering questions so there aren't any gaps in the question sequence but leaves original for comparison
  arrange(response_id, sorted_question_no) %>% # rearranges order of questions in resulting df (makes it more human readable)
  mutate(question_no = paste("Q", question_no, sep = ""), # pastes a "Q" onto the front of each question number for ease of filtering later
         sorted_question_no = paste("Q", sorted_question_no, sep = "")) %>%  # pastes a "Q" onto the front of each question number to make it more visible
  mutate(question_no = case_when(question_no == "Q35.5" ~ "Q49", # changes the original question_no back to what it was in the original data
                                 TRUE ~ question_no)) # leaves all the rest of the question_no's unchanged

# creating separate dataframe for plotting degree location maps (will require different methods than the rest of the data)
location_data <- tidy_survey_data %>% 
  filter(question_no == "Q10" | question_no == "Q11")



# creating question lists -------------------------------------------------

# creating list of all the questions
question_list <- unique(question_data$question_no)

# making list of questions to exclude from graph generation because they were typed response
# to change the questions included in this list, add/subtract/change the numbers in paste0() as desired
typed_question_list <- question_list[grepl(paste(paste0("Q", c(23,26,29,42,43,44,45,46,51)), collapse = "|"), question_list)]

# list of the remaining questions to be used for graph generation
multi_choice_question_list <- setdiff(question_list, typed_question_list)



# notes -------------------------------------------------------------------

# split location questions into separate df
# will make it easier to renumber other questions and still use map functions (location question numbers won't change)
# also change responses to be international vs domestic for Q10 vs Q11 (or make new question with domestic vs international vs na)
# Need to take data from Q10/Q11 and combine into a single question -> Q10.5
# Are you a U.S. citizen? Yes/No based on responses to Q10/11 (can look at format from stratify script)
# USE Q9

# tidy_survey_data %>%
#   filter(question == "Are_you_a_U.S._citizen?")


