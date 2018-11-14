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

tidy_survey_data %>% 
  filter(question_no == "Q22")

# notes -------------------------------------------------------------------


# tidy_survey_data %>% 
#   filter(question_no == "Q22") %>% 
#   filter(response_id == "R_0SAU591QCrEeuMp") %>% 
#   pull(response)
# 
# tidy_survey_data %>% 
#   filter(question_no == "Q22") %>% 
#   separate(response, sep = ",", c("a", "b"))
# 
# tidy_survey_data %>% 
#   filter(question_no == "Q22") %>% 
#   mutate(response = str_split(response, ",")) %>% 
#   unnest(response) %>% 
#   filter(response_id == "R_0SAU591QCrEeuMp") %>% 
#   pull(response)
# 
# tidy_survey_data %>% 
#   filter(question_no == "Q22") %>% 
#   mutate(response = str_split(response, ",")) %>% 
#   unnest(response)
# 
# tidy_survey_data %>% 
#   mutate(response = case_when(question_no == "Q22" ~ str_split(response, ","),
#                               TRUE ~ response))

tidy_survey_data %>% 
  mutate(response = ifelse(question_no == "Q22", str_split(response, ","), response)) %>% # splits concatenated answers for Q22 into a list of strings
  unnest(response) %>% # breaks col of lists into separate rows (does not affect other answers)
  filter(question_no == "Q22")
