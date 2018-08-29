# loading packages --------------------------------------------------------

# NOTE: see final section for extended notes and extra code

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
  select(response_id, question_no, question, response) %>% # rearranging the cols to be human readable
  mutate_all(funs(str_replace_all(., " \\, | \\,|\\, ", ","))) %>% # removing spaces around commas specifically to make lists easier
  mutate_all(funs(str_replace_all(., "  | |\\n", "_"))) %>% # getting rid of spaces and line breaks ("\n") in questions
  mutate_all(funs(str_replace_all(., "\\)|\\(", ""))) %>% # getting rid of symbols that may cause problems
  mutate_all(funs(str_replace_all(., "The_.*_-_|Please_.*_-_", ""))) %>% # getting rid of repeated phrasing for multi-part questions
  mutate_all(funs(str_replace(., "\\t", ""))) %>% # runs str_replace on each variable to remove aberrant "\t" at beginning of each line
  mutate_all(funs(str_replace_all(., "\\t", ","))) %>% # replaces any internal "\t" with "," to make it easier to separate lists
  mutate_all(funs(str_replace_all(., "\\,\\,", ","))) # replaces any double commas with a single comma (tried finding reason for ",," in code but couldn't, probably something to do with "\t")



# generating statistics ---------------------------------------------------

# creating list of all the questions
question_list <- unique(tidy_survey_data$question_no)

# for loop for generating question specific response tibbles
for (i in question_list) {
  assign(paste(i, "statistics", sep = "_"), tidy_survey_data %>%
    filter(question_no == i) %>%
    group_by(question_no, question, response) %>%
    summarize(n = n()) %>%
    mutate(percent_freq = n/sum(n)*100))
  eval(as.name(paste(i, "statistics", sep = "_")))
}

# function for generating individual response frequencies
get_question_response_freq <- function(question_number_chr) {
  question_response_freq <- tidy_survey_data %>% 
    filter(question_no == question_number_chr) %>% 
    group_by(question_no, question, response) %>% 
    summarize(n = n()) %>% 
    mutate(percent_freq = n/sum(n)*100)
  return(question_response_freq)
}



# generating plots --------------------------------------------------------

# NOTE: Q22 responses need to be broken up and retabulated before graphing

# making list of questions to exclude from graph generation because they were typed response
# to change the questions included in this list, add/subtract/change the numbers as desired
typed_question_list <- question_list[grepl(paste(paste0("Q", c(10,11,23,29,42,43,44,45,46,51)), collapse = "|"), question_list)]

# list of the remaining questions to be used for graph generation
predetermined_question_list <- setdiff(question_list, typed_question_list)

# creating a function to wrap the text of a particularly long title by setting the max number of characters desired per line (width)
# ex: wrapper("Super long title", width = 50)
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

# making for loop for rapid generation of plots using questions with predetermined responses
for (i in predetermined_question_list) {
  assign(paste(i, "plot", sep = "_"), eval(as.name(paste0(i, "_statistics"))) %>% 
           ggplot() +
           geom_bar(aes(x = response, y = percent_freq, fill = response),
                    color = "black", show.legend = F, stat = "identity", width = 0.75) +
           geom_text(aes(x = response, y = percent_freq, label = paste0(round(percent_freq, 1), "%")), vjust = -0.5) +
           scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
           scale_x_discrete(labels = c(str_replace_all(eval(as.name(paste0(i, "_statistics")))$response, "_", " "))) +
           labs(title = wrapper(str_replace_all(unique(eval(as.name(paste0(i, "_statistics")))$question), "_", " "), width = 60),
                y = "Proportion of Postdoctoral Respondents (%)",
                x = "Responses") +
           theme_classic() +
           theme(plot.title = element_text(hjust = 0.5)))
}

# testing output from for loop
Q19_8_plot

# making a plot generating function
make_response_plots <- function(question_number_chr) {
  response_plot <- eval(as.name(paste0(question_number_chr, "_statistics"))) %>% 
    ggplot() +
    geom_bar(aes(x = response, y = percent_freq, fill = response),
             color = "black", show.legend = F, stat = "identity", width = 0.75) +
    geom_text(aes(x = response, y = percent_freq, label = paste0(round(percent_freq, 1), "%")), vjust = -0.5) +
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
    scale_x_discrete(labels = c(str_replace_all(eval(as.name(paste0(question_number_chr, "_statistics")))$response, "_", " "))) +
    labs(title = wrapper(str_replace_all(unique(eval(as.name(paste0(question_number_chr, "_statistics")))$question), "_", " "), width = 60),
         y = "Proportion of Postdoctoral Respondents (%)",
         x = "Responses") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  return(response_plot)
}

# testing response plot function
make_response_plots("Q25")



# notes/extra code ----------------------------------------------------------

##### tidying data #####

# # removing unnecessary data
# filtered_survey_data <- survey_data %>% 
#   select(ResponseId, contains("Q"), -matches("Q47|Q50")) %>% # removes excess columns/metadata
#   filter(!str_detect(Q6, "ImportId"))  # removing extra row
# 
# # tidying filtered data
# tidy_survey_data <- filtered_survey_data %>% # removes ImportId line
#   rename_all(funs(paste(colnames(filtered_survey_data), filtered_survey_data[1,], sep = "."))) %>% # adding question text to question number in col names
#   slice(-1) %>% # removes first row which contained the question text
#   rename_all(funs(str_replace_all(., " |\\n", "_"))) %>% # getting rid of spaces and line breaks ("\n") in questions
#   rename_all(funs(str_replace_all(., "\\)|\\(|\\,|\\/", ""))) %>% # getting rid of symbols that may cause problems
#   rename_all(funs(str_replace_all(., "The_.*_-_|Please_.*_-_", ""))) %>% # getting rid of repeated phrasing for multi-part questions
#   mutate_all(funs(str_replace(., "\\t", ""))) %>% # runs str_replace on each variable to remove aberrant "\t" at beginning of each line
#   gather(question, response, 2:ncol(.))
  
# # testing removal of "\t" string in responses during tidy_survey_data generation
# survey_data$Q7[2:10] # before
# tidy_survey_data$`Q7.How_long_have_you_been_a_postdoctoral_fellow_at_the_University_of_Michigan?`[2:10] # after
# grep("\\t", tidy_survey_data$`Q7.How_long_have_you_been_a_postdoctoral_fellow_at_the_University_of_Michigan?`) # searching just in case
# # "\t" was successfully removed
# 
# # testing col name formatting
# colnames(tidy_survey_data)



##### tidying data #####

# # generating "n" and "frequency" statistics for individual questions (made into a for loop in script)
# tidy_survey_data %>% 
#   filter(question_no == "Q10") %>% 
#   group_by(question_no, question, response) %>% 
#   summarize(n = n()) %>% 
#   mutate(freq = n/sum(n))

# # ended up putting statistics generation into a function instead of a for loop
# for (i in unique(tidy_survey_data$question_no)) {
#   survey_freq <- tidy_survey_data %>% 
#     filter(question_no == i) %>% 
#     group_by(question_no, question, response) %>% 
#     summarize(n = n()) %>% 
#     mutate(freq = n/sum(n))
#   return(survey_freq)
# }

# # made functional for loop for generating response rates but will be keeping function as well
# # testing for loop functionality
# assign(paste("Q23", "statistics", sep = "_"), tidy_survey_data %>%
#          filter(question_no == "Q23") %>%
#          group_by(question_no, question, response) %>%
#          summarize(n = n()) %>%
#          mutate(freq = n/sum(n)))
# eval(as.name("Q23_statistics"))



##### generating plots #####

# # creating a template to plot statistics question by question
# Q12_plot <- Q12_statistics %>% 
#   ggplot() +
#   geom_bar(aes(x = response, y = percent_freq, fill = response), show.legend = F, stat = "identity") +
#   scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
#   scale_x_discrete(labels = c(str_replace_all(Q12_statistics$response, "_", " "))) +
#   labs(title = str_replace_all(unique(Q12_statistics$question), "_", " "),
#        y = "Proportion of Postdoctoral Respondents (%)",
#        x = "Responses")
# # looking at variable containing the plot
# Q12_plot

# # template v2
# Q12_plot <- eval(as.name(paste0("Q12", "_statistics"))) %>% 
#   ggplot() +
#   geom_bar(aes(x = response, y = percent_freq, fill = response), # colors based on response
#            color = "black", show.legend = F, stat = "identity", width = 0.75) + # black outlined bars, no legend, bar heights = percent from data frame instead of counts
#   geom_text(aes(x = response, y = percent_freq, label = paste0(round(percent_freq, 1), "%")), vjust = -0.5) + # adds the percent per answer above each bar rounded to the first decimal point
#   scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # setting the y axis limits to be consistent
#   scale_x_discrete(labels = c(str_replace_all(eval(as.name(paste0("Q12", "_statistics")))$response, "_", " "))) + # allows dynamic labeling of the x axis categories based on question
#   labs(title = str_replace_all(unique(eval(as.name(paste0("Q12", "_statistics")))$question), "_", " "), # adds dynamic labeling of the chart title based on question used
#        y = "Proportion of Postdoctoral Respondents (%)",
#        x = "Responses") +
#   theme_classic() +
#   theme(plot.title = element_text(hjust = 0.5)) # centers the chart title
# #checking output
# Q12_plot



##### end #####
