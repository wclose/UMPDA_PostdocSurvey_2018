# sentiment scores (several options)
  # do by question and include most pos/neg words
  # look at emotions (fear, joy, etc.)
  # sentiment of question + sentiment of words in each questions
# word importance (LDA)
# word clouds (can do in shape of something)
# word relationships
  # networks
  # bigrams
  # trigrams


# plan:
  # break responses into individual words (tokenization)
  # remove common words to focus analysis
  # pool all responses and analyze each question
  # break responses up by strat ids and analyze each question

# list of questions with text responses
  # Q: 


# importing stuff ---------------------------------------------------------

# import dataset
source("code/tidy_survey.R")


library(tidyverse)
library(tidytext) # text manipulation, used for tokenization and stop words
library(wordcloud) # word cloud
library(stringr) #string manipulation
library(igraph)
library(ggraph)

library(widyr)
library(broom)

library(DT)

library(irlba)
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes

library(caret)
library(glmnet)



# text analysis -----------------------------------------------------------

# creating list of all the questions
question_list <- unique(tidy_survey_data$question_no)

# making list of questions to exclude from graph generation because they were typed response
# to change the questions included in this list, add/subtract/change the numbers in paste0() as desired
typed_question_list <- question_list[grepl(paste(paste0("Q", c(23,29,42,43,44,45,46,51)), collapse = "|"), question_list)]

typed_question_df <- tidy_survey_data %>% 
  filter(question_no %in% typed_question_list)

typed_question_df %>% 
  mutate(response = str_replace_all(response, "_", " ")) %>% # removing all "_" to prevent identification as a single word
  unnest_tokens(word, response) %>% # breaks sentences apart into individual rows
  filter(!word %in% stop_words$word) # filters out stop words which are common words with minimal impact on sentence significance


createBarPlotCommonWords = function(train,title)
{
  train %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    head(10) %>%
    
    ggplot(aes(x = word,y = n)) +
    geom_bar(stat='identity',colour="white", fill =fillColor) +
    geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Word', y = 'Word Count', 
         title = title) +
    coord_flip() + 
    theme_bw()
  
}

createBarPlotCommonWords(scripts,'Top 10 most Common Words')










