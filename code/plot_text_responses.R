# sentiment scores (several options)
  # do by question and include most pos/neg words
  # look at emotions (fear, joy, etc.)
  # sentiment of question + sentiment of words in each questions
# word importance (LDA/TF-IDF)
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

# when comparing all the data
# word cloud per question
  # tokens vs bigrams vs trigrams
# top 20 tokens per question chart
# top 20 bigrams
# top 20 trigrams

# when comparing stratifications
# everything from before but add:
# TF-IDF (most important words per question) - token
# TF-IDF - bigram
# TF-IDF - trigram

# importing stuff ---------------------------------------------------------

# import dataset
source("code/tidy_survey.R")


library(tidyverse)
library(tidytext) # text manipulation, used for tokenization and stop words
library(wordcloud) # word cloud
# library(stringr) #string manipulation
# library(igraph)
# library(ggraph)
# 
# library(widyr)
# library(broom)
# 
# library(DT)
# 
# library(irlba)
# library(topicmodels) # for LDA topic modelling 
# library(tm) # general text mining functions, making document term matrixes
# 
# library(caret)
# library(glmnet)



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

# top n number of words bar plot
createBarPlotCommonWords = function(train,title)
{
  train %>%
    mutate(response = str_replace_all(response, "_", " ")) %>% # removing all "_" to prevent identification as a single word
    unnest_tokens(word, response) %>%
    filter(!word %in% stop_words$word) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    head(10) %>%
    
    ggplot(aes(x = word,y = n)) +
    geom_bar(stat='identity',colour="white") +
    geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Word', y = 'Word Count', 
         title = title) +
    coord_flip() + 
    theme_bw()
  
}

typed_question_df %>% 
  filter(question_no == "Q51" & !is.na(response)) %>% 
  createBarPlotCommonWords('Top 10 most Common Words')

# word clouds
createWordCloud = function(train)
{
  train %>%
    mutate(response = str_replace_all(response, "_", " ")) %>% # removing all "_" to prevent identification as a single word
    unnest_tokens(word, response) %>%
    filter(!word %in% stop_words$word) %>%
    count(word,sort = TRUE) %>%
    ungroup()  %>%
    head(50) %>%
    
    with(wordcloud(word, n, max.words = 50,colors=brewer.pal(8, "Dark2"))) 
}

typed_question_df %>% 
  filter(question_no == "Q51" & !is.na(response)) %>%
  createWordCloud()




# Term Frequency - Inverse Document Frequency (aka most important words)
# need to compare questions that are inverse of each other
# works similar to random forest modelling
trainWords <- typed_question_df %>%
  mutate(response = str_replace_all(response, "_", " ")) %>% # removing all "_" to prevent identification as a single word
  unnest_tokens(word, response) %>%
  filter(!is.na(word)) %>% 
  count(question_no, word, sort = TRUE) %>%
  ungroup()

total_words <- trainWords %>% 
  group_by(question_no) %>% 
  summarize(total = sum(n))

trainWords <- left_join(trainWords, total_words)

#Now we are ready to use the bind_tf_idf which computes the tf-idf for each term. 
trainWords <- trainWords %>%
  filter(!is.na(question_no)) %>%
  bind_tf_idf(word, question_no, n)


plot_trainWords <- trainWords %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_trainWords

plot_trainWords %>% 
  top_n(20) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  theme_bw()

# deciding which classification system to use for sentiment analysis
# afinn has best score distribution (ex: negative scores align well with negative opinions) and better range (-5 to 5) but fewer words in lexicon (2476 words)
# bing has second best score distribution and significantly more words in lexicon (6788 words) but scores are either "pos" or "neg"
get_sentiments("bing") %>% 
  select(sentiment) %>% 
  unique()
get_sentiments("afinn") %>% 
  select(score) %>% 
  unique() %>% 
  pull() %>% 
  sort()


calc_sentiment <- function(survey_df) {
  sentiments <- survey_df %>% 
    unnest_tokens(word, response)
}

visualize_sentiments <- function(SCWords) {
  SCWords_sentiments <- SCWords %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(Character) %>%
    summarize(score = sum(score * n) / sum(n)) %>%
    arrange(desc(score))
  
  SCWords_sentiments %>%
    top_n(10) %>%
    mutate(Character = reorder(Character, score)) %>%
    ggplot(aes(Character, score, fill = score > 0)) +
    geom_col(show.legend = TRUE) +
    coord_flip() +
    ylab("Average sentiment score") + theme_bw()
}

# testing AFINN based scoring
# basing code off of example from kaggle on text mining:
# https://www.kaggle.com/ambarish/seinfeld-text-mining-wordembeddings-modelling
top_30_sentiment <- typed_question_df %>% # calling df of typed question from survey data
  mutate(response = str_replace_all(response, "_", " ")) %>% # removing all "_" left from tidying dataset
  filter(question_no == "Q43") %>% # filtering to a specific questions
  unnest_tokens(word, response) %>% # breaks apart responses into individual words
  filter(!is.na(word)) %>% # removes any blank rows
  inner_join(get_sentiments("afinn"), by = "word") %>% # adds AFINN scores to words from dataset if present in each
  group_by(word) %>% # groups based on word
  summarize(n = n(), # counts number of occurrences
            cum_score = sum(score)) %>% # calculates (score * number of occurrences) of each word to find relative contribution to total score
  mutate(percent_score = (cum_score / sum(cum_score)) * 100) %>%  # calculates percent of total score attributed to each word
  arrange(desc(percent_score)) %>% # arranges words based on percent score (makes human readable but not preserved for plotting)
  mutate(word = reorder(word, percent_score)) %>% # reorders the words for use in plotting
  top_n(30, abs(cum_score)) # selects top number of words based on absolute score for contribution

# plotting based on sentiment scores
top_30_sentiment %>% 
  ggplot(aes(word, percent_score, fill = percent_score > 0)) + # colors based on score
  geom_col(show.legend = TRUE) + # makes bar plot
  coord_flip() # turns plot on its side

# NOTE: AFINN is score based so quantitative scores can be calculated/plotted, bing is classification based so is only binary pos or neg

typed_question_df %>% 
  mutate(response = str_replace_all(response, "_", " ")) %>% 
  unnest_tokens(word, response) %>% 
  filter(!is.na(word)) %>% 
  inner_join(get_sentiments("nrc"), by = "word")






