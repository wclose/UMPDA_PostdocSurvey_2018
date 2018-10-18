# https://rpubs.com/joshyazman/sentiment-analysis-lexicon-comparison
# https://www.tidytextmining.com/
# https://lizrush.gitbooks.io/algorithms-for-webdevs-ebook/content/
# https://www.kaggle.com/xvivancos/analyzing-star-wars-movie-scripts
# https://www.kaggle.com/cosinektheta/mining-the-women-s-clothing-reviews
# https://www.kaggle.com/ambarish/seinfeld-text-mining-wordembeddings-modelling
# https://www.kaggle.com/ambarish/fun-in-text-mining-with-simpsons


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
library(wordcloud2)
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

# NOTE: stop_words includes "no", "not", etc. which may be bad for some types of analyses

# creating list of all the questions
question_list <- unique(tidy_survey_data$question_no)

# making list of questions to exclude from graph generation because they were typed response
# to change the questions included in this list, add/subtract/change the numbers in paste0() as desired
typed_question_list <- question_list[grepl(paste(paste0("Q", c(23,29,42,43,44,45,46,51)), collapse = "|"), question_list)]

typed_question_df <- tidy_survey_data %>% 
  filter(question_no %in% typed_question_list) %>% 
  mutate(response = str_replace_all(response, "_", " ")) %>% 
  filter(!is.na(response) & !str_detect(response, "\\bn[/]a\\b"))

# creating way to filter out all weird NA responses
test <- typed_question_df %>% 
  filter(str_detect(response, regex(paste(paste0("\\b", c("n[/]a", "na"), "\\b"), collapse = "|"), ignore_case = TRUE)))



#bigrams
typed_question_df %>% 
  unnest_tokens(word, response) %>% 
  #anti_join(stop_words) %>% # removes common words, may be bad for sentiment analysis because removes "no", "not", etc.
  count(word, sort = TRUE)





typed_question_df %>% 
  mutate(response = str_replace_all(response, "_", " ")) %>% # removing all "_" to prevent identification as a single word
  unnest_tokens(word, response) %>% # breaks sentences apart into individual rows
  filter(!word %in% stop_words$word) # filters out stop words which are common words with minimal impact on sentence significance

typed_question_df %>% 
  filter(question_no == "Q51" & !is.na(response)) %>%
  createWordCloud()




# tf-idf ------------------------------------------------------------------

# NOTE: Because of the size of the dataset at present (2018), tf-idf is limited because there aren't enough repeats of n-grams
# so several n-grams all have the same tf-idf value which makes plotting difficult. Could be fixed by repeating the survey and
# expanding the available data but limits yearly evaluations

# Term Frequency - Inverse Document Frequency (aka most important words)
# need to compare questions that are inverse of each other
# works similar to random forest modelling by finding words that are common but to only a subset of the collection of documents



# creating test dataset
example_data <- strat_data$college_school %>% 
  filter(question_no %in% typed_question_list) %>% # filtering to only contain text based responses
  filter(question_no == "Q44") %>% # only in place for testing purposes
  mutate(response = str_replace_all(response, "_", " ")) #%>% # removing extra symbols from tidying
  # filter(!is.na(response) & !str_detect(response, "\\bn[/]a\\b")) # removing any weird "NA" type responses people input

example_data2 <- tidy_survey_data %>% 
  filter(question_no %in% typed_question_list) %>% 
  filter(question_no == "Q44")



# creating function to break responses into n-grams
# survery_df = df containing survey responses; n_token = size of n-gram to use (ex: 2 = bigram)
get_n_gram <- function(survey_df, n_token) {
  
  # converting data to form for parsing
  data <- survey_df %>%
    mutate(response = str_replace_all(response, "_", " ")) # reformatting responses to remove any "_" from tidying process
  
  # creating list of unigrams
  if (n_token == 1) {
    
    n_gram_df <- data %>% 
      unnest_tokens(n_gram, response) %>% # breaking responses up into individual words
      filter(!n_gram %in% stop_words$word) # filtering out stop words from tokens
    
  # creating list of bigrams
  } else if (n_token == 2) {
    
    n_gram_df <- data %>% 
      unnest_tokens(n_gram, response, token = "ngrams", n = 2) %>% # breaking data up into bigrams
      separate(n_gram, c("word1", "word2"), sep = " ") %>% # separating bigrams into individual cols to filter out stop words
      filter(!word1 %in% stop_words$word) %>% # removing stop words from col1
      filter(!word2 %in% stop_words$word) %>% # removing stop words from col2
      unite(n_gram, word1, word2, sep = " ") # reuniting words to reform bigrams
  
  # creating list of trigrams
  } else if (n_token == 3) {
    
    n_gram_df <- data %>% 
      unnest_tokens(n_gram, response, token = "ngrams", n = 3) %>% # breaking data up into bigrams
      separate(n_gram, c("word1", "word2", "word3"), sep = " ") %>% # separating bigrams into individual cols
      filter(!word1 %in% stop_words$word) %>% # removing stop words from col1
      filter(!word2 %in% stop_words$word) %>% # removing stop words from col2
      filter(!word3 %in% stop_words$word) %>% # removing stop words from col3
      unite(n_gram, word1, word2, word3, sep = " ") # reuniting words to reform bigrams
    
  # making message for debugging
  } else {
    
    print("ERROR: n_token value invalid. Must be 1, 2, or 3.")
    
  }
  
  # outputting n-gram list
  return(n_gram_df)
  
}

# # testing get_n_gram() function to make sure it works
# get_n_gram(example_data2, 3)
# get_n_gram(example_data, 1)
# get_n_gram(example_data, 2)
# get_n_gram(example_data, 3)



# creating function to calculate term frequency for entire dataset
calc_tf <- function(survey_df, n_token) {
  
  # generating term frequency df
  tf_df <- get_n_gram(survey_df, n_token) %>% # generating list of n-grams
    count(n_gram, sort = TRUE) %>% # counting occurrences of each n-gram
    mutate(tf = n/sum(n)) %>% # calculating term frequencies based on total number of n-grams in dataset
    arrange(desc(tf)) # ordering output based on tf value
  
  # returning the output
  return(tf_df)
  
}

# # testing calc_tf() function
# calc_tf(example_data, 1)



# creating function to calculate tf-idf for stratified data
calc_tf_idf <- function(survey_df, n_token) {
  
  # creating df of tf-idf values for each n-gram
  tf_idf_df <- get_n_gram(survey_df, n_token) %>% # running user function to generate list of n-grams
    count(strat_id, n_gram, sort = TRUE) %>% # counting instances of each n-gram in each strat_id
    bind_tf_idf(n_gram, strat_id, n) %>% # calculating tf-idf for each n-gram
    arrange(desc(tf_idf)) # ordering output based on tf-idf value
  
  # returning output tf-idf values
  return(tf_idf_df)
  
}

# # testing calc_tf_idf() function
# calc_tf_idf(example_data, 2)






# creating plotting function for looking at tf-idf of the various strat_id's
plot_tf_idf <- function(tf_idf_df) {
  
  tf_idf_plot <- tf_idf_df %>% 
    group_by(strat_id) %>% # grouping for calculations
    top_n(20, tf_idf) %>% # pulls out top 20 possible entries for each strat_id based on tf-idf
    filter(tf_idf != min(tf_idf)) %>% # removing n-grams that have the smallest tf-idf value for each group (prevents over-plotting)
    ungroup() %>% # ungroup for plotting
    mutate(n_gram = reorder(n_gram, tf_idf)) %>% # ordering the data to be based on value for nicer looking plots
    ggplot(aes(n_gram, tf_idf, fill = strat_id)) + # setting the plotting conditions
    geom_col(show.legend = FALSE) + # graph will be a bar chart without a legend
    labs(x = NULL, y = "tf-idf") + # only need the tf-idf value label (n-grams will be other labels)
    facet_wrap(~strat_id, ncol = 2, scales = "free") + # making individual plots for each strat_id
    coord_flip() # turns the plot sideways
  
  return(tf_idf_plot)
  
}


test <- calc_tf_idf(example_data, 2)

plot_tf_idf(test)

# # looking at unigrams
# example_unigrams <- example_data %>% 
#   unnest_tokens(word, response) %>% # breaking responses up into individual words
#   anti_join(stop_words) %>% # removing any common words
#   count(strat_id, word, sort = TRUE) %>% # group words by strat_id, count instances of each word, then sort output
#   ungroup() # ungrouping from count()

# # calculating tf-idf of unigrams
# plot_example <- example_unigrams %>% 
#   bind_tf_idf(word, strat_id, n) %>% # calculating tf-idf of word while grouping on strat_id and using n as input
#   arrange(desc(tf_idf)) # arranging output

# # plotting tf-idf
# # NOTE: plotting top 15 but a lot of words have the same score so more than 15 are plotted for some strat_id's
# plot_example %>% 
#   group_by(strat_id) %>% # grouping for calculations
#   top_n(15, tf_idf) %>% # pulls out top 15 entries for each strat_id based on tf-idf
#   ungroup() %>% 
#   mutate(word = reorder(word, tf_idf)) %>% # ordering the data for plotting purposes
#   ggplot(aes(word, tf_idf, fill = strat_id)) + # setting the plotting conditions
#   geom_col(show.legend = FALSE) +
#   labs(x = NULL, y = "tf-idf") +
#   facet_wrap(~strat_id, ncol = 2, scales = "free") + # making individual plots for each strat_id
#   coord_flip() # turns the plot sideways


# # testing on bigrams
# # NOTE: need to find way to break ties for tf-idf (ideas: n, tf-idf of unigrams that form bigram)
# example_data %>% 
#   unnest_tokens(bigram, response, token = "ngrams", n = 2) %>% # breaking data up into bigrams
#   count(bigram, sort = TRUE) # looking at rough numbers
# 
# example_data %>% 
#   unnest_tokens(bigram, response, token = "ngrams", n = 2) %>% # breaking data up into bigrams
#   separate(bigram, c("word1", "word2"), sep = " ") %>% # separating bigrams into individual cols
#   filter(!word1 %in% stop_words$word) %>% # removing stop words from col1
#   filter(!word2 %in% stop_words$word) %>% # removing stop words from col2
#   count(word1, word2, sort = TRUE) # looking at new counts

# # taking out words associated with "postdoc" to focus responses
# # NOTE: potentially removes some important pieces of information
# my_stop_words <- add_row(stop_words, word = c("post", "doc", "postdoc", "postdocs", "pd", "pds"),
#                          lexicon = "custom")

# # making list of bigrams
# example_bigrams <- example_data %>% 
#   unnest_tokens(bigram, response, token = "ngrams", n = 2) %>% # breaking data up into bigrams
#   separate(bigram, c("word1", "word2"), sep = " ") %>% # separating bigrams into individual cols
#   filter(!word1 %in% stop_words$word) %>% # removing stop words from col1
#   filter(!word2 %in% stop_words$word) %>% # removing stop words from col2
#   unite(bigram, word1, word2, sep = " ") # reuniting words to reform bigrams
# 
# # calc tf-idf for each bigram
# bigram_tf_idf <- example_bigrams %>%
#   count(strat_id, bigram, sort = TRUE) %>% # counting individual bigrams
#   bind_tf_idf(bigram, strat_id, n) %>% # calc tf-idf
#   arrange(desc(tf_idf)) # ordering output based on tf-idf value

# # plotting the data
# bigram_tf_idf %>% 
#   group_by(strat_id) %>% # grouping for calculations
#   top_n(15, tf_idf) %>% # pulls out top 15 entries for each strat_id based on tf-idf
#   ungroup() %>% 
#   mutate(bigram = reorder(bigram, tf_idf)) %>% # ordering the data for plotting purposes
#   ggplot(aes(bigram, tf_idf, fill = strat_id)) + # setting the plotting conditions
#   geom_col(show.legend = FALSE) +
#   labs(x = NULL, y = "tf-idf") +
#   facet_wrap(~strat_id, ncol = 2, scales = "free") + # making individual plots for each strat_id
#   coord_flip() # turns the plot sideways


# # plotting the data v2
# # pulls out larger number of top responses and removes those with the minimum tf-idf value for each strat_id to prevent overplotting
# # temporary fix until the data is expanded
# bigram_tf_idf %>% 
#   group_by(strat_id) %>% # grouping for calculations
#   top_n(25, tf_idf) %>% # pulls out top 25 entries for each strat_id based on tf-idf
#   filter(tf_idf != min(tf_idf)) %>% 
#   ungroup() %>% 
#   mutate(bigram = reorder(bigram, tf_idf)) %>% # ordering the data for plotting purposes
#   ggplot(aes(bigram, tf_idf, fill = strat_id)) + # setting the plotting conditions
#   geom_col(show.legend = FALSE) +
#   labs(x = NULL, y = "tf-idf") +
#   facet_wrap(~strat_id, ncol = 2, scales = "free") + # making individual plots for each strat_id
#   coord_flip() # turns the plot sideways


# # trigrams
# example_trigrams <- example_data %>% 
#   unnest_tokens(trigram, response, token = "ngrams", n = 3) %>% # breaking data up into bigrams
#   separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% # separating bigrams into individual cols
#   filter(!word1 %in% stop_words$word) %>% # removing stop words from col1
#   filter(!word2 %in% stop_words$word) %>% # removing stop words from col2
#   filter(!word3 %in% stop_words$word) %>% 
#   unite(trigram, word1, word2, word3, sep = " ") # reuniting words to reform bigrams
# 
# # calc tf-idf for each trigram
# trigram_tf_idf <- example_trigrams %>%
#   count(strat_id, trigram) %>% # counting individual bigrams
#   bind_tf_idf(trigram, strat_id, n) %>% # calc tf-idf
#   arrange(desc(tf_idf)) # ordering output based on tf-idf value

# # plotting the data
# trigram_tf_idf %>% 
#   group_by(strat_id) %>% # grouping for calculations
#   top_n(15, tf_idf) %>% # pulls out top 15 entries for each strat_id based on tf-idf
#   ungroup() %>% 
#   mutate(trigram = reorder(trigram, tf_idf)) %>% # ordering the data for plotting purposes
#   ggplot(aes(trigram, tf_idf, fill = strat_id)) + # setting the plotting conditions
#   geom_col(show.legend = FALSE) +
#   labs(x = NULL, y = "tf-idf") +
#   facet_wrap(~strat_id, ncol = 2, scales = "free") + # making individual plots for each strat_id
#   coord_flip() # turns the plot sideways

# sentiment analysis ------------------------------------------------------

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



# testing AFINN based scoring
# basing code off of example from kaggle on text mining:
# https://www.kaggle.com/ambarish/seinfeld-text-mining-wordembeddings-modelling
top_30_sentiment <- typed_question_df %>% # calling df of typed question from survey data
  mutate(response = str_replace_all(response, "_", " ")) %>% # removing all "_" left from tidying dataset
  filter(question_no == "Q43") %>% # filtering to a specific questions
  # filter(question == "What_aspects_of_your_UM_postdoctoral_fellow_have_not_been_positive?") %>% 
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



# wordclouds --------------------------------------------------------------

top_30_sentiment %>% 
  wordcloud2(word, size = 0.5)


# notes -------------------------------------------------------------------

# # cmd for removing weird NA responses if desired
# filter(str_detect(response, regex(paste(paste0("\\b", c("n[/]a", "na"), "\\b"), collapse = "|"), ignore_case = TRUE)))
