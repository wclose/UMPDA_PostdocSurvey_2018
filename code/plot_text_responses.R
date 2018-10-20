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
source("code/stratify_data.R")


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
# 
# 
# typed_question_df %>% 
#   filter(question_no == "Q51" & !is.na(response)) %>%
#   createWordCloud()



# tf-idf ------------------------------------------------------------------

# NOTE: Because of the size of the dataset at present (2018), tf-idf is limited because there aren't enough repeats of n-grams
# so several n-grams all have the same tf-idf value which makes plotting difficult. Could be fixed by repeating the survey and
# expanding the available data but limits yearly evaluations

# Term Frequency - Inverse Document Frequency (aka most important words)
# need to compare questions that are inverse of each other
# works similar to random forest modelling by finding words that are common but to only a subset of the collection of documents



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



# creating function to calculate term frequency for entire dataset
calc_tf <- function(survey_df, n_token, question_no_chr) {
  
  # generating term frequency df
  tf_df <- survey_df %>% 
    filter(question_no == question_no_chr) %>% # filtering based on desired question
    get_n_gram(n_token) %>% # generating list of n-grams
    count(n_gram, sort = TRUE) %>% # counting occurrences of each n-gram
    mutate(tf = n/sum(n)) %>% # calculating term frequencies based on total number of n-grams in dataset
    arrange(desc(tf)) #%>% # ordering output based on tf value
    #mutate(question = tidy_survey_data %>% filter(question_no == question_no_chr) %>% pull(question) %>% unique()) # adds col with question text back in
  
  # returning the output
  return(tf_df)
  
}



# creating function to calculate tf-idf for stratified data
calc_tf_idf <- function(survey_df, n_token, question_no_chr) {
  
  # creating df of tf-idf values for each n-gram
  tf_idf_df <- survey_df %>% 
    filter(question_no == question_no_chr) %>% # filtering based on desired question
    get_n_gram(n_token) %>% # running user function to generate list of n-grams
    count(strat_id, n_gram, sort = TRUE) %>% # counting instances of each n-gram in each strat_id
    bind_tf_idf(n_gram, strat_id, n) %>% # calculating tf-idf for each n-gram
    arrange(desc(tf_idf)) #%>% # ordering output based on tf-idf value
    #mutate(question = tidy_survey_data %>% filter(question_no == question_no_chr) %>% pull(question) %>% unique()) # adds col with question text back in
  
  # returning output tf-idf values
  return(tf_idf_df)
  
}



# NOTE: need to change color scheme
# NOTE: combine calc function and plot function then take out question col from calc function and only have in plot

# creating plotting function for looking at tf for each question/dataset
plot_tf <- function(tf_df) {
  
  tf_data <- tf_df %>% 
    top_n(20, tf) %>% # pulls out top 20 entries based on tf
    filter(tf != min(tf)) %>% # removing n-grams that have the smallest tf value for each group (prevents over-plotting)
    filter(!str_detect(n_gram, "\\bNA\\b")) %>% # filtering out lines containing NA as a token 
    mutate(n_gram = reorder(n_gram, tf)) # ordering the data to be based on value for nicer looking plots
  
  response_no <- length(unique(tf_data$n_gram)) # calculating the number of unique responses for aspect ratio scaling
  
  aspect <- 0.2*response_no/7 # scales the aspect ratio to standardize appearance of bars after setting consistent width w/ grobbing
  
  tf_plot <- tf_data %>% 
    ggplot(aes(n_gram, tf)) + # setting the plotting conditions
    geom_col(show.legend = FALSE, fill = "red", color = "black") + # graph will be a bar chart without a legend
    labs(title = str_wrap(str_replace_all(unique(tf_data$question), "_", " "), width = 100),
         x = NULL, y = "tf") + # only need the tf value label (n-grams will be other labels)
    coord_flip(expand = FALSE) + # turns the plot sideways
    theme(plot.title = element_text(size = 10, hjust = 0.5), # sets size of chart title and centers over plot
          axis.line = element_line(size = 0.5, color = "black"), # formatting axis lines as desired
          axis.title = element_text(size = 10), # making all chart titles a consistent size
          axis.title.x = element_text(margin = margin(10,0,0,0)), # adding space between x axis title and axis labels
          axis.text = element_text(size = 8),
          plot.margin = margin(20,20,20,20), # giving plot a bit of padding on edges in case something is plotted out of bounds
          panel.background = element_rect(fill = "white"), # making panels have white background
          # formatting plots to have a consistent size
          aspect.ratio = aspect) # making size of bars compared to plot consistent
  
  grob_table <- ggplotGrob(tf_plot) # creates a gtable of plot features
  
  grob_table$widths[4] <- unit(6, "cm") # sets alignment of y axis in chart area thereby aligning all plots generated with this script (moves chart to the right)
  grob_table$widths[6] <- unit(6, "cm") # used for alignment (moves chart to the left)

  grobbed_plot <- as_ggplot(arrangeGrob(grob_table)) # recreating the plots with updated coordinates and saving as a ggplot item
  
  return(grobbed_plot)
  
}

# str_wrap("test two", width = 3)

# testing
map(test2, plot_tf)





# creating plotting function for looking at tf-idf of the various strat_id's
plot_tf_idf <- function(tf_idf_df) {
  
  tf_idf_plot <- tf_idf_df %>% 
    group_by(strat_id) %>% # grouping for calculations
    top_n(20, tf_idf) %>% # pulls out top 20 possible entries for each strat_id based on tf-idf
    filter(tf_idf != min(tf_idf)) %>% # removing n-grams that have the smallest tf-idf value for each group (prevents over-plotting)
    filter(!str_detect(n_gram, "\\bNA\\b")) %>% # filtering out lines containing NA as a token 
    ungroup() %>% # ungroup for plotting
    mutate(n_gram = reorder(n_gram, tf_idf)) %>% # ordering the data to be based on value for nicer looking plots
    ggplot(aes(n_gram, tf_idf, fill = strat_id)) + # setting the plotting conditions
    geom_col(show.legend = FALSE) + # graph will be a bar chart without a legend
    labs(x = NULL, y = "tf-idf") + # only need the tf-idf value label (n-grams will be other labels)
    facet_wrap(~strat_id, ncol = 2, scales = "free") + # making individual plots for each strat_id
    coord_flip(expand = FALSE) # turns the plot sideways
  
  return(tf_idf_plot)
  
}



library(ggwordcloud)
library(RColorBrewer)

plot_wordcloud_tf <- function(survey_df, n_token, question_no_chr) {
  
  tf_df <- calc_tf(survey_df, n_token, question_no_chr) %>% 
    mutate(question = tidy_survey_data %>% filter(question_no == question_no_chr) %>% pull(question) %>% unique()) # adds col with question text back in
  
  tf_top <- tf_df %>% 
    top_n(20, tf) %>% # pulls out top 20 entries based on tf
    filter(tf != min(tf)) %>% # removing n-grams that have the smallest tf value for each group (prevents over-plotting)
    filter(!str_detect(n_gram, "\\bNA\\b")) %>% # filtering out lines containing NA as a token 
    mutate(n_gram = reorder(n_gram, tf)) # ordering the data to be based on value for nicer looking plots
  
  tf_wordcloud <- tf_top %>%
    ggplot(aes(label = n_gram, size = tf)) +
    geom_text_wordcloud(aes(color = colorRampPalette(brewer.pal(11,"Spectral"))(16)),
                        eccentricity = 1, grid_size = 4, grid_margin = 3,
                        fontface = "bold", family = "Times New Roman") +
    scale_size(range = c(5, 15)) +
    labs(title = str_replace_all(unique(tf_top$question), "_", " ")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  
  return(tf_wordcloud)
  
}

plot_wordcloud_tf(example_data, 2, "Q42")

map(test2, plot_wordcloud_tf2)



# plot_wordcloud_tf <- function(tf_df) {
#   
#   tf_data <- tf_df %>% 
#     top_n(20, tf) %>% # pulls out top 20 entries based on tf
#     filter(tf != min(tf)) %>% # removing n-grams that have the smallest tf value for each group (prevents over-plotting)
#     filter(!str_detect(n_gram, "\\bNA\\b")) %>% # filtering out lines containing NA as a token 
#     mutate(n_gram = reorder(n_gram, tf)) # ordering the data to be based on value for nicer looking plots
#   
#   tf_wordcloud <- tf_data %>%
#     ggplot(aes(label = n_gram, size = tf)) +
#     geom_text_wordcloud(eccentricity = 1, grid_size = 4, grid_margin = 3, fontface = "bold", family = "Times New Roman") +
#     scale_size(range = c(5, 15)) +
#     labs(title = "Test") +
#     theme_minimal()
#   
#   return(tf_wordcloud)
#   
# }
# 
# plot_wordcloud_tf2(test2$Q43)
# 
# map(test2, plot_wordcloud_tf2)




plot_wordcloud_tf_idf <- function(tf_idf_df) {
  
  tf_idf_data <- tf_idf_df %>% 
    group_by(strat_id) %>% # grouping for calculations
    top_n(20, tf_idf) %>% # pulls out top 20 possible entries for each strat_id based on tf-idf
    filter(tf_idf != min(tf_idf)) %>% # removing n-grams that have the smallest tf-idf value for each group (prevents over-plotting)
    filter(!str_detect(n_gram, "\\bNA\\b")) %>% # filtering out lines containing NA as a token 
    ungroup() %>% # ungroup for plotting
    mutate(n_gram = reorder(n_gram, tf_idf)) 
  
}



### testing things ###

# creating test dataset
example_data <- strat_data$language %>% 
  filter(question_no %in% typed_question_list) #%>% # filtering to only contain text based responses
  #mutate(response = str_replace_all(response, "_", " ")) #%>% # removing extra symbols from tidying
# filter(!is.na(response) & !str_detect(response, "\\bn[/]a\\b")) # removing any weird "NA" type responses people input

# example_data2 <- tidy_survey_data %>% 
#   filter(question_no %in% typed_question_list) %>% 
#   filter(question_no == "Q44")

# # testing get_n_gram() function to make sure it works
# get_n_gram(example_data2, 3)
# get_n_gram(example_data, 1)
# get_n_gram(example_data, 2)
# get_n_gram(example_data, 3)

# # testing calc_tf() function
# calc_tf(example_data, 1)

# # testing calc_tf_idf() function
# calc_tf_idf(example_data, 2)

# # testing plotting
# test_question <- "Q44"
# strat_data$language %>%
#   filter(question_no == test_question)
# test <- calc_tf_idf(example_data, 2, test_question)
# test %>%
#   group_by(strat_id) %>% # grouping for calculations
#   top_n(20, tf_idf) %>% # pulls out top 20 possible entries for each strat_id based on tf-idf
#   filter(tf_idf != min(tf_idf)) %>% # removing n-grams that have the smallest tf-idf value for each group (prevents over-plotting)
#   filter(!str_detect(n_gram, "\\bNA\\b")) %>% # filtering out lines containing NA as a token
#   ungroup() %>% # ungroup for plotting
#   mutate(n_gram = reorder(n_gram, tf_idf))
# plot_tf_idf(test)



### running it ###

# calc_tf <- function(survey_df, n_token, question_no_chr) 
# setting up mapping function to loop through all plots and question numbers of stratified data
# save_all_strat_plots <- function(plot_list, question_no_chr_list, category) {
#   arguments <- data_frame(plot_name = plot_list,
#                           question_no_chr = question_no_chr_list)
#   pmap(arguments, save_strat_plots, category = category)
# }

test_function <- function(survey_df, n_token, question_no_chr_list) {
  arguments <- data_frame(survey_df = list(survey_df),
                          n_token = n_token,
                          question_no_chr = question_no_chr_list)
  data <- pmap(arguments, calc_tf) %>% 
    set_names(question_no_chr_list)
  return(data)
}

test2 <- test_function(example_data, 2, typed_question_list)

map(test2, plot_tf)












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
