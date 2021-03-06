# loading dependencies ----------------------------------------------------

# loads upstream scripts/variables if not already loaded
# checks for variables generated at end of previous script in pipeline and sources if not found
if (!exists("strat_data")){
  source("code/stratify_data.R")
}

# loading packages required for text analysis
library(tidytext) # text manipulation, used for tokenization and stop words
library(ggwordcloud) # geom_text_wordcloud()
library(grid) # required for table grobbing of response plots
library(gridExtra) # used to align response plot coordinates
library(ggpubr) # required to save gtable plots as ggplot items
library(viridis) # color scaling



# text analysis -----------------------------------------------------------

# NOTE: stop_words includes "no", "not", etc. which may be bad for some types of analyses

# setting the seed for all functions in script (could put in original tidy script to have propagate through all scripts)
my_seed <- 1

# creating custom stop_words df to filter out specific terms that may affect results
my_stop_words <- stop_words %>%
  add_row(word = c("post", "doc"), lexicon = "custom")



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
  
  # message for debugging
  print("Creating n-gram dfs.")
  
  # converting data to form for parsing
  data <- survey_df %>%
    mutate(response = str_replace_all(response, "_", " ")) # reformatting responses to remove any "_" from tidying process
  
  # creating list of unigrams
  if (n_token == 1) {
    
    n_gram_df <- data %>% 
      unnest_tokens(n_gram, response) %>% # breaking responses up into individual words
      filter(!n_gram %in% my_stop_words$word) %>% # filtering out stop words from tokens
      filter(!str_detect(n_gram, paste(c("\\b\\d+\\b", "\\d+th", "\\w\\.\\w"), collapse = "|"))) # removing numbers, dates, and n_grams with punctuation from list of n-grams
    
  # creating list of bigrams
  } else if (n_token == 2) {
    
    n_gram_df <- data %>% 
      unnest_tokens(n_gram, response, token = "ngrams", n = 2) %>% # breaking data up into bigrams
      separate(n_gram, c("word1", "word2"), sep = " ") %>% # separating bigrams into individual cols to filter out stop words
      filter(!word1 %in% my_stop_words$word) %>% # removing stop words from col1
      filter(!word2 %in% my_stop_words$word) %>% # removing stop words from col2
      filter(!str_detect(word1, paste(c("\\b\\d+\\b", "\\d+th", "\\w\\.\\w"), collapse = "|")) & # removing numbers, dates, and n_grams with punctuation from list of n-grams
               !str_detect(word2, paste(c("\\b\\d+\\b", "\\d+th", "\\w\\.\\w"), collapse = "|"))) %>% 
      unite(n_gram, word1, word2, sep = " ") # reuniting words to reform bigrams
  
  # creating list of trigrams
  } else if (n_token == 3) {
    
    n_gram_df <- data %>% 
      unnest_tokens(n_gram, response, token = "ngrams", n = 3) %>% # breaking data up into bigrams
      separate(n_gram, c("word1", "word2", "word3"), sep = " ") %>% # separating bigrams into individual cols
      filter(!word1 %in% my_stop_words$word) %>% # removing stop words from col1
      filter(!word2 %in% my_stop_words$word) %>% # removing stop words from col2
      filter(!word3 %in% my_stop_words$word) %>% # removing stop words from col3
      filter(!str_detect(word1, paste(c("\\b\\d+\\b", "\\d+th", "\\w\\.\\w"), collapse = "|")) & # removing numbers, dates, and n_grams with punctuation from list of n-grams
               !str_detect(word2, paste(c("\\b\\d+\\b", "\\d+th", "\\w\\.\\w"), collapse = "|")) &
               !str_detect(word3, paste(c("\\b\\d+\\b", "\\d+th", "\\w\\.\\w"), collapse = "|"))) %>% 
      unite(n_gram, word1, word2, word3, sep = " ") # reuniting words to reform bigrams
    
  # making message for debugging
  } else {
    
    print("ERROR: n_token value invalid. Must be 1, 2, or 3.")
    
  }
  
  # outputting n-gram list
  return(n_gram_df)
  
}



# combining the functions for calculating tf or tf-idf
# usage: freq_type = "tf" or "tf-idf", survey_df = df of survey data, n_token = words per n-gram (1:3), question_no_chr = as.character(question_no)
calc_n_gram_freq <- function(freq_type, survey_df, n_token, question_no_chr) {
  
  # generating n-gram dfs
  n_gram_df <- survey_df %>% 
    filter(question_no == question_no_chr) %>% # filtering based on desired question
    get_n_gram(n_token) # generating list of n-grams
 
  # message for debugging
  print("Calculating n-gram frequencies.")
  
  if (freq_type == "tf") {
    
    # message for debugging
    print("Calculating term frequency (tf).")
    
    # generating term frequency df
    freq_df <- n_gram_df %>% 
      count(n_gram, sort = TRUE) %>% # counting occurrences of each n-gram
      mutate(tf = n/sum(n)) %>% # calculating term frequencies based on total number of n-grams in dataset
      arrange(desc(tf)) #%>% # ordering output based on tf value
  
  } else if (freq_type == "tf-idf") {
    
    # message for debugging
    print("Calculating term frequency - inverse document frequency (tf-idf).")
    
    # creating df of tf-idf values for each n-gram
    freq_df <- n_gram_df %>% 
      count(strat_id, n_gram, sort = TRUE) %>% # counting instances of each n-gram in each strat_id
      bind_tf_idf(n_gram, strat_id, n) %>% # calculating tf-idf for each n-gram
      arrange(desc(tf_idf)) # ordering output based on tf-idf value
    
  } else {
    
    print("ERROR: Invalid frequency type")
    
  }
  
  # returning the output
  return(freq_df)
  
}

# # testing combined function
# calc_n_gram_freq(freq_type = "tf-idf", strat_data$college_school, 2, "Q44")



# creating function to generate dfs of top n-grams based on question and strat_id
# deals with issues of overplotting by subsampling n-grams in top_n list with minimum term frequency values
get_top_n_gram <- function(survey_df, question_no_chr, n_token = 1, freq_type = "tf", n_top = 10) {
  
  # generating df of frequencies
  freq_df <- calc_n_gram_freq(freq_type, survey_df, n_token, question_no_chr) %>% # calcs term frequencies for n-grams
    mutate(question_no = question_no_chr, # adding col with question number for plotting
           sorted_question_no = survey_df %>% filter(question_no == question_no_chr) %>% pull(sorted_question_no) %>% unique(),
           question = question_data %>% filter(question_no == question_no_chr) %>% pull(question) %>% unique()) %>% # adds col with question text back in
    filter(!str_detect(n_gram, "\\bNA\\b")) # filtering out lines containing NA as a token/part of an n-gram (focuses data)
  
  # message for debugging
  print("Generating df of top n-grams.")
  
  if (freq_type == "tf") {
    
    # message for debugging
    print(paste0("Generating df of top ", paste(n_top), " n-grams based on tf."))
    
    # making df of top n-grams for each dataset
    tf_top <- freq_df %>%
      top_n(n_top, tf) # pulls out top n entries based on tf
    
    # if there are less unique n-grams than the desired number of top n-grams
    # (i.e. want top 10 n-grams by freq but only have 7), nothing is done and only those are plotted.
    # if there are exactly the correct number of n-grams as desired, nothing is done and the data is plotted as is
    if (nrow(tf_top) <= n_top) {
      
      top_n_grams <- tf_top %>% 
        mutate(tf_scale = tf*(8/max(tf))) # creating a col for plot scaling by setting max tf value
    
    # if there are too many answers (more than one n-gram with same term freq), n-grams with the smallest freq value in the top n-gram
    # df are subsampled to give only the desired number of n-grams thus preventing overplotting
    } else {
      
      # subsetting top n-gram df to be only n-grams with freq != min freq to prevent overplotting
      tf_top_max <- tf_top %>%
        filter(tf != min(tf)) # removing n-grams that have the smallest tf value for each group (prevents over-plotting)
      
      # subsetting top n-gram df to be only n-grams with freq = min freq for subsampling and rejoining with the max df to plot desired number of n-grams
      tf_top_min <- tf_top %>%
        filter(tf == min(tf)) # removing n-grams that have the largest tf values for each group
      
      # subsampling tf_top_min df and adding back to tf_top_max df to give consistent number of n-grams per strat_id for plotting
      set.seed(my_seed) # setting the seed for randomization during subsampling
      
      top_n_grams <- tf_top_max %>%
        bind_rows(tf_top_min %>% sample_n(size = n_top - nrow(tf_top_max), replace = FALSE)) %>% # subsampling min df and binding to max df
        mutate(tf_scale = tf*(8/max(tf))) # creating a col for plot scaling by setting max tf value
      
    }
    
  } else if (freq_type == "tf-idf") {
    
    # message for debugging
    print(paste0("Generating df of top ", paste(n_top), " n-grams based on tf-idf."))
    
    # making df of top n-grams for each dataset
    tf_idf_top <- freq_df %>%
      group_by(strat_id) %>% # grouping for calculations
      top_n(n_top, tf_idf) # pulls out top n number of n-grams based on freq values
    
    # creating df of strat_ids with more n-grams than specified by n_top (would result in overplotting)
    tf_idf_top_over <- tf_idf_top %>% 
      filter(n() > n_top) # removing any rows with less than or the same number of n-grams specified by n_top
    
    # for df with more n-grams than desired (n_top), removes any n-gram with the minimum tf_idf value
    tf_idf_top_over_max <- tf_idf_top_over %>%
      filter(tf_idf != min(tf_idf)) # removing n-grams that have the smallest tf-idf value for each group (prevents over-plotting)
    
    # for df with more n-grams than desired, generating a subset df containing only n-grams with the minimum tf_idf value
    # will be subsampled and added back to generate a df with the desired number of n-grams for each strat_id
    tf_idf_top_over_min <- tf_idf_top_over %>%
      filter(tf_idf == min(tf_idf)) # removing n-grams that have the largest tf-idf values for each group
    
    # creating count table for n-grams in each strat_id, any strat_ids that may be missing are added back in and set to 0
    # will be used to set degree of subsampling of tf_idf_top_over_min df
    max_counts <- tf_idf_top_over_max %>%
      summarize(n_strat = n()) %>% # counting number of n-grams per strat_id
      rename(strat_id_chr = strat_id) %>% # renaming col for use with retrieve_rows() later
      when(nrow(.) < nrow(count(tf_idf_top)) ~ add_row(., strat_id_chr = setdiff(count(tf_idf_top)[["strat_id"]], .[["strat_id_chr"]]), n_strat = 0), # if there are less strat_ids than in parent df, they are added back with values of 0
           TRUE ~ .) # otherwise the df is left as is
    
    # creating function to randomly subsample n number of rows from each strat_id group
    # used to build top_n df up to a desired row number when overplotting is an issue due to multiple rows having same term freq value
    retrieve_rows  <- function(df, strat_id_chr, n_top, n_strat) {
      
      set.seed(my_seed) # setting seed for randomized subsampling
      
      data <- df %>% 
        filter(strat_id == strat_id_chr) %>% # filtering based on strat_id
        sample_n(size = n_top - n_strat, replace = FALSE) # subsampling df
      
      return(data) # returning output
      
    }
    
    # subsampling tf_idf_top_over_min df and adding back to tf_idf_top_over_max df to give consistent number of n-grams per strat_id for plotting
    top_n_grams <- tf_idf_top_over_max %>%
      bind_rows(tf_idf_top %>% filter(n() <= n_top), # adding data from strat_ids with less than or equal to the number of desired n-grams (n_top) because no subsampling, etc. needed
                pmap_df(df = tf_idf_top_over_min, n_top = n_top, .l = max_counts, .f = retrieve_rows)) %>%  # subsampling min df and binding to max df
      mutate(tf_idf_scale = tf_idf*(8/max(tf_idf))) %>% # creating a col for plot scaling by setting max tf-idf value for each strat_id to 8 so all strat_ids can have the same range
      ungroup()
    
  }
  
  # returning df of top_n n-grams
  return(top_n_grams)
  
}

# # testing the combined function
# get_top_n_gram(survey_df = strat_data$college_school, question_no_chr = "Q23", freq_type = "tf-idf", n_token = 2, n_top = 15)



# creating function to plot all of the unstratified dataset using pmap
get_all_top_n_grams <- function(survey_df, question_no_chr_list, n_token, freq_type, n_top) {
  
  # debugging messages
  if (freq_type == "tf") {
    
    print(paste0("Plotting all ", deparse(substitute(survey_df)), " tf wordclouds."))
    
  } else if (freq_type == "tf-idf") {
    
    print(paste0("Plotting all ", deparse(substitute(survey_df)), " tf-idf wordclouds."))
    
  }
  
  # pulling sorted question list numbers for use in labeling output plots
  sorted_question_no_chr_list <- survey_df %>% 
    filter(question_no %in% question_no_chr_list) %>% 
    pull(sorted_question_no) %>% 
    unique()
  
  # creating df of arguments for use with pmap
  arguments <- data_frame(survey_df = list(survey_df),
                          question_no_chr = question_no_chr_list)
  
  # mapping over list of arguments in arguments df
  data <- pmap(arguments, get_top_n_gram, n_token = n_token, freq_type = freq_type, n_top = n_top) %>%
    set_names(sorted_question_no_chr_list)
  
  # outputting results
  return(data)
  
}

# generating all top n-grams based on tf
all_top_tf <- get_all_top_n_grams(survey_df = question_data, question_no_chr_list = typed_question_list,
                                   n_token = 2, freq_type = "tf", n_top = 15)

# generating all top n-grams based on tf-idf
all_top_tf_idf <- map(strat_data, get_all_top_n_grams, question_no_chr_list = typed_question_list,
                      n_token = 2, freq_type = "tf-idf", n_top = 15)


# # verifying output
# all_top_tf$Q44
# all_top_tf_idf$gender$Q44



# creating function for plotting wordclouds of specific questions based on term frequency and number of n-grams desired
plot_top_wordcloud <- function(survey_df, question_no_chr, n_token = 1, freq_type = "tf", n_top = 10) {
  
  # creating df of top n-grams for each question
  top_n_grams <- get_top_n_gram(survey_df, question_no_chr, n_token, freq_type, n_top)
  
  # message for debugging
  print("Plotting wordclouds of top n-grams.")
  
  # setting conversion for geom_text size to font point sizes
  text_size_conv <- 1/72*25.4
  
  if (freq_type == "tf") {
    
    # debugging message
    print(paste0("Plotting top n-grams for ", question_no_chr, " based on tf."))
    
    # creating the wordcloud
    n_gram_wordcloud <- top_n_grams %>%
      ggplot(aes(label = n_gram, size = tf_scale, color = tf_scale)) + # specifying data to be plotted
      geom_text_wordcloud(eccentricity = 2, # roundness of the wordcloud
                          grid_size = 6, grid_margin = 2, # spacing between terms in cloud
                          family = "Helvetica") + # altering font characteristics (does not inherit changes from theme())
      scale_color_viridis_c(begin = 0, end = 0.75) +
      # scale_color_gradient(low = "#00274c", high = "#886b01") + # changing color of n-grams in plot
      scale_size(range = c(8*text_size_conv, 18*text_size_conv)) + # setting the lower and upper bounds of text point sizes
      labs(tag = unique(top_n_grams$sorted_question_no),
           title = str_replace_all(unique(top_n_grams$question), c("_" = " ", "," = ", "))) + # adding the question text as the plot title
      theme_minimal() + # getting rid of all theme background
      theme(text = element_text(family = "Helvetica"),
            plot.title = element_text(hjust = 0.5, size = 9, face = "bold"),  # centers the plot title and alters font size
            strip.background = element_rect(fill = NULL, color = "white"),
            strip.text = element_text(size = 9, face = "bold"),
            axis.title = element_text(size = 9),
            plot.tag = element_text(hjust = 0, size = 12),
            plot.tag.position = c(0.015,1))
    
    grob_plot <- ggplotGrob(n_gram_wordcloud)
    
    grob_plot$heights[4] <- unit(-0.25, "cm")
    
    n_gram_wordcloud <- as_ggplot(arrangeGrob(grob_plot))
    
  } else if (freq_type == "tf-idf") {
    
    # debugging message
    print(paste0("Plotting top n-grams for ", question_no_chr, " based on tf-idf."))
    
    # creating the wordcloud
    n_gram_wordcloud <- top_n_grams %>%
      ggplot(aes(label = n_gram, size = tf_idf_scale, color = tf_idf_scale)) + # specifying data to be plotted
      geom_text_wordcloud(eccentricity = 1, # roundness of the wordcloud
                          grid_size = 6, grid_margin = 2, # spacing between terms in cloud
                          # fontface = "bold",
                          family = "Helvetica") + # altering font characteristics (does not inherit changes from theme())
      scale_color_viridis_c(begin = 0, end = 0.75) +
      # scale_color_gradient(low = "#00274c", high = "#886b01") + # changing color of n-grams in plot
      scale_size(range = c(8*text_size_conv, 18*text_size_conv)) + # setting the lower and upper bounds of text point sizes
      labs(tag = unique(top_n_grams$sorted_question_no),
           title = str_replace_all(unique(top_n_grams$question), c("_" = " ", "," = ", "))) + # adding the question text as the plot title
      facet_wrap(~str_replace_all(strat_id, "_", " "), ncol = 2, scales = "free") + # making individual plots for each strat_id
      theme_minimal() + # getting rid of all theme background
      theme(text = element_text(family = "Helvetica"),
            plot.title = element_text(hjust = 0.5, size = 9, face = "bold"),  # centers the plot title and alters font size
            strip.background = element_rect(fill = NULL, color = "white"),
            strip.text = element_text(size = 9, face = "bold"),
            axis.title = element_text(size = 9),
            plot.tag = element_text(hjust = 0, size = 12),
            plot.tag.position = c(0.015,1),
            panel.spacing = unit(2, "lines"))
    
  }
  
  # outputting results
  return(n_gram_wordcloud)
  
}

# plot_top_wordcloud(strat_data$residency, "Q44", 2, freq_type = "tf", n_top = 15)
# plot_top_wordcloud(strat_data$college_school, "Q44", 2, freq_type = "tf-idf", n_top = 15)



# creating function to plot all of the unstratified dataset using pmap
plot_all_wordclouds <- function(survey_df, question_no_chr_list, n_token, freq_type, n_top) {
  
  # debugging messages
  if (freq_type == "tf") {
    
    print(paste0("Plotting all ", deparse(substitute(survey_df)), " tf wordclouds."))
    
  } else if (freq_type == "tf-idf") {
    
    print(paste0("Plotting all ", deparse(substitute(survey_df)), " tf-idf wordclouds."))
    
  }

  # pulling sorted question list numbers for use in labeling output plots
  sorted_question_no_chr_list <- survey_df %>% 
    filter(question_no %in% question_no_chr_list) %>% 
    pull(sorted_question_no) %>% 
    unique()
  
  # creating df of arguments for use with pmap
  arguments <- data_frame(survey_df = list(survey_df),
                          question_no_chr = question_no_chr_list)
  
  # mapping over list of arguments in arguments df
  plots <- pmap(arguments, plot_top_wordcloud, n_token = n_token, freq_type = freq_type, n_top = n_top) %>%
    set_names(sorted_question_no_chr_list)
  
  # outputting results
  return(plots)
  
}

# # testing plot_all_wordclouds() function
# plot_all_wordclouds(example_data, typed_question_list, 2, "tf-idf", 15)

# generating all tf wordclouds
all_tf_wordclouds <- plot_all_wordclouds(survey_df = question_data, question_no_chr_list = typed_question_list,
                         n_token = 2, freq_type = "tf", n_top = 15)

# generating all tf-idf wordclouds
all_tf_idf_wordclouds <- map(strat_data, plot_all_wordclouds, question_no_chr_list = typed_question_list,
                             n_token = 2, freq_type = "tf-idf", n_top = 15)

# # verifying the outputs
# all_tf_wordclouds$Q44
# all_tf_idf_wordclouds$college_school$Q45



# creating function to save plots of stratified data using dynamic scaling of heights based on number of unique strat_ids per plot
save_wordclouds <- function(freq_type, question_no_chr, category = NULL) {
  
  if (freq_type == "tf") {
    
    # debugging message
    print("Saving tf wordclouds.")
    
    # changing question number to be based on sorted numbers instead for use in filename after saving
    sorted_question_no_chr <- question_data %>% 
      filter(question_no == question_no_chr) %>% # matches question_no to find sorted_question_no
      pull(sorted_question_no) %>% # extracts value(s)
      unique() # finds unique sorted question number
    
    ggsave(plot = all_tf_wordclouds[[sorted_question_no_chr]], filename = paste0("results/", category, "/", paste(category, sorted_question_no_chr, sep = "_"), ".png"), # saving the plots as png
           device = "png", width = 15, height = 2.75, dpi = 300) # specifying dimensions of plots found by trial and error
    
  } else if (freq_type == "tf-idf") {
    
    # debugging message
    print("Saving tf-idf wordclouds.")
    
    # changing question number to be based on sorted numbers instead for use in filename after saving
    sorted_question_no_chr <- strat_data[[category]] %>% 
      filter(question_no == question_no_chr) %>% # matches question_no to find sorted_question_no
      pull(sorted_question_no) %>% # extracts value(s)
      unique() # finds unique sorted question number
    
    strat_ids <- all_top_tf_idf[[category]][[sorted_question_no_chr]] %>%  # pulling out the list of strat_ids for each plot to properly scale the plot height
      pull(strat_id) %>% # compiling list of strat_ids
      unique() # finding only unique ids
    
    strat_id_no <- ceiling(length(strat_ids)) + ceiling(length(strat_ids)) %% 2 # counting the number of unique strat_ids for plot scaling
    
    ggsave(plot = all_tf_idf_wordclouds[[category]][[sorted_question_no_chr]], filename = paste0("results/", category, "/", paste(category, sorted_question_no_chr, sep = "_"), ".png"), # saving the plots as png
           device = "png", width = 15, height = 0.75+(1.25*strat_id_no), dpi = 300) # specifying dimensions of plots found by trial and error
    
  }
  
}

# # testing save_wordclouds
# save_wordclouds("tf", "Q51", "all")
# save_wordclouds("tf-idf", "Q44", category = "college_school")



# setting up mapping function to loop through and save all plots
save_all_wordclouds <- function(freq_type, question_no_chr_list, category_list) {
  
  # debugging messages
  if (freq_type == "tf") {
    
    print("Saving all tf wordclouds.")
    
  } else if (freq_type == "tf-idf") {
    
    print("Saving all tf-idf wordclouds.")
    
  }
  
  # setting up df of input values for pmap()
  arguments <- data_frame(category = rep(category_list, each = length(typed_question_list)),
                            question_no_chr = rep(question_no_chr_list, times = length(category_list)))

  # iterating over input value df
  pmap(arguments, save_wordclouds, freq_type = freq_type)

}

# # testing save_all_wordclouds()
# save_all_wordclouds("tf-idf", typed_question_list, "gender")



# saving all tf wordclouds
save_all_wordclouds("tf", typed_question_list, "all")

# saving all tf-idf wordclouds
save_all_wordclouds("tf-idf", typed_question_list, names(strat_list))



# sentiment analysis ------------------------------------------------------

# # deciding which classification system to use for sentiment analysis
# # afinn has best score distribution (ex: negative scores align well with negative opinions) and better range (-5 to 5) but fewer words in lexicon (2476 words)
# # bing has second best score distribution and significantly more words in lexicon (6788 words) but scores are either "pos" or "neg"
# get_sentiments("bing") %>% 
#   select(sentiment) %>% 
#   unique()
# get_sentiments("afinn") %>% 
#   select(score) %>% 
#   unique() %>% 
#   pull() %>% 
#   sort()
# 
# 
# 
# # testing AFINN based scoring
# # basing code off of example from kaggle on text mining:
# # https://www.kaggle.com/ambarish/seinfeld-text-mining-wordembeddings-modelling
# top_30_sentiment <- typed_question_df %>% # calling df of typed question from survey data
#   mutate(response = str_replace_all(response, "_", " ")) %>% # removing all "_" left from tidying dataset
#   filter(question_no == "Q43") %>% # filtering to a specific questions
#   # filter(question == "What_aspects_of_your_UM_postdoctoral_fellow_have_not_been_positive?") %>% 
#   unnest_tokens(word, response) %>% # breaks apart responses into individual words
#   filter(!is.na(word)) %>% # removes any blank rows
#   inner_join(get_sentiments("afinn"), by = "word") %>% # adds AFINN scores to words from dataset if present in each
#   group_by(word) %>% # groups based on word
#   summarize(n = n(), # counts number of occurrences
#             cum_score = sum(score)) %>% # calculates (score * number of occurrences) of each word to find relative contribution to total score
#   mutate(percent_score = (cum_score / sum(cum_score)) * 100) %>%  # calculates percent of total score attributed to each word
#   arrange(desc(percent_score)) %>% # arranges words based on percent score (makes human readable but not preserved for plotting)
#   mutate(word = reorder(word, percent_score)) %>% # reorders the words for use in plotting
#   top_n(30, abs(cum_score)) # selects top number of words based on absolute score for contribution
# 
# # plotting based on sentiment scores
# top_30_sentiment %>% 
#   ggplot(aes(word, percent_score, fill = percent_score > 0)) + # colors based on score
#   geom_col(show.legend = TRUE) + # makes bar plot
#   coord_flip() # turns plot on its side
# 
# # NOTE: AFINN is score based so quantitative scores can be calculated/plotted, bing is classification based so is only binary pos or neg
# 
# typed_question_df %>% 
#   mutate(response = str_replace_all(response, "_", " ")) %>% 
#   unnest_tokens(word, response) %>% 
#   filter(!is.na(word)) %>% 
#   inner_join(get_sentiments("nrc"), by = "word")



# notes -------------------------------------------------------------------


