# loading dependencies ----------------------------------------------------

# loading tidyverse and dataset
source("code/calc_response_stats.R")



# misc functions ----------------------------------------------------------

# creating a function to wrap the text of long titles by setting the max number of chrs per line (width = ##)
# ex: wrapper("Super long title", width = 50)
text_wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}



# multiple choice plots ---------------------------------------------------

# NOTE: Q22 responses need to be broken up and retabulated before graphing

# making function to generate plots for multi choice questions
make_response_plot <- function(df) { # df = properly formatted data frame with data
  freq_plot <- df %>% # assigning the output
    ggplot() +
    geom_bar(aes(x = response, y = percent_freq, fill = response, group = subquestion_no), # adding bars
             color = "black", show.legend = F, stat = "identity", width = 0.75) +
    geom_text(aes(x = response, y = percent_freq, label = paste0(round(percent_freq, 1), "%")), vjust = -0.5) + # adding response freq over bars
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # formatting y axis
    scale_x_discrete(labels = c(str_replace_all(df$response, "_", " "))) + # reformatting axis labels to look nice
    labs(title = text_wrapper(str_replace_all(unique(df$question), "_", " "), width = 60), # reformatting remainging labels to look nice
         y = "Proportion of Postdoctoral Respondents (%)",
         x = "Responses") +
    theme_classic() + # changing color scheme, etc.
    theme(plot.title = element_text(hjust = 0.5)) # centering the title over the plot
  return(freq_plot) # returning the plot to environment
}

# testing response plotting function
make_response_plot(response_freq$Q6)

# using map() to generate plots for each data frame contained within response_freq list
response_plots <- map(response_freq, make_response_plot) # don't need to specify names b/c preserves names from response_freq

# verifying indexing ability and plot appearance
response_plots$Q12



# making function to generate plots for multi choice questions
make_stack_response_plot <- function(df) { # df = properly formatted data frame with data
  freq_plot <- df %>% # assigning the output
    ggplot() +
    geom_bar(aes(x = question_no, y = percent_freq, fill = response), # adding bars
             color = "black", show.legend = T, stat = "identity", width = 0.75, position = "stack") +
    #geom_text(aes(x = response, y = percent_freq, label = paste0(round(percent_freq, 1), "%")), vjust = -0.5) + # adding response freq over bars
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # formatting y axis
    #scale_x_discrete(labels = c(str_replace_all(df$question, "_", " "))) + # reformatting axis labels to look nice
    labs(#title = text_wrapper(str_replace_all(unique(df$question), "_", " "), width = 60), # reformatting remainging labels to look nice
         y = "Proportion of Postdoctoral Respondents (%)",
         x = "Responses") +
    #coord_flip() +
    theme_classic() + # changing color scheme, etc.
    theme(plot.title = element_text(hjust = 0.5)) + # centering the title over the plot
    facet_wrap( ~ question)
  return(freq_plot) # returning the plot to environment
}

# make_stack_response_plot(response_freq$Q35)
# 
# # toy data set
# test <- strat_response_freq$language %>% 
#   filter(question_no == "Q35" & !is.na(response) & response != "Prefer_not_to_answer")
# 
# test_2 <- strat_response_freq$language %>% 
#   filter(question_no == "Q14" & !is.na(response) & response != "Prefer_not_to_answer")
# 
# # basic layout for the plots
# test %>% 
#   ggplot(aes(x = strat_id, y = percent_freq, fill = response)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~ subquestion_no)

# creating a plotting function that makes new graph for each question
test_plot_function <- function(df, question_no_chr) {
  df %>% 
    filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer") %>% 
    ggplot(aes(x = strat_id, y = percent_freq, fill = response)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ subquestion_no)
}

# testing it out
test_plot_function(strat_response_freq$language, "Q35")

# making function to allow use of pmap to iterate through each plot needing to be made
# NOTE: can make list of input questions as a new variable to be populated when ran (makes it more DRY)
test_function <- function(list_strat_response_freq_df) {
  arguments <- data_frame(df = list(list_strat_response_freq_df), # 1st col = input df repeated to be same length as number of questions
                          question_no_chr = c(multi_choice_question_list))
  data <- pmap(arguments, test_plot_function) # collating data during map and assigning output
  return(data)
}


# testing the pmap function
test_output <- test_function(strat_response_freq$language, multi_choice_question_list)
test_output

# # making plots for all of the stratified data sets
# map(strat_response_freq, test_function)


# typed plots -------------------------------------------------------------

# look into ideas to plot responses to typed questions



# notes -------------------------------------------------------------------


