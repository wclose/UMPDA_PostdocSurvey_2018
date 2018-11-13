# loading dependencies ----------------------------------------------------

# loading tidyverse and dataset
source("code/calc_response_stats.R")

# packages needed for plot size manipulation
library(grid) # required for table grobbing of response plots
library(gridExtra) # used to align response plot coordinates
library(ggpubr) # required to save gtable plots as ggplot items



# plotting functions ------------------------------------------------------

# NOTE: Need to design separate plotting functions for Q6 and 22
# NOTE: Q22 responses need to be broken up and retabulated before graphing

# creating a plotting function that makes new graph for each question for entire dataset
make_response_plot <- function(df, question_no_chr) {
  response_data <- df %>% 
    filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer") # removing ambiguous answers from plots
  
  geom_text_pt_size <- 8 # setting desired size for geom_text plotting
  
  response_no <- length(unique(response_data$response)) # calculating the number of unique responses for aspect ratio scaling
  
  aspect <- 0.2*response_no/7 # scales the aspect ratio to standardize appearance of bars after setting consistent width w/ grobbing
  
  response_plot <- response_data %>% 
    ggplot(aes(x = question_no, y = percent_freq, fill = response)) + # plotting the stratified categories by response
    geom_bar(stat = "identity", show.legend = F, color = "black") + # bar plot
    geom_text(aes(x = question_no, y = percent_freq, label = paste0(format(round(percent_freq, digits = 1), nsmall = 1), "")), # adding response freq over bars
              hjust = -0.25, size = 1/72*25.4*geom_text_pt_size) + # size is given in mm so need to convert to pts = 1/72*25.4*desired_pt_size
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # formatting y axis
    facet_grid(str_replace_all(question, "_", " ") ~ str_replace_all(response, c("_" = " ", "," = ", ")), # plots each question/group of subquestions
               switch = "y", # moves y axis strip to opposite side of plot
               labeller = label_wrap_gen(width = 60, multi_line = TRUE)) + # allows text wrapping in strip labels
    labs(x = "", # removing x label since the facet labels are the new x labels
         y = "Proportion of postdoctoral respondents (%)") +
    coord_flip(clip = "off") + # rotating the plots and allowing plotting outside of plot area
    # NOTE: x and y commands are now swapped due to coord_flip rotating the axes
    theme(axis.line = element_line(size = 0.5, colour = "black"), # formatting axis lines as desired
          axis.title = element_text(size = 10), # making all chart titles a consistent size
          axis.title.x = element_text(margin = margin(10,0,0,0)), # adding space between x axis title and axis labels
          axis.text = element_text(size = 8),
          axis.text.y = element_blank(), # removing y axis text since there's only one group
          axis.ticks.y = element_blank(), # removing y axis tick marks since there's only one group
          plot.margin = margin(20,20,20,0), # giving plot a bit of padding on edges in case something is plotted out of bounds
          # formatting elements of the facets/strips (facet labels)
          panel.background = element_rect(fill = "white"), # making panels have white background
          panel.spacing = unit(1, "lines"), # increasing spacing between panels
          panel.spacing.x = unit(2, "lines"), # adding a bit more horizontal space between panels
          strip.text = element_text(size = 10), # setting strip labels to same size as other plot labels
          strip.text.y = element_text(angle = 180, margin = margin(0,10,0,10)), # formatting and positioning new y labels
          strip.text.x = element_text(margin = margin(15,0,15,0)),
          strip.background.x = element_rect(fill = "white", color = NA), # formatting strip col labels
          strip.background.y = element_rect(fill = "white", color = NA), # removing border from strip row labels (new y labels)
          strip.placement = "outside", # moving strip row labels outside y axis labels to make them the new y labels
          # formatting plots to have a consistent size
          aspect.ratio = aspect) # making size of bars compared to plot consistent
  
  grob_table <- ggplotGrob(response_plot) # creates a gtable of plot features
  
  grob_table$widths[4] <- unit(12, "cm") # sets alignment of y axis in chart area thereby aligning all plots generated with this script
  
  grobbed_plot <- as_ggplot(arrangeGrob(grob_table)) # recreating the plots with updated coordinates and saving as a ggplot item
  
  return(grobbed_plot)
}



# creating a plotting function for the stratified data
# based on plotting function for unstratified data but builds in scaling for number of strat categories, etc.
# NOTE: could set up an if/else statement to consolidate plotting functions into a single function
make_strat_response_plot <- function(df, ref_df, question_no_chr) {
  
  response_data <- df %>% 
    filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer") # removing ambiguous answers from plots
  
  line_data <- ref_df %>% 
    filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer") # removing ambiguous answers from plots
  
  geom_text_pt_size <- 8
  
  response_no <- length(unique(response_data$response)) # calculating the number of unique responses for aspect ratio scaling
  
  strat_no <- length(unique(response_data$strat_id)) # calculating the number of categories/bars per question for scaling
  
  aspect <- 0.2*response_no*strat_no/7 # scales the aspect ratio to standardize appearance of bars after setting consistent width w/ grobbing
  
  response_plot <- response_data %>% 
    ggplot(aes(x = strat_id, y = percent_freq, fill = response)) + # plotting the stratified categories by response
    geom_bar(stat = "identity", show.legend = F, color = "black") + # bar plot
    geom_hline(data = line_data, aes(yintercept = percent_freq), linetype = "21", color = "red", size = 0.5, alpha = 0.75) + # adding reference line equal to values for unstrat data
    geom_text(aes(x = strat_id, y = percent_freq, label = paste0(format(round(percent_freq, digits = 1), nsmall = 1), "")), # adding response freq over bars
              hjust = -0.25, size = 1/72*25.4*geom_text_pt_size) + # size is given in mm so need to convert to pts = 1/72*25.4*desired_pt_size
    scale_x_discrete(labels = c(str_replace_all(unique(df$strat_id), "_", " "))) + # reformatting axis labels to look nice
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # formatting y axis
    scale_fill_viridis(discrete = TRUE, option = "D") +
    facet_grid(str_replace_all(question, "_", " ") ~ str_replace_all(response, c("_" = " ", "," = ", ")), # plots each question/group of subquestions
               switch = "y", # moves y axis strip to opposite side of plot
               labeller = label_wrap_gen(width = 60, multi_line = TRUE)) + # allows text wrapping in strip labels
    labs(title = paste(question_no_chr), 
         x = "", # removing x label since the facet labels are the new x labels
         y = "Proportion of postdoctoral respondents (%)") +
    coord_flip(clip = "off") + # rotating the plots and allowing plotting outside of plot area
    # NOTE: x and y commands are now swapped due to coord_flip rotating the axes
    theme(axis.line = element_line(size = 0.5, colour = "black"), # formatting axis lines as desired
          axis.title = element_text(size = 10), # making all chart titles a consistent size
          axis.title.x = element_text(margin = margin(10,0,0,0)), # adding space between x axis title and axis labels
          axis.text = element_text(size = 8),
          plot.margin = margin(20,20,20,0), # giving plot a bit of padding on edges in case something is plotted out of bounds
          # formatting elements of the facets/strips (facet labels)
          panel.background = element_rect(fill = "white"), # making panels have white background
          panel.spacing = unit(1, "lines"), # increasing spacing between panels
          panel.spacing.x = unit(2, "lines"), # adding a bit more horizontal space between panels
          strip.text = element_text(size = 10), # setting strip labels to same size as other plot labels
          strip.text.y = element_text(angle = 180, margin = margin(0,10,0,10)), # formatting and positioning new y labels
          strip.text.x = element_text(margin = margin(15,0,15,0)),
          strip.background.x = element_rect(fill = "white", color = NA), # formatting strip col labels
          strip.background.y = element_rect(fill = "white", color = NA), # removing border from strip row labels (new y labels)
          strip.placement = "outside", # moving strip row labels outside y axis labels to make them the new y labels
          # formatting plots to have a consistent size
          aspect.ratio = aspect) # making size of bars compared to plot consistent
  
  grob_table <- ggplotGrob(response_plot) # creates gtable of plot features
  
  grob_table$widths[4] <- unit(12, "cm") # changes left side of plot to be in consistent place making all the plots align
  
  grobbed_plot <- as_ggplot(arrangeGrob(grob_table)) # saving the resulting plot as a ggplot item
  
  return(response_plot)
  
}

# make_strat_response_plot <- function(df, question_no_chr) {
#   response_data <- df %>% 
#     filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer") # removing ambiguous answers from plots
#   
#   geom_text_pt_size <- 8
#   
#   response_no <- length(unique(response_data$response)) # calculating the number of unique responses for aspect ratio scaling
#   
#   strat_no <- length(unique(response_data$strat_id)) # calculating the number of categories/bars per question for scaling
#   
#   aspect <- 0.2*response_no*strat_no/7 # scales the aspect ratio to standardize appearance of bars after setting consistent width w/ grobbing
#   
#   response_plot <- response_data %>% 
#     ggplot(aes(x = strat_id, y = percent_freq, fill = response)) + # plotting the stratified categories by response
#     geom_bar(stat = "identity", show.legend = F, color = "black") + # bar plot
#     geom_text(aes(x = strat_id, y = percent_freq, label = paste0(format(round(percent_freq, digits = 1), nsmall = 1), "")), # adding response freq over bars
#               hjust = -0.25, size = 1/72*25.4*geom_text_pt_size) + # size is given in mm so need to convert to pts = 1/72*25.4*desired_pt_size
#     scale_x_discrete(labels = c(str_replace_all(unique(df$strat_id), "_", " "))) + # reformatting axis labels to look nice
#     scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # formatting y axis
#     facet_grid(str_replace_all(question, "_", " ") ~ str_replace_all(response, c("_" = " ", "," = ", ")), # plots each question/group of subquestions
#                switch = "y", # moves y axis strip to opposite side of plot
#                labeller = label_wrap_gen(width = 60, multi_line = TRUE)) + # allows text wrapping in strip labels
#     labs(x = "", # removing x label since the facet labels are the new x labels
#          y = "Proportion of postdoctoral respondents (%)") +
#     coord_flip(clip = "off") + # rotating the plots and allowing plotting outside of plot area
#     # NOTE: x and y commands are now swapped due to coord_flip rotating the axes
#     theme(axis.line = element_line(size = 0.5, colour = "black"), # formatting axis lines as desired
#           axis.title = element_text(size = 10), # making all chart titles a consistent size
#           axis.title.x = element_text(margin = margin(10,0,0,0)), # adding space between x axis title and axis labels
#           axis.text = element_text(size = 8),
#           plot.margin = margin(20,20,20,0), # giving plot a bit of padding on edges in case something is plotted out of bounds
#           # formatting elements of the facets/strips (facet labels)
#           panel.background = element_rect(fill = "white"), # making panels have white background
#           panel.spacing = unit(1, "lines"), # increasing spacing between panels
#           panel.spacing.x = unit(2, "lines"), # adding a bit more horizontal space between panels
#           strip.text = element_text(size = 10), # setting strip labels to same size as other plot labels
#           strip.text.y = element_text(angle = 180, margin = margin(0,10,0,10)), # formatting and positioning new y labels
#           strip.text.x = element_text(margin = margin(15,0,15,0)),
#           strip.background.x = element_rect(fill = "white", color = NA), # formatting strip col labels
#           strip.background.y = element_rect(fill = "white", color = NA), # removing border from strip row labels (new y labels)
#           strip.placement = "outside", # moving strip row labels outside y axis labels to make them the new y labels
#           # formatting plots to have a consistent size
#           aspect.ratio = aspect) # making size of bars compared to plot consistent
#   
#   grob_table <- ggplotGrob(response_plot) # creates gtable of plot features
#   
#   grob_table$widths[4] <- unit(12, "cm") # changes left side of plot to be in consistent place making all the plots align
#   
#   grobbed_plot <- as_ggplot(arrangeGrob(grob_table)) # saving the resulting plot as a ggplot item
#   
#   return(grobbed_plot)
# }



# making function to cycle through each question for each data frame based on strat category
# extra function needed because of the nested nature of strat_response_freq_df
make_all_strat_response_plots <- function(strat_response_freq_df, question_no_chr_list, ref_df) {
  arguments <- data_frame(df = list(strat_response_freq_df), # 1st col = input df repeated to be same length as number of questions
                          question_no_chr = c(question_no_chr_list)) # 2nd col = list of question numbers in chr format
  data <- pmap(arguments, make_strat_response_plot, ref_df = ref_df) %>% # collating data during map and assigning output
    set_names(question_no_chr_list) # naming the output for easier indexing
  return(data)
}



# saving functions --------------------------------------------------------

# creating save function for unstratified data
# dynamically scales height of output, saves in desired dir, and names files dynamically
save_multichoice_plots <- function(plot_name, category=NULL, question_no_chr) {
  questions <- response_freq %>% # pulls questions from data used to generate plots
    filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer") %>% # removing ambiguous answers from plots
    pull(question) # returns list of questions to be stored
  subquestion_no <- length(unique(questions)) # calculates the number of questions for use in scaling height
  plot_height <- ((4/7 * subquestion_no) + 2) # scaling factor for plot height based on trial and error
  ggsave(plot = plot_name, filename = paste0("results/", category, "/", paste(category, question_no_chr, sep = "_"), ".png"), # saving the plots as png
         device = "png", width = 15, height = plot_height, dpi = 300) # specifying dimensions of plots
}

# setting up mapping function to loop through all plots and question numbers of unstratified data
save_all_multichoice_plots <- function(plot_list, category, question_no_chr_list) {
  arguments <- data_frame(plot_name = plot_list,
                          question_no_chr = c(question_no_chr_list))
  pmap(arguments, save_multichoice_plots, category = category)
}



# creating function to save plots of stratified data
# based on unstratified function but also adds in scaling factor for number of strat categories (bars) in each
save_strat_plots <- function(plot_name, category, question_no_chr) {
  questions <- strat_response_freq[[category]] %>% # pulls questions from data used to generate plots
    filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer") %>% # removing ambiguous answers from plots
    pull(question) # returns list of questions to be stored
  strat_ids <- strat_response_freq[[category]] %>% # pulls list of strat_ids for each plot for use in scaling height of output figure
    pull(strat_id)
  subquestion_no <- length(unique(questions)) # calculates the number of questions for use in scaling height
  strat_id_no <- length(unique(strat_ids)) # calculates number of strat ids for use in scaling
  plot_height <- ((4/7 * subquestion_no * 0.6 * strat_id_no) + 2) # scaling factor for plot height based on (lots of) trial and error
  ggsave(plot = plot_name, filename = paste0("results/", category, "/", paste(category, question_no_chr, sep = "_"), ".png"), # saving the plots as png
         device = "png", width = 15, height = plot_height, dpi = 300) # specifying dimensions of plots
}

# setting up mapping function to loop through all plots and question numbers of stratified data
save_all_strat_plots <- function(plot_list, question_no_chr_list, category) {
  arguments <- data_frame(plot_name = plot_list,
                          question_no_chr = question_no_chr_list)
  pmap(arguments, save_strat_plots, category = category)
}

# creating function to feed multiple lists of plots, categories, and save folders for stratified data
# need this extra function due to the stratified plots being in a list within a list
save_all_strat_plots_set <- function(plot_list_list, category_list, question_no_chr_list) {
  arguments <- data_frame(plot_list = plot_list_list,
                          category = category_list)
  pmap(arguments, save_all_strat_plots, question_no_chr_list = question_no_chr_list)
}



# generating plots for unstratified data ----------------------------------

# iterating through the list of question numbers over the df response_freq which contains all the data
response_plots <- map(.x = multi_choice_question_list, .f = make_response_plot, df = response_freq) %>% 
  set_names(multi_choice_question_list)

# # testing map(make_response_plot()) output
# response_plots$Q9
# response_plots$Q7
# response_plots$Q49



# saving unstratified plots -----------------------------------------------

# # testing save_multichoice_plots()
# save_multichoice_plots(response_plots$Q35, "test", "Q35")
# save_multichoice_plots(response_plots$Q49, "test", "Q49")
# save_multichoice_plots(response_plots$Q7, "test", "Q7")

# saving all of the unstratified plots
save_all_multichoice_plots(response_plots, "all", multi_choice_question_list)



# generating plots for stratified data ------------------------------------

# # testing make_strat_response_plot()
# make_strat_response_plot(strat_response_freq$language, "Q16")
# make_strat_response_plot(strat_response_freq$language, "Q35")
# make_strat_response_plot(strat_response_freq$college_school, "Q35")

# # testing make_all_response_plots()
# test_output <- make_all_response_plots(strat_response_freq$language, multi_choice_question_list)
# test_output[[1]]
# test_output$Q27

# making plots for all of the stratified data sets
# question_no_chr_list is passed on to make_all_strat_response_plots()
strat_response_plots <- map(.x = strat_response_freq, .f = make_all_strat_response_plots, question_no_chr_list = multi_choice_question_list, ref_df = response_freq)

# # verifying output of map(make_all_strat_response_plots())
# strat_response_plots$residency$Q6
# strat_response_plots$college_school$Q35
# strat_response_plots$residency$Q16
# strat_response_plots$college_school$Q16
# strat_response_plots$residency$Q49



# saving stratified plots -------------------------------------------------

# # testing save_strat_plots()
# save_strat_plots(strat_response_plots$residency$Q35, "residency", "Q35")
# save_strat_plots(strat_response_plots$residency$Q49, "residency", "Q49")

# # testing save_all_strat_plots
# save_all_strat_plots(strat_response_plots$college_school, category = "college_school", multi_choice_question_list)

# saving all of the stratified plots in the appropriate locations
save_all_strat_plots_set(strat_response_plots, strat_list_names, multi_choice_question_list)










# adding lines to stratified plots ----------------------------------------

# # updating plotting function for stratified data
# # new function adds reference lines representing values from unstratified data
# test_make_strat_response_plot <- function(df, ref_df, question_no_chr) {
#   
#   response_data <- df %>% 
#     filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer") # removing ambiguous answers from plots
#   
#   line_data <- ref_df %>% 
#     filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer") # removing ambiguous answers from plots
#   
#   geom_text_pt_size <- 8
#   
#   response_no <- length(unique(response_data$response)) # calculating the number of unique responses for aspect ratio scaling
#   
#   strat_no <- length(unique(response_data$strat_id)) # calculating the number of categories/bars per question for scaling
#   
#   aspect <- 0.2*response_no*strat_no/7 # scales the aspect ratio to standardize appearance of bars after setting consistent width w/ grobbing
#   
#   response_plot <- response_data %>% 
#     ggplot(aes(x = strat_id, y = percent_freq, fill = response)) + # plotting the stratified categories by response
#     geom_bar(stat = "identity", show.legend = F, color = "black") + # bar plot
#     geom_hline(data = line_data, aes(yintercept = percent_freq), linetype = "21", color = "red", size = 0.5, alpha = 0.75) + # adding reference line equal to values for unstrat data
#     geom_text(aes(x = strat_id, y = percent_freq, label = paste0(format(round(percent_freq, digits = 1), nsmall = 1), "")), # adding response freq over bars
#               hjust = -0.25, size = 1/72*25.4*geom_text_pt_size) + # size is given in mm so need to convert to pts = 1/72*25.4*desired_pt_size
#     scale_x_discrete(labels = c(str_replace_all(unique(df$strat_id), "_", " "))) + # reformatting axis labels to look nice
#     scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # formatting y axis
#     scale_fill_viridis(discrete = TRUE, option = "D") +
#     facet_grid(str_replace_all(question, "_", " ") ~ str_replace_all(response, c("_" = " ", "," = ", ")), # plots each question/group of subquestions
#                switch = "y", # moves y axis strip to opposite side of plot
#                labeller = label_wrap_gen(width = 60, multi_line = TRUE)) + # allows text wrapping in strip labels
#     labs(title = paste(question_no_chr), 
#          x = "", # removing x label since the facet labels are the new x labels
#          y = "Proportion of postdoctoral respondents (%)") +
#     coord_flip(clip = "off") + # rotating the plots and allowing plotting outside of plot area
#     # NOTE: x and y commands are now swapped due to coord_flip rotating the axes
#     theme(axis.line = element_line(size = 0.5, colour = "black"), # formatting axis lines as desired
#           axis.title = element_text(size = 10), # making all chart titles a consistent size
#           axis.title.x = element_text(margin = margin(10,0,0,0)), # adding space between x axis title and axis labels
#           axis.text = element_text(size = 8),
#           plot.margin = margin(20,20,20,0), # giving plot a bit of padding on edges in case something is plotted out of bounds
#           # formatting elements of the facets/strips (facet labels)
#           panel.background = element_rect(fill = "white"), # making panels have white background
#           panel.spacing = unit(1, "lines"), # increasing spacing between panels
#           panel.spacing.x = unit(2, "lines"), # adding a bit more horizontal space between panels
#           strip.text = element_text(size = 10), # setting strip labels to same size as other plot labels
#           strip.text.y = element_text(angle = 180, margin = margin(0,10,0,10)), # formatting and positioning new y labels
#           strip.text.x = element_text(margin = margin(15,0,15,0)),
#           strip.background.x = element_rect(fill = "white", color = NA), # formatting strip col labels
#           strip.background.y = element_rect(fill = "white", color = NA), # removing border from strip row labels (new y labels)
#           strip.placement = "outside", # moving strip row labels outside y axis labels to make them the new y labels
#           # formatting plots to have a consistent size
#           aspect.ratio = aspect) # making size of bars compared to plot consistent
#   
#   grob_table <- ggplotGrob(response_plot) # creates gtable of plot features
#   
#   grob_table$widths[4] <- unit(12, "cm") # changes left side of plot to be in consistent place making all the plots align
#   
#   grobbed_plot <- as_ggplot(arrangeGrob(grob_table)) # saving the resulting plot as a ggplot item
#   
#   return(response_plot)
#   
# }

# test_make_strat_response_plot(strat_response_freq$college_school, response_freq, "Q35")



# notes -------------------------------------------------------------------

# NOTE: Q22 responses need to be broken up and retabulated before graphing

# NOTE: could set up an if/else statement to consolidate plotting functions into a single function

# NOTE: Need to design separate plotting functions for Q6 and 22

