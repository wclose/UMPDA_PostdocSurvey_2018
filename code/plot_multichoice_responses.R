# loading dependencies ----------------------------------------------------

# loading tidyverse and dataset
source("code/calc_response_stats.R")

# packages needed for plot size manipulation
library(grid) # required for table grobbing of response plots
library(gridExtra) # used to align response plot coordinates
library(ggpubr) # required to save gtable plots as ggplot items



# plotting functions ------------------------------------------------------

# creating a plotting function that makes new graph for each question for entire dataset
make_response_plot <- function(df, question_no_chr, unstrat_ref_df = NULL) {
  
  ### modifying dfs in preparation for plotting ###
  response_data <- df %>% 
    filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer"
           & response != "Not_applicable" & response != "None_of_the_above") %>%  # removing ambiguous answers from plots
    mutate(question = str_replace_all(question, c("_" = " ", "," = ", ")), # making text look nice
           response = str_replace_all(response, c("_" = " ", "," = ", "))) # making text look nice
  
  ### setting global function variables for standardizing plot aesthetics ###
  geom_text_pt_size <- 8 # setting desired text point size (geom_text uses different default scale than pt)
  
  # setting variables for scaling purposes
  response_no <- length(unique(response_data$response)) # calculating the number of unique responses for aspect ratio scaling
  
  ### changing which bars are plotted depending on the contents of the supplied df ###
  # if the input df does not have a col called "strat_id" (aka input df is NOT stratified)
  if (!any(names(df) == "strat_id")) {
    
    # setting variable for x axis
    x_var <- "question_no" # set the x variable to be question_no (plots a single bar)
    
    # setting variables for scaling purposes
    aspect <- 0.2*response_no/7 # scales the aspect ratio to standardize appearance of bars after setting consistent width w/ grobbing
    
  # otherwise plot this
  } else {
    
  # setting variable for x axis
    x_var <- "strat_id" # set the x variable to be strat_id (one bar per strat_id = multiple bars)
    
    # setting variables for scaling purposes
    strat_no <- length(unique(response_data$strat_id)) # calculating the number of categories/bars per question for scaling
    
    aspect <- 0.2*response_no*strat_no/7 # scales the aspect ratio to standardize appearance of bars after setting consistent width w/ grobbing
    
  }
  
  ### generating premliminary plots using code common to all plot varieties ###
  shared_plot <- response_data %>%
    ggplot(aes(x = eval(as.name(x_var)), y = percent_freq, fill = response)) + # plotting the stratified categories by response
    geom_bar(stat = "identity", show.legend = F, color = "black") + # bar plot
    geom_text(aes(x = eval(as.name(x_var)), y = percent_freq, label = paste0(format(round(percent_freq, digits = 1), nsmall = 1), "")), # adding response freq over bars
              hjust = -0.25, size = 1/72*25.4*geom_text_pt_size) + # size is given in mm so need to convert to pts = 1/72*25.4*desired_pt_size
    scale_x_discrete(labels = c(str_replace_all(unique(response_data[[x_var]]), "_", " "))) + # reformatting axis labels to look nice
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # formatting y axis
    scale_fill_viridis(discrete = TRUE, option = "D") +
    labs(title = paste(question_no_chr),
         x = "", # removing x label since the facet labels are the new x labels
         y = "Proportion of postdoctoral respondents (%)") +
    coord_flip(clip = "off") + # rotating the plots and allowing plotting outside of plot area
    # NOTE: x and y commands are now swapped due to coord_flip rotating the axes
    theme(axis.line = element_line(size = 0.5, colour = "black"), # formatting axis lines as desired
          axis.title = element_text(size = 10), # making all chart titles a consistent size
          axis.title.x = element_text(margin = margin(10,0,0,0), size = 9, face = "bold"), # adding space between x axis title and axis labels
          axis.text = element_text(size = 9))
  
  ### adding in reference lines if desired ###
  # if the data is stratified and a reference df has been included
  if (any(names(df) == "strat_id") & !is.null(unstrat_ref_df)) {
    
    # creating/reformatting df of reference data for drawing reference lines
    line_data <- unstrat_ref_df %>%
      filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer"
             & response != "Not_applicable" & response != "None_of_the_above") %>%  # removing ambiguous answers from plots
      mutate(question = str_replace_all(question, c("_" = " ", "," = ", ")), # making text look nice
             response = str_replace_all(response, c("_" = " ", "," = ", "))) # making text look nice
    
    # draws a reference line showing the values from unstratified data
    shared_plot <- shared_plot +
      geom_hline(data = line_data, aes(yintercept = percent_freq), linetype = "21", color = "red", size = 0.5, alpha = 0.75) # adding reference line equal to values for unstrat data
    
  }
  
  ### shared formatting elements of the facets/strips (facet labels) ###
  # needs be added after faceting and different questions require different faceting strategies
  shared_theme <- theme(panel.background = element_rect(fill = "white"), # making panels have white background
                        panel.spacing = unit(1, "lines"), # increasing spacing between panels
                        panel.spacing.x = unit(2.5, "lines"), # adding a bit more horizontal space between panels
                        strip.text = element_text(size = 9, face = "bold"), # setting strip labels to same size as other plot labels
                        strip.text.y = element_text(angle = 180, margin = margin(0,10,0,10)), # formatting and positioning new y labels
                        strip.text.x = element_text(margin = margin(15,0,15,0)),
                        strip.background = element_rect(fill = "white", color = NA), # formatting strip col labels
                        strip.placement = "outside") # moving strip row labels outside y axis labels to make them the new y labels
  
  ### creating separate plotting function for ***Q6*** data specifically (desired data viz requires different facetting scheme) ###
  if (question_no_chr == "Q6") {

    aspect <- aspect/4 # need to alter the aspect ratio slightly to conform with other plots due to number of rows/responses being plotted

    # adding Q6 format specific attributes to shared plot format from above
    unformatted_response_plot <- shared_plot +
      facet_wrap(~ response, nrow = 4, # plots each question/group of subquestions
                 labeller = label_wrap_gen(width = 40, multi_line = TRUE)) + # allows text wrapping in strip labels
      theme(plot.margin = margin(20,40,20,0), # giving plot a bit of padding on edges in case something is plotted out of bounds
            aspect.ratio = aspect) + # formatting bars to have a consistent size
      shared_theme # adding in the shared theme elements   
    
    # if the data is unstratified, removes y axis labels/tick marks to make it look nicer
    if (!any(names(df) == "strat_id")) {
      
      unformatted_response_plot <- unformatted_response_plot +
        theme(axis.text.y = element_blank(), # removing y axis text since there's only one group
              axis.ticks.y = element_blank()) # removing y axis tick marks since there's only one group
      
    }

    # altering the grob tables of generated plots to line up margins, axes, etc.
    grob_table <- ggplotGrob(unformatted_response_plot) # creates gtable of plot features
    
    # turning clipping off for strip chart labels (allows strip titles to go outside of plot dimensions for a little extra room)
    for(i in which(grepl("strip-t", grob_table$layout$name))){ # finds location of top strips ("strip-t") in the plot grob table
      grob_table$grobs[[i]]$layout$clip <- "off" # turns text clipping off
    }
    
    response_plot <- as_ggplot(arrangeGrob(grob_table)) # saving the resulting plot as a ggplot item
    
  ### creating/formatting plots from all other multichoice questions ###
  } else {

    # setting scaling factor for text wrapping of facet titles (more facets = less space for facet titles)
    label_width <- case_when(response_no <= 4 ~ 40,
                             response_no <= 6 ~ 25,
                             TRUE ~ 18)

    # adding format specific attributes to shared plot format from above (different from Q6 style)
    unformatted_response_plot <- shared_plot +
      facet_grid(question ~ response, # plots each question/group of subquestions
                 switch = "y", # moves y axis strip to opposite side of plot
                 labeller = labeller(question = label_wrap_gen(width = 60, multi_line = TRUE), # allows long text wrapping in strip labels
                                     # response = label_wrap_gen(width = 15, multi_line = TRUE))) + # allows shorter text wrapping in strip labels
                                     response = label_wrap_gen(width = label_width, multi_line = TRUE))) + # allows shorter text wrapping in strip labels
      theme(plot.margin = margin(20,20,20,0), # giving plot a bit of padding on edges in case something is plotted out of bounds
            aspect.ratio = aspect) + # formatting bars to have a consistent size
      shared_theme # adding in the shared theme elements    
    
    # if the data is unstratified, removes y axis labels/tick marks to make it look nicer
    if (!any(names(df) == "strat_id")) {
      
      unformatted_response_plot <- unformatted_response_plot +
        theme(axis.text.y = element_blank(), # removing y axis text since there's only one group
              axis.ticks.y = element_blank()) # removing y axis tick marks since there's only one group
      
    }

    # altering the grob tables of generated plots to line up margins, axes, etc.
    grob_table <- ggplotGrob(unformatted_response_plot) # creates gtable of plot features
    
    grob_table$widths[4] <- unit(12, "cm") # changes left side of plot to be in consistent place making all the plots align (unit value set by trial and error)

    # turning clipping off for strip chart labels (allows strip titles to go outside of plot dimensions for a little extra room)
    for(i in which(grepl("strip-t", grob_table$layout$name))){ # finds location of top strips ("strip-t") in the plot grob table
      grob_table$grobs[[i]]$layout$clip <- "off" # turns text clipping off
    }
    
    response_plot <- as_ggplot(arrangeGrob(grob_table)) # saving the resulting plot as a ggplot item

  }

  ### returning the finished plots ###
  return(response_plot)
  
}


# # testing make_response_plot() function
# make_response_plot(strat_response_freq$college_school, "Q22", response_freq)
# test_plot <- make_response_plot(response_freq, "Q35")
# make_response_plot(response_freq, "Q35")
# make_response_plot(strat_response_freq$college_school, "Q6")
# make_response_plot(response_freq, "Q35")
# college_test <- make_response_plot(strat_response_freq$college_school, "Q6", response_freq)





# making function to cycle through each question for each data frame based on strat category
# extra function needed because of the nested nature of strat_response_freq_df
make_all_response_plots <- function(response_freq_df, question_no_chr_list, unstrat_ref_df = NULL) {
  
  arguments <- data_frame(df = list(response_freq_df), # 1st col = input df repeated to be same length as number of questions
                          question_no_chr = c(question_no_chr_list)) # 2nd col = list of question numbers in chr format
  
  data <- pmap(arguments, make_response_plot, unstrat_ref_df = unstrat_ref_df) %>% # collating data during map and assigning output
    set_names(question_no_chr_list) # naming the output for easier indexing
  
  return(data)
  
}



# saving functions --------------------------------------------------------

# how to tell difference between unstrat vs strat plot nested lists
class(unstrat_response_plots)
any(class(unstrat_response_plots[[1]]) == "list")
any(class(strat_response_plots[[1]]) == "list") # strat_plot list 

grepl("unstrat", deparse(substitute(strat_response_plots$college_school$Q12))) # looking for 'unstrat' in the name of the plot being supplied

unstrat_response_plots

deparse(substitute(strat_response_plots$college_school$Q12))

test_name <- str_split(deparse(substitute(unstrat_response_plots$Q12)), "\\$")[[1]][2] # pulling category from name of plot

test_name

last(str_split(deparse(substitute(unstrat_response_plots$Q16)), "\\$") %>% unlist()) # pulling question_no from name of plot

str_detect(test_name, "Q\\d+")

response_freq %>% # pulls questions from data used to generate plots
  filter(question_no == "Q35" & !is.na(response) & response != "Prefer_not_to_answer") %>% # removing ambiguous answers from plots
  pull(question) %>% 
  unique() %>% 
  length()






# creating save function for unstratified data
# dynamically scales height of output, saves in desired dir, and names files dynamically
test_save_plots <- function(plot_name) {
  
  category <- str_split(deparse(substitute(plot_name)), "\\$")[[1]][2] # pulling category from name of plot
  
  question_no_chr <- last(str_split(deparse(substitute(plot_name)), "\\$") %>% unlist()) # pulling question_no from name of plot by extracting last string after splitting at '$'
  
  if (str_detect(category, "Q\\d+")) {
    
    category <- "all" # if category is not present (as in case of unstratified data), sets category to "all" so the plot is saved in the proper location, etc.
    
    question_tally <- response_freq %>% # pulls questions from data used to generate plots
      filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer" & response != "Not_applicable") %>% # removing ambiguous answers from plots
      pull(question) %>%  # returns list of questions to be stored
      unique() %>% # removing repeated questions from list
      length() # calculates the number of questions for use in scaling height
    
    # setting different plot height scale for Q6 data because facet_wrap is set to nrow = 4 in plotting function
    if (question_no_chr == "Q6") {
      
      plot_height <- 4 * ((4/7 * question_tally) + 3) # scaling factor for plot height based on (lots of) trial and error
      
    # setting plot height scale for all other plots
    } else {
      
      plot_height <- ((4/7 * question_tally) + 3) # scaling factor for plot height based on (lots of) trial and error
      
    }
    
  } else {
    
    question_tally <- strat_response_freq[[category]] %>% # pulls questions from data used to generate plots
      filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer" & response != "Not_applicable") %>% # removing ambiguous answers from plots
      pull(question) %>%  # returns list of questions to be stored
      unique() %>% 
      length()
    
    strat_tally <- strat_response_freq[[category]] %>% # pulls list of strat_ids for each plot for use in scaling height of output figure
      pull(strat_id) %>% 
      unique() %>% 
      length()
    
    # setting different plot height scale for Q6 data because facet_wrap is set to nrow = 4 in plotting function
    if (question_no_chr == "Q6") {
      
      plot_height <- 4 * ((4/7 * question_tally * 0.6 * strat_tally) + 3) # scaling factor for plot height based on (lots of) trial and error
      
      # setting plot height scale for all other plots
    } else {
      
      plot_height <- ((4/7 * question_tally * 0.6 * strat_tally) + 3) # scaling factor for plot height based on (lots of) trial and error
      
    }
    
  }
  
  # saving the plots using the parameters defined above
  ggsave(plot = plot_name, filename = paste0("results/", category, "/", paste(category, question_no_chr, sep = "_"), ".png"), # saving the plots as png
         device = "png", width = 15, height = plot_height, dpi = 300) # specifying dimensions/resolution of plots
  
}

test_save_plots(strat_response_plots$college_school$Q22)




# # creating save function for unstratified data
# # dynamically scales height of output, saves in desired dir, and names files dynamically
# save_multichoice_plots <- function(plot_name, category=NULL, question_no_chr) {
#   questions <- response_freq %>% # pulls questions from data used to generate plots
#     filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer") %>% # removing ambiguous answers from plots
#     pull(question) # returns list of questions to be stored
#   
#   subquestion_no <- length(unique(questions)) # calculates the number of questions for use in scaling height
#   
#   plot_height <- ((4/7 * subquestion_no) + 2) # scaling factor for plot height based on trial and error
#   
#   ggsave(plot = plot_name, filename = paste0("results/", category, "/", paste(category, question_no_chr, sep = "_"), ".png"), # saving the plots as png
#          device = "png", width = 15, height = plot_height, dpi = 300) # specifying dimensions of plots
# }

# setting up mapping function to loop through all plots and question numbers of unstratified data
save_all_multichoice_plots <- function(plot_list, category, question_no_chr_list) {
  arguments <- data_frame(plot_name = plot_list,
                          question_no_chr = c(question_no_chr_list))
  pmap(arguments, save_multichoice_plots, category = category)
}





# # creating function to save plots of stratified data
# # based on unstratified function but also adds in scaling factor for number of strat categories (bars) in each
# save_strat_plots <- function(plot_name, category, question_no_chr) {
#   
#   # defining global final attributes for saved plots
#   questions <- strat_response_freq[[category]] %>% # pulls questions from data used to generate plots
#     filter(question_no == question_no_chr & !is.na(response) & response != "Prefer_not_to_answer" & response != "Not_applicable") %>% # removing ambiguous answers from plots
#     pull(question) # returns list of questions to be stored
#   
#   strat_ids <- strat_response_freq[[category]] %>% # pulls list of strat_ids for each plot for use in scaling height of output figure
#     pull(strat_id)
#   
#   subquestion_no <- length(unique(questions)) # calculates the number of questions for use in scaling height
#   
#   strat_id_no <- length(unique(strat_ids)) # calculates number of strat ids for use in scaling
#   
#   # setting different plot height scale for Q6 data because facet_wrap is set to nrow = 4 in plotting function
#   if (question_no_chr == "Q6") {
#     
#     plot_height <- 4 * ((4/7 * subquestion_no * 0.6 * strat_id_no) + 3) # scaling factor for plot height based on (lots of) trial and error
#     
#   # setting plot height scale for all other plots
#   } else {
#     
#     plot_height <- ((4/7 * subquestion_no * 0.6 * strat_id_no) + 3) # scaling factor for plot height based on (lots of) trial and error
#     
#   }
#   
#   # saving the plots using the parameters defined above
#   ggsave(plot = plot_name, filename = paste0("results/", category, "/", paste(category, question_no_chr, sep = "_"), ".png"), # saving the plots as png
#          device = "png", width = 15, height = plot_height, dpi = 300) # specifying dimensions/resolution of plots
#   
# }

# # testing save_strat_plots() function
# save_strat_plots(college_test, "college_school", "Q6")










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
unstrat_response_plots <- make_all_response_plots(response_freq, multi_choice_question_list)

# testing make_all_response_plots() output
unstrat_response_plots$Q6



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
strat_response_plots <- map(.x = strat_response_freq, .f = make_all_response_plots,
                            question_no_chr_list = multi_choice_question_list, unstrat_ref_df = response_freq)

# verifying output of map(make_all_strat_response_plots())
strat_response_plots$residency$Q6
strat_response_plots$college_school$Q35
strat_response_plots$residency$Q16
strat_response_plots$college_school$Q16
strat_response_plots$residency$Q49



# saving stratified plots -------------------------------------------------

# # testing save_strat_plots()
# save_strat_plots(strat_response_plots$residency$Q35, "residency", "Q35")
# save_strat_plots(strat_response_plots$residency$Q49, "residency", "Q49")

# # testing save_all_strat_plots
# save_all_strat_plots(strat_response_plots$college_school, category = "college_school", multi_choice_question_list)

# saving all of the stratified plots in the appropriate locations
save_all_strat_plots_set(strat_response_plots, strat_list_names, multi_choice_question_list)



# notes -------------------------------------------------------------------

# NOTE: Q22 responses need to be broken up and retabulated before graphing

# NOTE: could set up an if/else statement to consolidate plotting functions into a single function

# NOTE: Need to design separate plotting functions for Q6 and 22

