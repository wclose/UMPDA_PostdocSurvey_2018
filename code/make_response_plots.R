# loading dependencies ----------------------------------------------------

# loading tidyverse and dataset
source("code/calc_response_stats.R")



# misc functions ----------------------------------------------------------

# creating a function to wrap the text of long titles by setting the max number of chrs per line (width = ##)
# ex: wrapper("Super long title", width = 50)
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}



# multiple choice plots ---------------------------------------------------

# NOTE: Q22 responses need to be broken up and retabulated before graphing

# making function to generate plots for multi choice questions
make_response_plot <- function(df) { # df = properly formatted data frame with data
  freq_plot <- df %>% # assigning the output
    ggplot() +
    geom_bar(aes(x = response, y = percent_freq, fill = response), # adding bars
             color = "black", show.legend = F, stat = "identity", width = 0.75) +
    geom_text(aes(x = response, y = percent_freq, label = paste0(round(percent_freq, 1), "%")), vjust = -0.5) + # adding response freq over bars
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) + # formatting y axis
    scale_x_discrete(labels = c(str_replace_all(df$response, "_", " "))) + # reformatting axis labels to look nice
    labs(title = wrapper(str_replace_all(unique(df$question), "_", " "), width = 60), # reformatting remainging labels to look nice
         y = "Proportion of Postdoctoral Respondents (%)",
         x = "Responses") +
    theme_classic() + # changing color scheme, etc.
    theme(plot.title = element_text(hjust = 0.5)) # centering the title over the plot
  return(freq_plot) # returning the plot to environment
}

# # testing response plotting function
# make_response_plot(response_freq$Q12)

# using map() to generate plots for each data frame contained within response_freq list
response_plots <- map(response_freq, make_response_plot) # don't need to specify names b/c preserves names from response_freq

# # verifying indexing ability and plot appearance
# response_plots$Q12



# typed plots -------------------------------------------------------------

# look into ideas to plot responses to typed questions



# notes -------------------------------------------------------------------


