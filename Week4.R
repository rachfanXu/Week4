# Loading necessary libraries
library("purrr")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(glue)

# Reading the dataset from the CSV file
data <- read.csv("EMDAT.csv")

# Filtering and renaming the dataset for desired countries and metrics
df <- data %>% 
  # Filter for specific countries
  filter(Entity %in% c("Afghanistan", "Argentina", "Australia", "Belgium", "Canada")) %>%
  # Select required columns
  select(Entity, Year, deaths_all_disasters, injured_all_disasters, homeless_all_disasters) %>%
  # Rename the columns for better clarity
  rename(deaths = deaths_all_disasters, 
         injuries = injured_all_disasters, 
         homelessness = homeless_all_disasters, 
         country = Entity)

# View the transformed dataset
View(df)

# Function to create a trend plot for a given metric (e.g., deaths, injuries, homelessness)
create_trend_plot <- function(i) {
  # Extract the name of the metric based on the index 'i'
  metric <- names(df)[i]
  
  # Plot the data
  df %>%
    ggplot(aes_string(x = "Year", y = metric, color = "country")) +   # Set axes and color aesthetics
    geom_line() +                                                      # Draw line plot
    labs(
      title = glue("Trends of {metric}"),                              # Dynamic title based on metric name
      y = glue("{metric}")                                            # Dynamic y-axis label
    ) +
    theme_minimal() +                                                 # Apply minimal theme
    scale_color_brewer(palette = "Set1") +                            # Set color palette
    theme(legend.position = "top")                                    # Position the legend at the top
}

# Use the map function to create trend plots for each metric
plots_list <- map(3:5, create_trend_plot)

# Arrange the plots in a grid with one column
plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 1)
plots_grid
