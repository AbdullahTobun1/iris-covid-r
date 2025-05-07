# Load required libraries
library(tidyverse)
library(readr)
library(ggplot2)

# Function to load and clean unemployment data
load_unemployment_data <- function(file_path) {
  read_csv(file_path) %>%
    mutate(time = as.character(time_period),  # <- Removed extra )
           unemployment_rate = as.numeric(value)) %>%  
    select(time, unemployment_rate) %>%
    drop_na()  
  
}

# Function to load and clean COVID data
load_covid_data <- function(file_path) {
  read_csv(file_path) %>%
    mutate(time = as.character(Year),  # Adjust column name as needed
           covid_metric = as.numeric(cases)) %>%  # Adjust column name as needed
    select(time, covid_metric) %>%
    drop_na()  # Remove rows with missing values
}

# Prompt user to select files
cat("Please select the unemployment CSV file:\n")
unemployment_file <- file.choose()

cat("Please select the COVID metrics CSV file:\n")
covid_file <- file.choose()

# Load datasets with error handling
tryCatch({
  unemployment <- load_unemployment_data(unemployment_file)
  covid <- load_covid_data(covid_file)
  
  # Merge datasets on time
  merged_data <- inner_join(unemployment, covid, by = "time") %>%
    mutate(time = as.factor(time))  # Convert time to factor for better plotting
  
  # Calculate correlation with error handling
  if(nrow(merged_data) > 1) {
    correlation <- cor(merged_data$unemployment_rate, 
                       merged_data$covid_metric, 
                       use = "complete.obs")
    cat(paste("\nCorrelation between COVID and Unemployment:", 
              round(correlation, 3), "\n"))
    
    # Plot the relationship
    plot <- ggplot(merged_data, aes(x = covid_metric, y = unemployment_rate)) +
      geom_point(color = "blue", alpha = 0.6, size = 3) +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = "Relationship between COVID Metrics and Unemployment Rate",
           subtitle = paste("Correlation:", round(correlation, 3)),
           x = "COVID Metric (cases/deaths/hospitalizations)",
           y = "Unemployment Rate (%)",
           caption = "Source: Your data sources here") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
    
    print(plot)
    
    # Save the plot
    ggsave("covid_unemployment_plot.png", plot, width = 10, height = 6)
    cat("\nPlot saved as 'covid_unemployment_plot.png' in working directory\n")
    
  } else {
    cat("\nError: Not enough data points after merging to calculate correlation.\n")
  }
  
}, error = function(e) {
  cat("\nError occurred:", e$message, "\n")
  cat("Please check:\n")
  cat("1. File formats (must be CSV)\n")
  cat("2. Column names in your files match the expected names\n")
  cat("3. There are no missing or non-numeric values in key columns\n")
})