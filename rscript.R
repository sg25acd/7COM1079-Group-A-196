# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

# Load the CSV file
data <- read_csv("2006-2012-math-test-results-school-gender-1 (1).csv")

# Rename the 'Mean Scale Score' column
data <- data %>%
  rename(Mean_Scale_Score = `Mean Scale Score`)

# Convert Mean_Scale_Score to numeric
data$Mean_Scale_Score <- as.numeric(data$Mean_Scale_Score)

# Convert Year to factor
data$Year <- as.factor(data$Year)

# Filter only Male & Female rows
data_gender <- data %>%
  filter(Demographic %in% c("Male", "Female")) %>%
  select(Year, Demographic, Mean_Scale_Score)

# Summary statistics by Year and Demographic
summary_gender <- data_gender %>%
  group_by(Year, Demographic) %>%
  summarise(mean_score = mean(Mean_Scale_Score, na.rm = TRUE),
            .groups = 'drop')

print(summary_gender)