#✔ Load libraries and dataset
library(readxl)
library(dplyr)
library(ggplot2)

# Load your Excel file
data <- read_excel("2006-2012-math-test-results-school-gender-1 (1).xlsx")

#✔ Rename Columns (Your Required Column Renaming)
colnames(data)[6]  <- "Averagescore"
colnames(data)[7]  <- "Prof_Level1"
colnames(data)[9]  <- "Prof_Level2"
colnames(data)[11] <- "Prof_Level3"

#✔ Clean Averagescore (convert to numeric but keep all data)
data <- data %>%
  mutate(
    Averagescore = ifelse(Averagescore == "s", NA, Averagescore),
    Averagescore = as.numeric(Averagescore)
  )

#✔ Convert Year to factor
data$Year <- as.factor(data$Year)

#✔ Filter Male/Female ONLY (no column deletion)
data_gender <- data %>%
  filter(Demographic %in% c("Male", "Female"))

#✔ Summary statistics (Mean of Averagescore)
summary_gender <- data_gender %>%
  group_by(Year, Demographic) %>%
  summarise(mean_score = mean(Averagescore, na.rm = TRUE),
            .groups = 'drop')

print(summary_gender)

# Histogram for Male students
ggplot(data_gender %>% filter(Demographic == "Male"),
       aes(x = Mean_Scale_Score)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  facet_wrap(~Year) +
  labs(title = "Histogram of Mean Scale Score for Male Students",
       x = "Mean Scale Score", y = "Count") +
  theme_minimal()

# Histogram for Female students
ggplot(data_gender %>% filter(Demographic == "Female"),
       aes(x = Mean_Scale_Score)) +
  geom_histogram(binwidth = 10, fill = "pink", color = "black") +
  facet_wrap(~Year) +
  labs(title = "Histogram of Mean Scale Score for Female Students",
       x = "Mean Scale Score", y = "Count") +
  theme_minimal()

# Bar plot
ggplot(summary_gender, aes(x = Year, y = mean_score, fill = Demographic)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  labs(title = "Year-wise Comparison of Mean Scale Score by Gender",
       x = "Year", y = "Mean Scale Score") +
  theme_minimal()

