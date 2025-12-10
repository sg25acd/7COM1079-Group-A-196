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

# Clean data for analysis
data_clean <- data_gender %>%
  filter(!is.na(Averagescore)) %>%
  select(Year, Demographic, Averagescore)

# Visualization 1: Histogram with normal curve
histogram_function <- function(df, binwidth = 10, ymax = 1000) {
  stats <- df %>% group_by(Demographic) %>%
    summarise(mean_score = mean(Averagescore),
              sd_score = sd(Averagescore),
              n = n(),
              .groups = 'drop')
  
  curve_data <- do.call(rbind, lapply(1:nrow(stats), function(i) {
    x_vals <- seq(min(df$Averagescore), max(df$Averagescore), length.out = 100)
    y_vals <- dnorm(x_vals, mean = stats$mean_score[i], sd = stats$sd_score[i])
    y_vals <- y_vals * stats$n[i] * binwidth
    data.frame(x = x_vals, y = y_vals, Demographic = stats$Demographic[i])
  }))
  
  ggplot(df, aes(x = Averagescore)) +
    geom_histogram(aes(y = ..count..), binwidth = binwidth, 
                   fill = "lightblue", color = "black", alpha = 0.6) +
    geom_line(data = curve_data, aes(x = x, y = y), color = "red", size = 1) +
    facet_wrap(~Demographic) +
    labs(title = "Histogram of Average Scores with Normal Curve (2006–2012)",
         x = "Average Score", y = "Frequency") +
    ylim(0, ymax) + theme_minimal()
}

print(histogram_function(data_clean, binwidth = 10, ymax = 7000))

# Visualization 2: Boxplot
boxplot_plot <- ggplot(data_clean, aes(x = Demographic, y = Averagescore, fill = Demographic)) +
  geom_boxplot() + scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  labs(title = "Boxplot: Distribution of Mathematics Scores by Gender (2006-2012)",
       x = "Demographic", y = "Average Score") + theme_minimal()

print(boxplot_plot)


# Statistical testing
print("R code for t-test:")
print("t.test(Averagescore ~ Demographic, data = data_clean, var.equal = equal_var)")

var_test <- var.test(Averagescore ~ Demographic, data = data_clean)
equal_var <- var_test$p.value > 0.05

t_test_result <- t.test(Averagescore ~ Demographic, data = data_clean, var.equal = equal_var)

print(paste("Test statistic (t):", round(t_test_result$statistic, 4)))
print(paste("p-value:", format(t_test_result$p.value, scientific = TRUE, digits = 4)))

if(t_test_result$p.value < 0.05) {
  print("p-value < 0.05")
  print("Result: Statistically significant")
  print("Decision: Reject the null hypothesis")
} else {
  print("p-value > 0.05")
  print("Result: Not statistically significant")
  print("Decision: Fail to reject the null hypothesis")
}

