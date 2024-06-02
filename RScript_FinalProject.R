# Load necessary libraries
library(ggplot2)
library(dplyr)
options(scipen=999) #to avoid scientefic notation
library(readxl)
library(GGally)

install.packages("sf")
library(sf)

install.packages("lmtest")
library(lmtest)

# Load the dataset
cancer_data <- read_excel("/Users/vaishnavishirsekar/Documents/Data Analytics & BI/AFinal/Cancer.xlsx")

# View the first few rows of the dataset
head(cancer_data)

# Summary of the dataset
summary(cancer_data)

#Q1-c Scatter plot with state as color and size mapped to population estimate
ggplot(cancer_data, aes(x = popEst2015, y = incidenceRate, color = State, size = medIncome)) +
  geom_point(alpha = 0.7) +  # Add transparency to points
  scale_color_manual(values = rainbow(length(unique(cancer_data$State)))) +  # Color scale for states
  labs(x = "Population Estimate 2015", y = "Incidence Rate", color = "State", size = "Median Income") +
  theme_minimal()  # Minimalistic theme

#Q1-d Boxplot of cancer rates
ggplot(cancer_data, aes(y = incidenceRate)) +
  geom_boxplot(fill = "skyblue") +
  labs(y = "Incidence Rate") +
  ggtitle("Distribution of Cancer Incidence Rates")
# Identify outliers
outliers <- boxplot.stats(cancer_data$incidenceRate)$out
# Scatter plot highlighting outliers
ggplot(cancer_data, aes(x = medIncome, y = incidenceRate, color = State)) +
  geom_point(alpha = 0.7) +
  geom_point(data = subset(cancer_data, incidenceRate %in% outliers), color = "red", size = 3) +
  labs(x = "Median Income", y = "Incidence Rate", color = "State") +
  ggtitle("Cancer Incidence Rates vs. Median Income") +
  theme_minimal()

#Q1.e->1 Descriptive statistics for Median Income 
#Define a function to calculate the mode (the most frequent value)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
median_income_mode <- get_mode(cancer_data$medIncome)
poverty_mode <- get_mode(cancer_data$PovertyEst)
cancer_rate_mode <- get_mode(cancer_data$incidenceRate)

# Descriptive Statistics for Compute additional statistics like variance and standard deviation for medIncome.
summary(cancer_data$medIncome)
var(cancer_data$medIncome)
sd(cancer_data$medIncome)
cat("Mode of Median Income: ", median_income_mode, "\n")


# Descriptive statistics for PovertyEst, Compute additional statistics like variance and standard deviation for PovertyEst.
summary(cancer_data$PovertyEst)
var(cancer_data$PovertyEst)
sd(cancer_data$PovertyEst)
cat("Mode of Poverty Estimates: ", poverty_mode, "\n")


# Descriptive statistics for cancerRate, Compute additional statistics like variance and standard deviation for cancerRate.
summary(cancer_data$incidenceRate)
var(cancer_data$incidenceRate)
sd(cancer_data$incidenceRate)
cat("Mode of Cancer Rates: ", cancer_rate_mode, "\n")


# Q1.f-> Analyze central tendency and dispersion by state
cancer_data %>%
  group_by(State) %>%
  summarise(Mean_Incidence = mean(incidenceRate), 
            SD_Incidence = sd(incidenceRate),
            Mean_MedIncome = mean(medIncome),
            SD_MedIncome = sd(medIncome))


# Q1.g-> Generate summary tables categorizing counties based on cancer rates and median income
cancer_data %>%
  mutate(Income_Quantile = ntile(medIncome, 4),
         Cancer_Rate_Quantile = ntile(incidenceRate, 4)) %>%
  group_by(Income_Quantile, Cancer_Rate_Quantile) %>%
  summarise(Count = n())


#Q2.a-> Linear regression model
model <- lm(incidenceRate ~ medIncome + PovertyEst, data = cancer_data)

# Summary of the model
summary(model)


#Q2-c R-squared and Adjusted R-squared
rsquared <- summary(model)$r.squared
adjusted_rsquared <- summary(model)$adj.r.squared
# Residual analysis
residuals <- residuals(model)

# Plot of residuals vs. predicted values for homoscedasticity
plot(model$fitted.values, residuals, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. Fitted")

# Histogram of residuals for normality
hist(residuals, breaks = 20, main = "Histogram of Residuals")

# QQ plot of residuals for normality
qqnorm(residuals)
qqline(residuals)

# Check for homoscedasticity
bptest(model)
# Plot residuals to check for normality
plot(residuals(model))

# Save the plot
ggsave("Cancer_Incidence_Rates_by_State.png", width = 10, height = 8, units = "in")



#correlation Matrix
# Selecting relevant variables for the correlation analysis
selected_data <- cancer_data[, c("PovertyEst", "medIncome", "popEst2015", "incidenceRate")]

# Convert population estimate to thousands for better scale on plot (if needed)
selected_data$popEstThousands <- selected_data$popEst2015 / 1000

# Plotting a scatter plot matrix using GGally
ggpairs(selected_data[, c("PovertyEst", "medIncome", "incidenceRate", "popEstThousands")])

# Computing the correlation matrix
correlation_matrix <- cor(selected_data[, c("PovertyEst", "medIncome", "incidenceRate", "popEstThousands")], use = "complete.obs")

# Plotting the correlation heatmap
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45,  # Text label color and rotation
         addCoef.col = "black")  # Add correlation coefficients to the plot


# Create a summary table to show counts of data points in each quantile combination
summary_table <- cancer_data %>%
  group_by(Income_Quantile, CancerRate_Quantile) %>%
  summarise(Count = n(), .groups = 'drop')  # Automatically drop the grouping

# Create a heatmap using ggplot2
heatmap_plot <- ggplot(summary_table, aes(x = Income_Quantile, y = CancerRate_Quantile, fill = Count)) +
  geom_tile() +  # Creates the tiles for the heatmap
  scale_fill_gradient(low = "blue", high = "red") +  # Color gradient from low to high counts
  labs(title = "Heatmap of Quantile Distribution for Income and Cancer Rates",
       x = "Income Quantile",
       y = "Cancer Rate Quantile",
       fill = "Count") +
  theme_minimal() +  # Minimal theme for cleaner look
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Improve x-axis label readability
        axis.title = element_text(size = 12, face = "bold"))  # Bold and larger axis titles
# Print the heatmap plot
print(heatmap_plot)