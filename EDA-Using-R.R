setwd("D:\\R\\R_PROJECTS\\Portfolio-Exploratory Data Analysis using R")
getwd()

#### Setting and loading the data
# Load the necessary libraries
library(readr)

# Set the file path
file_path <- "healthcare_dataset.csv"

# Load the data from the CSV file
data <- read_csv(file_path)

# Print the first few rows of the data to check
head(data)

# Print the structure (categorical or Numerical) of the data 
str(data)

names(data)

# Print the dimensions of the data (number of rows and columns)
dim(data)

View(data)



#####################################################
## DATA CLEANING AND PREPROCESSING
########################################


# Check for missing values
missing_values <- colSums(is.na(data))

# Display columns with missing values
missing_values[missing_values > 0]

# Impute missing values (replace with mean, median, or any other appropriate method)
# For example, imputing missing values in Age column with median
data$Age[is.na(data$Age)] <- median(data$Age, na.rm = TRUE)

# Or remove rows with missing values
# For example, removing rows with missing values in Blood Type column
data <- data[complete.cases(data$`Blood Type`), ]

# Check if missing values have been handled
# Print the dimensions of the data to check if rows have been removed
dim(data)




############################################
# Handle Duplicate values
# Detect and remove duplicate records
duplicates <- data[duplicated(data), ]
if (nrow(duplicates) > 0) {
  cat("Number of duplicate records detected:", nrow(duplicates), "\n")
  
  # Remove duplicate records
  data <- unique(data)
  
  cat("Duplicate records removed.\n")
} else {
  cat("No duplicate records detected.\n")
}


#####################################
# Data Transformations

# Convert data into appropriate formats and perform feature engineering
# For example, converting categorical variables into factors
data$Gender <- as.factor(data$Gender)
data$`Blood Type` <- as.factor(data$`Blood Type`)
data$`Medical Condition` <- as.factor(data$`Medical Condition`)
data$`Admission Type` <- as.factor(data$`Admission Type`)
data$`Test Results` <- as.factor(data$`Test Results`)

# Create new variables or perform feature engineering
# For example, create a new variable Age_Group based on Age
data$Age_Group <- cut(data$Age, breaks = c(0, 18, 40, 60, Inf), labels = c("0-18", "19-40", "41-60", "60+"))

# You can add more feature engineering steps here as needed

# Print the first few rows of the transformed data
head(data)


########################################
# DatA normalization or Scaling
# Check if there are numerical variables that need normalization
numeric_columns <- sapply(data, is.numeric)

# Normalize numerical features using z-score standardization
data_scaled <- as.data.frame(scale(data[, numeric_columns]))

# Combine normalized numerical features with categorical features
data_scaled <- cbind(data[, !numeric_columns], data_scaled)

# Print the first few rows of the scaled data
head(data_scaled)


# INITIAL PLOTS
# Load the necessary libraries
library(ggplot2)

# Plot histogram of Age
AgeHistogram <- ggplot(data, aes(x = Age)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")
# Save the plot with specific dimensions
ggsave("AgeHistogram.png", AgeHistogram, width = 10, height = 7.5, units = "in", dpi = 300)

# Plot bar plot for Gender
GenderBarPlot <- ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Gender", x = "Gender", y = "Count")
ggsave("GenderBarPlot.png", GenderBarPlot, width = 10, height = 7.5, units = "in", dpi = 300)

# Plot bar plot for Blood Type
BloodTypeBarPlot <- ggplot(data, aes(x = `Blood Type`)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Blood Type", x = "Blood Type", y = "Count")
ggsave("BloodTypeBarPlot.png", BloodTypeBarPlot, width = 10, height = 7.5, units = "in", dpi = 300)

# Plot bar plot for Medical Condition
ConditionBarPlot <- ggplot(data, aes(x = `Medical Condition`)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Medical Condition", x = "Medical Condition", y = "Count")
ggsave("ConditionBarPlot.png", ConditionBarPlot, width = 10, height = 7.5, units = "in", dpi = 300)


# Plot bar plot for Admission Type
AdmissionTypeBarPlot <- ggplot(data, aes(x = `Admission Type`)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Admission Type", x = "Admission Type", y = "Count")
ggsave("AdmissionTypeBarPlot.png", AdmissionTypeBarPlot, width = 10, height = 7.5, units = "in", dpi = 300)

# Plot bar plot for Test Results
ResultBarPlot <- ggplot(data, aes(x = `Test Results`)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Test Results", x = "Test Results", y = "Count")
ggsave("ResultBarPlot.png", ResultBarPlot, width = 10, height = 7.5, units = "in", dpi = 300)




###############################################
# EXPLORORATORY DATA ANALYSIS

# Summarize the main characteristics of the data
summary(data)


# Compute mean, median, mode, standard deviation, range for specific variables
mean_age <- mean(data$Age)
median_age <- median(data$Age)
sd_age <- sd(data$Age)
min_age <- min(data$Age)
max_age <- max(data$Age)

cat("Mean Age:", mean_age, "\n")
cat("Median Age:", median_age, "\n")
cat("Standard Deviation of Age:", sd_age, "\n")
cat("Minimum Age:", min_age, "\n")
cat("Maximum Age:", max_age, "\n")

# Compute frequency of unique values for a categorical variable
table(data$Gender)


#############################################################
# Visulization

# Load necessary library
library(ggplot2)
library(gridExtra)

# Histogram for Age
hist_age <- ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

# Box plot for Age by Gender
boxplot_age_gender <- ggplot(data, aes(x = Gender, y = Age, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Gender", x = "Gender", y = "Age") +
  theme(legend.position = "none")

# Scatter plot for Age and Billing Amount
scatter_age_billing <- ggplot(data, aes(x = Age, y = `Billing Amount`)) +
  geom_point(color = "skyblue") +
  labs(title = "Scatter Plot of Age vs Billing Amount", x = "Age", y = "Billing Amount")

# Compute correlation matrix
correlation_matrix <- cor(data[, sapply(data, is.numeric)])

# Reshape the correlation matrix
corr_df <- as.data.frame(as.table(correlation_matrix))
names(corr_df) <- c("Var1", "Var2", "Correlation")

# Create correlation matrix plot
gg_correlation <- ggplot(corr_df, aes(Var1, Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", mid = "steelblue", high = "red", 
                       midpoint = 0, limit = c(-1,1), na.value = "grey50") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  labs(title = "Correlation Matrix", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

# Arrange plots in a grid
ArrangedPlots <- grid.arrange(hist_age, boxplot_age_gender, scatter_age_billing, gg_correlation, ncol = 2)

ggsave("ArrangedPlots.png", ArrangedPlots, width = 10, height = 7.5, units = "in", dpi = 300)



####################################
# OUTLIERS

# Function to identify outliers using IQR method
identify_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

# Identify outliers for Age
outliers_age <- identify_outliers(data$Age)

# Identify outliers for Billing Amount
outliers_billing <- identify_outliers(data$`Billing Amount`)

# Check if outliers are found and print the result
if (length(outliers_age) == 0 && length(outliers_billing) == 0) {
  cat("No outliers\n")
} else {
  cat("Outliers in Age:", outliers_age, "\n")
  cat("Outliers in Billing Amount:", outliers_billing, "\n")
}

############################################
# TEst result is abnormal if admitted in emergency

# Subset data to include only relevant variables
relevant_data <- data[, c("Admission Type", "Test Results")]

# Create contingency table
contingency_table <- table(relevant_data$`Admission Type`, relevant_data$`Test Results`)

# Perform Chi-squared test
chi_square_test <- chisq.test(contingency_table)

# Print results
print(chi_square_test)







