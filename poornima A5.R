# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

# Load the data
file_path <- '/Users/Poornima/Downloads/NSSO68.csv'
data <- read_csv(file_path)

# Strip whitespace from column names
colnames(data) <- trimws(colnames(data))

# Print the cleaned column names
print(colnames(data))

# Define the columns to be used
state_column <- 'state'
district_column <- 'State_Region'
consumption_column <- 'foodtotal_v'  # This can be changed to 'Beveragestotal_v' or 'fv_tot' based on your requirement

# Filter the data for Karnataka
karnataka_state_code <- 29  # Assuming 29 is the state code for Karnataka
karnataka_data <- data %>% filter(!!sym(state_column) == karnataka_state_code)

# Replace infinite values with NA
karnataka_data <- karnataka_data %>% mutate(across(everything(), ~ifelse(. %in% c(Inf, -Inf), NA, .)))

# Verify if columns exist
if (district_column %in% colnames(karnataka_data) & consumption_column %in% colnames(karnataka_data)) {
  print(paste("Columns", district_column, "and", consumption_column, "found in the dataset."))
  
  # Drop rows with NA values in the specified columns
  karnataka_data <- karnataka_data %>% drop_na(!!sym(consumption_column), !!sym(district_column))
  
  # Plotting the histogram
  ggplot(karnataka_data, aes(x = !!sym(consumption_column))) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7, color = "black") +
    geom_density(alpha = 0.2, fill = "red") +
    labs(title = 'Distribution of Total Consumption Across Different Districts in Karnataka', 
         x = consumption_column, 
         y = 'Frequency') +
    theme_minimal()
  
  # Plotting the barplot
  ggplot(karnataka_data, aes(x = as.factor(!!sym(district_column)), y = !!sym(consumption_column))) +
    geom_bar(stat = "summary", fun = "mean", fill = "skyblue", color = "black") +
    labs(title = 'Consumption per District in Karnataka', 
         x = 'State_Region', 
         y = consumption_column) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
} else {
  print(paste("Check the column names. Available columns are:", paste(colnames(karnataka_data), collapse = ", ")))
}
--------------------------------------------------------------------------------------------------
  # Set the working directory and verify it
  setwd("/Users/poornimat/Downloads")
print(paste("Current working directory:", getwd()))

# Load required libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# List of required libraries
libraries <- c("dplyr", "ggplot2", "sf")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68.csv")

# Filtering for KA (Karnataka)
df <- subset(data, state_1 == "KA")

# Display dataset info
print("Dataset Information:")
print(colnames(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
print("Missing Values Information:")
print(missing_info)

# Subsetting the data
ka_new <- df %>% select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
ka_new$Meals_At_Home[is.na(ka_new$Meals_At_Home)] <- mean(ka_new$Meals_At_Home, na.rm = TRUE)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - 1.5 * IQR
  upper_threshold <- Q3 + 1.5 * IQR
  df[df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold, ]
}

outlier_columns <- c('ricepds_v', 'chicken_q')
for (col in outlier_columns) {
  ka_new <- remove_outliers(ka_new, col)
}

# Summarize consumption
ka_new$total_consumption <- rowSums(ka_new[c('ricepds_v', 'Wheatpds_q', 'chicken_q', 'pulsep_q', 'wheatos_q')], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(df, group_col) {
  summary <- df %>% group_by(!!sym(group_col)) %>% summarize(total_consumption = sum(total_consumption)) %>% arrange(desc(total_consumption))
  return(summary)
}

district_summary <- summarize_consumption(ka_new, 'District')
region_summary <- summarize_consumption(ka_new, 'Region')

print("Top Consuming Districts:")
print(head(district_summary, 4))
print("Region Consumption Summary:")
print(region_summary)

# Rename districts and sectors
district_mapping <- c("1" = "North West", "2" = "North", "3" = "North East", "4" = "East", "5" = "New Delhi", "6" = "Central Delhi", "7" = "West", "8" = "South West", "9" = "South")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

ka_new$District <- as.character(ka_new$District)
ka_new$Sector <- as.character(ka_new$Sector)
ka_new$District <- recode(ka_new$District, !!!district_mapping)
ka_new$Sector <- recode(ka_new$Sector, !!!sector_mapping)

# Display the updated dataframe
print(ka_new)

# Plotting the histogram
ggplot(ka_new, aes(x = total_consumption)) +
  geom_histogram(bins = 10, fill = 'blue', color = 'black') +
  labs(x = "Consumption", y = "Frequency", title = "Consumption Distribution in Karnataka State")

# Aggregate total consumption by district
KA_consumption <- ka_new %>% group_by(District) %>% summarize(total_consumption = sum(total_consumption))
print("KA_consumption DataFrame:")
print(KA_consumption)

# Bar plot
ggplot(KA_consumption, aes(x = District, y = total_consumption)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(x = "District", y = "Total Consumption", title = "Total Consumption per District") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


