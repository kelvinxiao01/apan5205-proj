# Data Cleaning Script for World Bank Datasets
# Author: Data Analysis Team
# Purpose: Clean and combine wb-gender-statistics.csv and wb-world-development.csv

# Load required libraries
library(tidyverse)
library(readr)

# Function to clean and standardize dataset structure
clean_wb_data <- function(filepath, dataset_name) {
  cat("Loading", dataset_name, "from", filepath, "\n")
  
  # Read the dataset
  data <- read_csv(filepath, show_col_types = FALSE)
  
  # Print original structure
  cat("Original dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
  cat("Original column names (first 6):", paste(head(names(data)), collapse = ", "), "\n")
  
  # Standardize column order to: Country Name, Country Code, Series Name, Series Code, then years
  # Check if columns need reordering
  if (names(data)[1] == "Series Name") {
    # Reorder columns for gender statistics dataset
    data <- data %>%
      select(`Country Name`, `Country Code`, `Series Name`, `Series Code`, everything())
  }
  
  # Get year columns (those that contain [YR])
  year_cols <- names(data)[grepl("\\[YR\\d{4}\\]", names(data))]
  
  # Extract year from column names and sort them
  year_order <- year_cols[order(as.numeric(gsub(".*\\[YR(\\d{4})\\].*", "\\1", year_cols)))]
  
  # Reorder the entire dataset with years in ascending order
  other_cols <- names(data)[!grepl("\\[YR\\d{4}\\]", names(data))]
  data <- data %>%
    select(all_of(other_cols), all_of(year_order))
  
  # Clean up the data
  data <- data %>%
    # Remove completely empty rows
    filter(!if_all(everything(), is.na)) %>%
    # Convert ".." to NA (World Bank uses ".." for missing values)
    mutate(across(everything(), ~ifelse(. == "..", NA, .))) %>%
    # Convert year columns to numeric
    mutate(across(all_of(year_order), as.numeric))
  
  # Add source column to track which dataset each row came from
  data$source_dataset <- dataset_name
  
  cat("Cleaned dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
  cat("Year columns range:", min(year_order), "to", max(year_order), "\n\n")
  
  return(data)
}

# Load and clean both datasets
cat("=== CLEANING WORLD BANK DATASETS ===\n\n")

# Clean gender statistics dataset
gender_data <- clean_wb_data("wb-gender-statistics.csv", "Gender Statistics")

# Clean world development dataset
development_data <- clean_wb_data("wb-world-development.csv", "World Development")

# Combine the datasets
cat("=== COMBINING DATASETS ===\n")
cat("Gender statistics rows:", nrow(gender_data), "\n")
cat("World development rows:", nrow(development_data), "\n")

# Ensure both datasets have the same column structure before combining
common_cols <- intersect(names(gender_data), names(development_data))
cat("Common columns:", length(common_cols), "\n")

# Combine datasets
combined_data <- bind_rows(
  gender_data %>% select(all_of(common_cols)),
  development_data %>% select(all_of(common_cols))
)

cat("Combined dataset dimensions:", nrow(combined_data), "rows x", ncol(combined_data), "columns\n")

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Unique countries:", length(unique(combined_data$`Country Name`)), "\n")
cat("Unique series:", length(unique(combined_data$`Series Name`)), "\n")
cat("Records by source:\n")
print(table(combined_data$source_dataset))

# Check for missing values in key columns
cat("\nMissing values in key columns:\n")
cat("Country Name:", sum(is.na(combined_data$`Country Name`)), "\n")
cat("Country Code:", sum(is.na(combined_data$`Country Code`)), "\n")
cat("Series Name:", sum(is.na(combined_data$`Series Name`)), "\n")
cat("Series Code:", sum(is.na(combined_data$`Series Code`)), "\n")

# Calculate missing values across all year columns
year_cols <- names(combined_data)[grepl("\\[YR\\d{4}\\]", names(combined_data))]
missing_by_year <- combined_data %>%
  select(all_of(year_cols)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "year", values_to = "missing_count") %>%
  mutate(year = as.numeric(gsub(".*\\[YR(\\d{4})\\].*", "\\1", year))) %>%
  arrange(year)

cat("\nMissing values by year (first 10 years):\n")
print(head(missing_by_year, 10))

# Save the combined dataset
write_csv(combined_data, "combined_wb_data.csv")
cat("\nCombined dataset saved as 'combined_wb_data.csv'\n")

# Display sample of the combined data
cat("\n=== SAMPLE OF COMBINED DATA ===\n")
cat("First 5 rows of combined dataset:\n")
print(head(combined_data %>% select(1:8), 5))

cat("\n=== DATA CLEANING COMPLETE ===\n")
cat("Next steps: Review the combined dataset and proceed with missing value imputation\n") 