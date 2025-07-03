# Missing Value Imputation Script for World Bank Combined Dataset
# Author: Data Analysis Team
# Purpose: Analyze and impute missing values in the combined World Bank dataset

# Load required libraries
library(tidyverse)
library(readr)
library(VIM)        # For missing value visualization
library(mice)       # For multiple imputation
library(Hmisc)      # For imputation functions
library(zoo)        # For time series imputation

# Function to analyze missing data patterns
analyze_missing_data <- function(data) {
  cat("=== MISSING DATA ANALYSIS ===\n\n")
  
  # Overall missing data summary
  total_cells <- nrow(data) * ncol(data)
  missing_cells <- sum(is.na(data))
  missing_percentage <- (missing_cells / total_cells) * 100
  
  cat("Total cells:", total_cells, "\n")
  cat("Missing cells:", missing_cells, "\n")
  cat("Overall missing percentage:", round(missing_percentage, 2), "%\n\n")
  
  # Missing data by column
  missing_by_col <- data %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    mutate(missing_percentage = (missing_count / nrow(data)) * 100) %>%
    arrange(desc(missing_count))
  
  cat("Columns with highest missing values:\n")
  print(head(missing_by_col, 10))
  
  # Missing data by series (indicator)
  missing_by_series <- data %>%
    group_by(`Series Name`) %>%
    summarise(
      total_observations = n(),
      year_cols = sum(!is.na(select(cur_data(), matches("\\[YR\\d{4}\\]")))),
      missing_years = sum(is.na(select(cur_data(), matches("\\[YR\\d{4}\\]")))),
      missing_percentage = (missing_years / (n() * 25)) * 100  # 25 years
    ) %>%
    arrange(desc(missing_percentage))
  
  cat("\nSeries with highest missing percentages:\n")
  print(head(missing_by_series, 10))
  
  return(list(
    overall_missing = missing_percentage,
    missing_by_col = missing_by_col,
    missing_by_series = missing_by_series
  ))
}

# Function to impute missing values using multiple strategies
impute_missing_values <- function(data, method = "comprehensive") {
  cat("=== MISSING VALUE IMPUTATION ===\n\n")
  
  # Get year columns
  year_cols <- names(data)[grepl("\\[YR\\d{4}\\]", names(data))]
  other_cols <- names(data)[!grepl("\\[YR\\d{4}\\]", names(data))]
  
  # Create a copy for imputation
  imputed_data <- data
  
  if (method == "comprehensive") {
    cat("Using comprehensive imputation strategy...\n")
    
    # Strategy 1: Linear interpolation for time series data
    cat("1. Applying linear interpolation for time series...\n")
    imputed_data <- imputed_data %>%
      group_by(`Country Name`, `Series Name`) %>%
      mutate(across(all_of(year_cols), ~na.approx(., na.rm = FALSE, rule = 2))) %>%
      ungroup()
    
    # Strategy 2: Forward/backward fill for remaining missing values
    cat("2. Applying forward/backward fill...\n")
    imputed_data <- imputed_data %>%
      group_by(`Country Name`, `Series Name`) %>%
      mutate(across(all_of(year_cols), ~na.fill(., "extend"))) %>%
      ungroup()
    
    # Strategy 3: Use regional/income group means for still missing values
    cat("3. Applying group-based imputation...\n")
    imputed_data <- imputed_data %>%
      group_by(`Series Name`) %>%
      mutate(across(all_of(year_cols), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
      ungroup()
    
  } else if (method == "remove_na") {
    cat("Removing rows with any missing values...\n")
    imputed_data <- data %>%
      filter(if_all(all_of(year_cols), ~!is.na(.)))
  }
  
  return(imputed_data)
}

# Function to create a clean dataset by removing problematic rows/columns
create_clean_dataset <- function(data, max_missing_percentage = 50) {
  cat("=== CREATING CLEAN DATASET ===\n\n")
  
  year_cols <- names(data)[grepl("\\[YR\\d{4}\\]", names(data))]
  
  # Remove series with too many missing values
  series_missing <- data %>%
    group_by(`Series Name`) %>%
    summarise(
      missing_percentage = (sum(is.na(select(cur_data(), all_of(year_cols)))) / 
                           (n() * length(year_cols))) * 100
    ) %>%
    filter(missing_percentage <= max_missing_percentage)
  
  cat("Keeping", nrow(series_missing), "series with <=", max_missing_percentage, "% missing values\n")
  
  # Filter the dataset
  clean_data <- data %>%
    filter(`Series Name` %in% series_missing$`Series Name`)
  
  # Remove rows where all year values are missing
  clean_data <- clean_data %>%
    filter(!if_all(all_of(year_cols), is.na))
  
  cat("Clean dataset dimensions:", nrow(clean_data), "rows x", ncol(clean_data), "columns\n")
  
  return(clean_data)
}

# Main execution
cat("=== STARTING MISSING VALUE IMPUTATION PROCESS ===\n\n")

# Load the combined dataset
if (!file.exists("combined_wb_data.csv")) {
  stop("Combined dataset not found. Please run data_cleaning.R first.")
}

combined_data <- read_csv("combined_wb_data.csv", show_col_types = FALSE)
cat("Loaded combined dataset:", nrow(combined_data), "rows x", ncol(combined_data), "columns\n\n")

# Analyze missing data patterns
missing_analysis <- analyze_missing_data(combined_data)

# Create different versions of the dataset

# Version 1: Complete cases only (remove all rows with any NA)
cat("\n=== CREATING COMPLETE CASES DATASET ===\n")
year_cols <- names(combined_data)[grepl("\\[YR\\d{4}\\]", names(combined_data))]
complete_cases_data <- combined_data %>%
  filter(if_all(all_of(year_cols), ~!is.na(.)))

cat("Complete cases dataset:", nrow(complete_cases_data), "rows x", ncol(complete_cases_data), "columns\n")
cat("Percentage of original data retained:", round((nrow(complete_cases_data) / nrow(combined_data)) * 100, 2), "%\n")

# Version 2: Clean dataset (remove series with >50% missing, then impute)
clean_data <- create_clean_dataset(combined_data, max_missing_percentage = 50)

# Version 3: Imputed dataset using comprehensive strategy
cat("\n=== APPLYING COMPREHENSIVE IMPUTATION ===\n")
imputed_data <- impute_missing_values(clean_data, method = "comprehensive")

# Check imputation results
cat("\nImputation results:\n")
cat("Original missing values:", sum(is.na(clean_data)), "\n")
cat("Remaining missing values:", sum(is.na(imputed_data)), "\n")
cat("Imputation success rate:", round((1 - sum(is.na(imputed_data)) / sum(is.na(clean_data))) * 100, 2), "%\n")

# Save all versions
write_csv(complete_cases_data, "wb_data_complete_cases.csv")
write_csv(clean_data, "wb_data_clean.csv")
write_csv(imputed_data, "wb_data_imputed.csv")

cat("\n=== DATASETS SAVED ===\n")
cat("1. wb_data_complete_cases.csv - Only complete cases (no missing values)\n")
cat("2. wb_data_clean.csv - Filtered dataset (removed high-missing series)\n")
cat("3. wb_data_imputed.csv - Imputed dataset (missing values filled)\n")

# Summary comparison
cat("\n=== DATASET COMPARISON ===\n")
datasets_summary <- data.frame(
  Dataset = c("Original", "Complete Cases", "Clean", "Imputed"),
  Rows = c(nrow(combined_data), nrow(complete_cases_data), nrow(clean_data), nrow(imputed_data)),
  Columns = c(ncol(combined_data), ncol(complete_cases_data), ncol(clean_data), ncol(imputed_data)),
  Missing_Values = c(sum(is.na(combined_data)), sum(is.na(complete_cases_data)), 
                    sum(is.na(clean_data)), sum(is.na(imputed_data))),
  Missing_Percentage = c(
    round((sum(is.na(combined_data)) / (nrow(combined_data) * ncol(combined_data))) * 100, 2),
    round((sum(is.na(complete_cases_data)) / (nrow(complete_cases_data) * ncol(complete_cases_data))) * 100, 2),
    round((sum(is.na(clean_data)) / (nrow(clean_data) * ncol(clean_data))) * 100, 2),
    round((sum(is.na(imputed_data)) / (nrow(imputed_data) * ncol(imputed_data))) * 100, 2)
  )
)

print(datasets_summary)

# Recommendations
cat("\n=== RECOMMENDATIONS ===\n")
cat("1. For analysis requiring complete data: Use 'wb_data_complete_cases.csv'\n")
cat("2. For analysis tolerating some missing values: Use 'wb_data_clean.csv'\n")
cat("3. For analysis requiring full time series: Use 'wb_data_imputed.csv'\n")
cat("4. Consider the trade-offs between data completeness and sample size\n")

cat("\n=== MISSING VALUE IMPUTATION COMPLETE ===\n") 