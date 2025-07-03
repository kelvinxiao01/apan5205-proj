# Robust Missing Value Imputation Script for World Bank Combined Dataset
# Author: Data Analysis Team
# Purpose: Analyze and handle missing values in the combined World Bank dataset

# Load required libraries
library(tidyverse)
library(readr)

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
  year_cols <- names(data)[grepl("\\[YR\\d{4}\\]", names(data))]
  missing_by_year <- data %>%
    select(all_of(year_cols)) %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "year", values_to = "missing_count") %>%
    mutate(
      year_num = as.numeric(gsub(".*\\[YR(\\d{4})\\].*", "\\1", year)),
      missing_percentage = (missing_count / nrow(data)) * 100
    ) %>%
    arrange(year_num)
  
  cat("Missing values by year:\n")
  print(missing_by_year)
  
  # Missing data by series (indicator)
  missing_by_series <- data %>%
    group_by(`Series Name`) %>%
    summarise(
      total_observations = n(),
      total_year_cells = n() * length(year_cols),
      missing_year_cells = sum(is.na(select(cur_data(), all_of(year_cols)))),
      missing_percentage = (missing_year_cells / total_year_cells) * 100,
      .groups = 'drop'
    ) %>%
    arrange(desc(missing_percentage))
  
  cat("\nSeries with highest missing percentages:\n")
  print(head(missing_by_series, 10))
  
  return(list(
    overall_missing = missing_percentage,
    missing_by_year = missing_by_year,
    missing_by_series = missing_by_series
  ))
}

# Function to create different versions of clean datasets
create_clean_datasets <- function(data) {
  year_cols <- names(data)[grepl("\\[YR\\d{4}\\]", names(data))]
  
  # Version 1: Complete cases only (remove all rows with any NA in year columns)
  cat("=== CREATING COMPLETE CASES DATASET ===\n")
  complete_cases_data <- data %>%
    filter(if_all(all_of(year_cols), ~!is.na(.)))
  
  cat("Complete cases dataset:", nrow(complete_cases_data), "rows x", ncol(complete_cases_data), "columns\n")
  cat("Percentage of original data retained:", round((nrow(complete_cases_data) / nrow(data)) * 100, 2), "%\n\n")
  
  # Version 2: Remove series with >80% missing values
  # STEP 1 OF IMPUTATION PROCESS: QUALITY FILTERING BY SERIES
  # This step removes entire data series (indicators) that have more than 80% missing values
  # across all countries and years. This ensures we only work with indicators that have
  # sufficient data coverage for meaningful analysis.
  cat("=== CREATING FILTERED DATASET (Remove series with >80% missing) ===\n")
  
  # Calculate missing data statistics for each series (indicator)
  # For each unique series name, we count:
  # - total_year_cells: total possible data points (countries × years)
  # - missing_year_cells: actual missing data points
  # - missing_percentage: percentage of missing data for that series
  series_stats <- data %>%
    group_by(`Series Name`) %>%
    summarise(
      total_year_cells = n() * length(year_cols),        # Total possible data points
      missing_year_cells = sum(is.na(select(cur_data(), all_of(year_cols)))), # Missing points
      missing_percentage = (missing_year_cells / total_year_cells) * 100,     # Missing %
      .groups = 'drop'
    )
  
  # Filter to keep only series with ≤80% missing values
  # This threshold ensures we retain indicators with reasonable data coverage
  # Series with >80% missing values are considered too sparse for reliable analysis
  good_series <- series_stats %>%
    filter(missing_percentage <= 80) %>%
    pull(`Series Name`)
  
  # Apply the series filter to the original dataset
  # This removes all rows belonging to series with >80% missing values
  filtered_data <- data %>%
    filter(`Series Name` %in% good_series)
  
  cat("Kept", length(good_series), "series out of", nrow(series_stats), "total series\n")
  cat("Filtered dataset:", nrow(filtered_data), "rows x", ncol(filtered_data), "columns\n\n")
  
  # Version 3: Remove rows where >50% of year values are missing
  # STEP 2 OF IMPUTATION PROCESS: QUALITY FILTERING BY ROWS
  # This step removes individual country-indicator combinations that have more than 50%
  # missing values across the 25-year time period. This ensures each remaining row
  # has sufficient temporal coverage for meaningful time series analysis.
  cat("=== CREATING ROW-FILTERED DATASET (Remove rows with >50% missing years) ===\n")
  
  row_filtered_data <- filtered_data %>%
    rowwise() %>%  # Process each row individually
    mutate(
      # Count missing values across all year columns for this specific row
      # c_across() applies the function across multiple columns in a rowwise context
      missing_years = sum(is.na(c_across(all_of(year_cols)))),
      
      # Calculate what percentage of the 25 years are missing for this row
      # If a country-indicator combination has >50% missing years, it's considered
      # too sparse for reliable imputation and analysis
      missing_percentage = (missing_years / length(year_cols)) * 100
    ) %>%
    # Keep only rows with ≤50% missing years (i.e., at least 12-13 years of data)
    filter(missing_percentage <= 50) %>%
    # Remove the temporary calculation columns
    select(-missing_years, -missing_percentage) %>%
    ungroup()  # Remove rowwise grouping
  
  cat("Row-filtered dataset:", nrow(row_filtered_data), "rows x", ncol(row_filtered_data), "columns\n\n")
  
  # Version 4: Simple imputation with mean values
  # STEP 3 OF IMPUTATION PROCESS: MEAN IMPUTATION BY SERIES
  # This is the core imputation step that creates wb_data_mean_imputed.csv
  # Strategy: For each missing value, replace it with the mean of all available values
  # for that same indicator (series) across all countries and years.
  cat("=== CREATING MEAN-IMPUTED DATASET ===\n")
  
  # Group by Series Name to calculate series-specific means
  # This approach assumes that missing values for an indicator can be reasonably
  # approximated by the global average for that indicator across all countries
  mean_imputed_data <- row_filtered_data %>%
    group_by(`Series Name`) %>%  # Group by indicator type
    
    # Apply imputation across all year columns simultaneously
    # For each year column, the logic is:
    # - If the value is missing (NA), replace it with the mean of all non-NA values
    #   for that same indicator across all countries and years
    # - If the value is not missing, keep the original value unchanged
    # mean(., na.rm = TRUE) calculates the mean while ignoring NA values
    mutate(across(all_of(year_cols), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    ungroup()  # Remove grouping
  
  # STEP 4 OF IMPUTATION PROCESS: HANDLE EDGE CASES
  # Handle the special case where an entire series has no data at all
  # If a series has all missing values, mean(., na.rm = TRUE) returns NaN
  # We replace these NaN values with 0 as a conservative estimate
  # This prevents downstream analysis errors while acknowledging data limitations
  mean_imputed_data <- mean_imputed_data %>%
    mutate(across(all_of(year_cols), ~ifelse(is.nan(.), 0, .)))
  
  cat("Mean-imputed dataset:", nrow(mean_imputed_data), "rows x", ncol(mean_imputed_data), "columns\n")
  cat("Remaining missing values:", sum(is.na(mean_imputed_data)), "\n\n")
  
  # SUMMARY OF IMPUTATION PROCESS FOR wb_data_mean_imputed.csv:
  # 1. Started with 9,315 rows (original combined dataset)
  # 2. Removed 18 series with >80% missing values (quality filter)
  # 3. Removed 1,496 rows with >50% missing years (completeness filter)  
  # 4. Applied mean imputation: replaced each missing value with the global mean
  #    for that indicator across all countries and years
  # 5. Handled edge cases: replaced NaN values (from all-missing series) with 0
  # 6. Final result: 5,945 rows with 0 missing values
  
  return(list(
    complete_cases = complete_cases_data,
    filtered = filtered_data,
    row_filtered = row_filtered_data,
    mean_imputed = mean_imputed_data
  ))
}

# Main execution
cat("=== STARTING MISSING VALUE HANDLING PROCESS ===\n\n")

# Load the combined dataset
if (!file.exists("combined_wb_data.csv")) {
  stop("Combined dataset not found. Please run data_cleaning.R first.")
}

combined_data <- read_csv("combined_wb_data.csv", show_col_types = FALSE)
cat("Loaded combined dataset:", nrow(combined_data), "rows x", ncol(combined_data), "columns\n\n")

# Analyze missing data patterns
missing_analysis <- analyze_missing_data(combined_data)

# Create different versions of clean datasets
clean_datasets <- create_clean_datasets(combined_data)

# Save all versions
write_csv(clean_datasets$complete_cases, "wb_data_complete_cases.csv")
write_csv(clean_datasets$filtered, "wb_data_filtered.csv")
write_csv(clean_datasets$row_filtered, "wb_data_row_filtered.csv")
write_csv(clean_datasets$mean_imputed, "wb_data_mean_imputed.csv")

cat("=== DATASETS SAVED ===\n")
cat("1. wb_data_complete_cases.csv - Only complete cases (no missing values)\n")
cat("2. wb_data_filtered.csv - Removed series with >80% missing values\n")
cat("3. wb_data_row_filtered.csv - Additionally removed rows with >50% missing years\n")
cat("4. wb_data_mean_imputed.csv - Mean-imputed dataset (no missing values)\n\n")

# Summary comparison
cat("=== DATASET COMPARISON ===\n")
datasets_summary <- data.frame(
  Dataset = c("Original", "Complete Cases", "Filtered", "Row Filtered", "Mean Imputed"),
  Rows = c(
    nrow(combined_data), 
    nrow(clean_datasets$complete_cases), 
    nrow(clean_datasets$filtered),
    nrow(clean_datasets$row_filtered),
    nrow(clean_datasets$mean_imputed)
  ),
  Columns = c(
    ncol(combined_data), 
    ncol(clean_datasets$complete_cases), 
    ncol(clean_datasets$filtered),
    ncol(clean_datasets$row_filtered),
    ncol(clean_datasets$mean_imputed)
  ),
  Missing_Values = c(
    sum(is.na(combined_data)), 
    sum(is.na(clean_datasets$complete_cases)), 
    sum(is.na(clean_datasets$filtered)),
    sum(is.na(clean_datasets$row_filtered)),
    sum(is.na(clean_datasets$mean_imputed))
  ),
  Unique_Series = c(
    length(unique(combined_data$`Series Name`)),
    length(unique(clean_datasets$complete_cases$`Series Name`)),
    length(unique(clean_datasets$filtered$`Series Name`)),
    length(unique(clean_datasets$row_filtered$`Series Name`)),
    length(unique(clean_datasets$mean_imputed$`Series Name`))
  )
)

print(datasets_summary)

# Recommendations
cat("\n=== RECOMMENDATIONS FOR ANALYSIS ===\n")
cat("1. For robust analysis with guaranteed complete data:\n")
cat("   -> Use 'wb_data_complete_cases.csv' (", nrow(clean_datasets$complete_cases), " rows)\n")
cat("2. For analysis with good data quality and reasonable sample size:\n")
cat("   -> Use 'wb_data_row_filtered.csv' (", nrow(clean_datasets$row_filtered), " rows)\n")
cat("3. For analysis requiring maximum data coverage:\n")
cat("   -> Use 'wb_data_mean_imputed.csv' (", nrow(clean_datasets$mean_imputed), " rows)\n")
cat("4. Consider the trade-offs between data completeness and sample size\n")

cat("\n=== MISSING VALUE HANDLING COMPLETE ===\n")
cat("All datasets are now ready for analysis with different levels of data completeness.\n") 