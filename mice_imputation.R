# MICE Imputation Script for World Bank Combined Dataset
# Author: Data Analysis Team
# Purpose: Implement Multiple Imputation by Chained Equations (MICE) for missing values
# 
# MICE is a sophisticated imputation method that:
# 1. Models relationships between variables
# 2. Creates multiple imputed datasets to capture uncertainty
# 3. Uses iterative chained equations to refine imputations
# 4. Preserves statistical properties of the original data

# Load required libraries
library(tidyverse)    # For data manipulation
library(readr)        # For reading CSV files
library(mice)         # For MICE imputation
library(VIM)          # For missing value visualization
library(corrplot)     # For correlation plots
library(reshape2)     # For data reshaping

cat("=== MICE IMPUTATION PROCESS STARTED ===\n\n")

# ===============================================================================
# STEP 1: LOAD AND PREPARE DATA
# ===============================================================================

# Load the mean imputed dataset as our base
if (!file.exists("wb_data_mean_imputed.csv")) {
  stop("Mean imputed dataset not found. Please run data cleaning first.")
}

data <- read_csv("wb_data_mean_imputed.csv", show_col_types = FALSE)
cat("Loaded dataset:", nrow(data), "rows x", ncol(data), "columns\n")

# Define key indicators for MICE imputation
key_indicators <- c(
  "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
  "GDP per capita (constant 2015 US$)",
  "Population, total",
  "Infant mortality rate (per 1,000 live births)",
  "Life expectancy at birth, total (years)",
  "School enrollment, primary (% gross)",
  "Access to electricity (% of population)",
  "CO2 emissions (metric tons per capita)",
  "Urban population (% of total population)"
)

# Filter to key indicators
mice_data <- data %>%
  filter(`Series Name` %in% key_indicators) %>%
  select(`Country Name`, `Series Name`, starts_with("20"))

cat("Filtered to key indicators:", nrow(mice_data), "rows\n")
cat("Key indicators included:", length(key_indicators), "\n")

# ===============================================================================
# STEP 2: RESHAPE DATA FOR MICE
# ===============================================================================

cat("\n=== STEP 2: RESHAPING DATA FOR MICE ===\n")

# Get year columns (2010-2023)
year_cols <- names(mice_data)[grepl("^20(1[0-9]|2[0-3]) \\[YR20(1[0-9]|2[0-3])\\]$", names(mice_data))]
cat("Year columns found:", length(year_cols), "\n")

# Reshape to wide format for MICE
mice_wide <- mice_data %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = str_extract(Year, "^20\\d{2}"),
    # Create clean series names for column names
    Series_Clean = case_when(
      str_detect(`Series Name`, "Maternal mortality") ~ "MMR",
      str_detect(`Series Name`, "GDP per capita") ~ "GDP_per_capita",
      str_detect(`Series Name`, "Population, total") ~ "Population",
      str_detect(`Series Name`, "Infant mortality") ~ "Infant_mortality",
      str_detect(`Series Name`, "Life expectancy") ~ "Life_expectancy",
      str_detect(`Series Name`, "School enrollment") ~ "School_enrollment",
      str_detect(`Series Name`, "Access to electricity") ~ "Electricity_access",
      str_detect(`Series Name`, "CO2 emissions") ~ "CO2_emissions",
      str_detect(`Series Name`, "Urban population") ~ "Urban_population",
      TRUE ~ str_replace_all(`Series Name`, "[^A-Za-z0-9]", "_")
    ),
    Series_Year = paste0(Series_Clean, "_", Year)
  ) %>%
  select(`Country Name`, Series_Year, Value) %>%
  pivot_wider(
    names_from = Series_Year,
    values_from = Value
  )

cat("Reshaped data dimensions:", nrow(mice_wide), "rows x", ncol(mice_wide), "columns\n")

# ===============================================================================
# STEP 3: PREPARE FOR MICE IMPUTATION
# ===============================================================================

cat("\n=== STEP 3: PREPARING FOR MICE IMPUTATION ===\n")

# Separate country names and numeric data
country_names <- mice_wide$`Country Name`
mice_numeric <- mice_wide %>%
  select(-`Country Name`) %>%
  mutate(across(everything(), as.numeric))

# Check missing data patterns
total_cells <- nrow(mice_numeric) * ncol(mice_numeric)
missing_cells <- sum(is.na(mice_numeric))
missing_percentage <- (missing_cells / total_cells) * 100

cat("Total cells:", total_cells, "\n")
cat("Missing cells:", missing_cells, "\n")
cat("Missing percentage:", round(missing_percentage, 2), "%\n")

# Remove columns with too many missing values (>90%)
col_missing <- mice_numeric %>%
  summarise(across(everything(), ~sum(is.na(.)) / length(.) * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Pct")

cols_to_keep <- col_missing %>%
  filter(Missing_Pct <= 90) %>%
  pull(Variable)

mice_numeric <- mice_numeric %>%
  select(all_of(cols_to_keep))

cat("Columns after filtering (≤90% missing):", ncol(mice_numeric), "\n")

# ===============================================================================
# STEP 4: PERFORM MICE IMPUTATION
# ===============================================================================

cat("\n=== STEP 4: PERFORMING MICE IMPUTATION ===\n")

# MICE parameters
mice_m <- 5
mice_maxit <- 10
mice_seed <- 123

set.seed(mice_seed)

# Perform MICE imputation
cat("Starting MICE imputation (this may take several minutes)...\n")

start_time <- Sys.time()

mice_result <- mice(
  mice_numeric,
  m = mice_m,
  method = 'pmm',
  maxit = mice_maxit,
  seed = mice_seed,
  printFlag = TRUE
)

end_time <- Sys.time()
cat("MICE imputation completed in", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")

# ===============================================================================
# STEP 5: SAVE INDIVIDUAL IMPUTED DATASETS
# ===============================================================================

cat("\n=== STEP 5: SAVING INDIVIDUAL IMPUTED DATASETS ===\n")

# Save individual imputed datasets
for (i in 1:mice_m) {
  # Extract imputed dataset
  imputed_data <- complete(mice_result, i)
  
  # Add country names back
  imputed_data$`Country Name` <- country_names
  
  # Reshape back to original format
  imputed_long <- imputed_data %>%
    select(`Country Name`, everything()) %>%
    pivot_longer(
      cols = -`Country Name`,
      names_to = "Series_Year",
      values_to = "Value"
    ) %>%
    separate(Series_Year, into = c("Series_Clean", "Year"), sep = "_(?=20\\d{2}$)") %>%
    mutate(
      `Series Name` = case_when(
        Series_Clean == "MMR" ~ "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
        Series_Clean == "GDP_per_capita" ~ "GDP per capita (constant 2015 US$)",
        Series_Clean == "Population" ~ "Population, total",
        Series_Clean == "Infant_mortality" ~ "Infant mortality rate (per 1,000 live births)",
        Series_Clean == "Life_expectancy" ~ "Life expectancy at birth, total (years)",
        Series_Clean == "School_enrollment" ~ "School enrollment, primary (% gross)",
        Series_Clean == "Electricity_access" ~ "Access to electricity (% of population)",
        Series_Clean == "CO2_emissions" ~ "CO2 emissions (metric tons per capita)",
        Series_Clean == "Urban_population" ~ "Urban population (% of total population)",
        TRUE ~ Series_Clean
      ),
      Year_Col = paste0(Year, " [YR", Year, "]")
    ) %>%
    select(`Country Name`, `Series Name`, Year_Col, Value) %>%
    pivot_wider(
      names_from = Year_Col,
      values_from = Value
    )
  
  # Save individual dataset
  write_csv(imputed_long, paste0("wb_data_mice_imputed_", i, ".csv"))
  cat("Saved imputed dataset", i, "\n")
}

# ===============================================================================
# STEP 6: CREATE POOLED DATASET
# ===============================================================================

cat("\n=== STEP 6: CREATING POOLED DATASET ===\n")

# Create pooled estimates by averaging across all imputations
pooled_numeric <- mice_numeric
for (col in names(mice_numeric)) {
  # Get values from all imputations for this column
  col_values <- sapply(1:mice_m, function(i) complete(mice_result, i)[[col]])
  # Calculate row-wise means
  pooled_numeric[[col]] <- rowMeans(col_values, na.rm = TRUE)
}

# Add country names back
pooled_data <- pooled_numeric
pooled_data$`Country Name` <- country_names

# Reshape back to original format
pooled_long <- pooled_data %>%
  select(`Country Name`, everything()) %>%
  pivot_longer(
    cols = -`Country Name`,
    names_to = "Series_Year",
    values_to = "Value"
  ) %>%
  separate(Series_Year, into = c("Series_Clean", "Year"), sep = "_(?=20\\d{2}$)") %>%
  mutate(
    `Series Name` = case_when(
      Series_Clean == "MMR" ~ "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
      Series_Clean == "GDP_per_capita" ~ "GDP per capita (constant 2015 US$)",
      Series_Clean == "Population" ~ "Population, total",
      Series_Clean == "Infant_mortality" ~ "Infant mortality rate (per 1,000 live births)",
      Series_Clean == "Life_expectancy" ~ "Life expectancy at birth, total (years)",
      Series_Clean == "School_enrollment" ~ "School enrollment, primary (% gross)",
      Series_Clean == "Electricity_access" ~ "Access to electricity (% of population)",
      Series_Clean == "CO2_emissions" ~ "CO2 emissions (metric tons per capita)",
      Series_Clean == "Urban_population" ~ "Urban population (% of total population)",
      TRUE ~ Series_Clean
    ),
    Year_Col = paste0(Year, " [YR", Year, "]")
  ) %>%
  select(`Country Name`, `Series Name`, Year_Col, Value) %>%
  pivot_wider(
    names_from = Year_Col,
    values_from = Value
  )

# Save pooled dataset
write_csv(pooled_long, "wb_data_mice_pooled.csv")
cat("Saved pooled MICE dataset\n")

# ===============================================================================
# STEP 7: CREATE DIAGNOSTIC PLOTS
# ===============================================================================

cat("\n=== STEP 7: CREATING DIAGNOSTIC PLOTS ===\n")

# Convergence plot
png("mice_convergence.png", width = 800, height = 600)
plot(mice_result)
dev.off()

# Missing data patterns
png("mice_missing_patterns.png", width = 12, height = 8, units = "in", res = 300)
VIM::aggr(mice_numeric, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE)
dev.off()

# Stripplot
png("mice_stripplot.png", width = 1000, height = 600)
stripplot(mice_result, pch = 20, cex = 1.2)
dev.off()

# Density plots
png("mice_density_plots.png", width = 1200, height = 800)
tryCatch({
  densityplot(mice_result)
}, error = function(e) {
  plot(1, 1, type = "n", main = "Density plot not available")
  text(1, 1, "Could not generate density plot", cex = 1.2)
})
dev.off()

cat("Diagnostic plots saved\n")

# ===============================================================================
# STEP 8: SUMMARY
# ===============================================================================

cat("\n=== STEP 8: SUMMARY ===\n")

cat("MICE Imputation Summary:\n")
cat("- Countries processed:", length(country_names), "\n")
cat("- Indicators included:", length(key_indicators), "\n")
cat("- Years covered: 2010-2023\n")
cat("- Missing percentage before MICE:", round(missing_percentage, 2), "%\n")
cat("- Number of imputed datasets:", mice_m, "\n")
cat("- Imputation method: Predictive Mean Matching (PMM)\n")

cat("\nFiles created:\n")
for (i in 1:mice_m) {
  cat("- wb_data_mice_imputed_", i, ".csv\n")
}
cat("- wb_data_mice_pooled.csv (recommended for analysis)\n")
cat("- mice_convergence.png\n")
cat("- mice_missing_patterns.png\n")
cat("- mice_stripplot.png\n")
cat("- mice_density_plots.png\n")

cat("\n=== MICE IMPUTATION PROCESS COMPLETED ===\n")
cat("Use wb_data_mice_pooled.csv for your analysis.\n")
cat("This dataset contains properly imputed values using MICE methodology.\n") 