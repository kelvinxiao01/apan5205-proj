# Load required libraries
library(tidyverse)
library(skimr)

# Set options for better display
options(tibble.width = Inf)

cat("=== RAW DATA DESCRIPTION SCRIPT ===\n")
cat("Loading and describing World Bank datasets in long format\n\n")

# Load datasets
cat("Loading datasets...\n")
gender_stats <- read_csv("wb-gender-statistics.csv", show_col_types = FALSE)
world_dev <- read_csv("wb-world-development.csv", show_col_types = FALSE)

cat("Gender statistics dataset:", nrow(gender_stats), "rows,", ncol(gender_stats), "columns\n")
cat("World development dataset:", nrow(world_dev), "rows,", ncol(world_dev), "columns\n\n")



# Print skim summaries for both raw datasets
cat("=== GENDER STATISTICS DATASET (WIDE FORMAT) ===\n")
print(skim(gender_stats))

cat("\n=== WORLD DEVELOPMENT DATASET (WIDE FORMAT) ===\n")
print(skim(world_dev))

# Identify year columns
year_cols <- names(gender_stats)[str_detect(names(gender_stats), "\\d{4}")]
cat("\nYear columns found:", length(year_cols), "\n")
cat("Year range:", min(str_extract(year_cols, "\\d{4}")), "to", max(str_extract(year_cols, "\\d{4}")), "\n\n")

# Convert Gender Statistics to long format
cat("=== CONVERTING GENDER STATISTICS TO LONG FORMAT ===\n")
gender_stats_long <- gender_stats %>%
  # Remove any completely empty rows
  filter(!is.na(`Series Name`) | !is.na(`Country Name`)) %>%
  # Convert to long format
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year_label",
    values_to = "value"
  ) %>%
  # Extract year as numeric
  mutate(
    year = as.numeric(str_extract(year_label, "\\d{4}")),
    # Convert value to numeric, handling ".." as NA
    value = case_when(
      value == ".." ~ NA_real_,
      is.na(value) ~ NA_real_,
      TRUE ~ as.numeric(value)
    )
  ) %>%
  # Remove year_label column
  select(-year_label) %>%
  # Reorder columns
  select(`Series Name`, `Series Code`, `Country Name`, `Country Code`, year, value)

cat("Gender statistics (long format):", nrow(gender_stats_long), "rows,", ncol(gender_stats_long), "columns\n")

# Convert World Development to long format
cat("\n=== CONVERTING WORLD DEVELOPMENT TO LONG FORMAT ===\n")
world_dev_long <- world_dev %>%
  # Remove any completely empty rows
  filter(!is.na(`Series Name`) | !is.na(`Country Name`)) %>%
  # Convert to long format
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year_label",
    values_to = "value"
  ) %>%
  # Extract year as numeric
  mutate(
    year = as.numeric(str_extract(year_label, "\\d{4}")),
    # Convert value to numeric, handling ".." as NA
    value = case_when(
      value == ".." ~ NA_real_,
      is.na(value) ~ NA_real_,
      TRUE ~ as.numeric(value)
    )
  ) %>%
  # Remove year_label column
  select(-year_label) %>%
  # Reorder columns
  select(`Series Name`, `Series Code`, `Country Name`, `Country Code`, year, value)

cat("World development (long format):", nrow(world_dev_long), "rows,", ncol(world_dev_long), "columns\n")

# Combine both datasets in long format
cat("\n=== COMBINING DATASETS IN LONG FORMAT ===\n")
combined_long <- bind_rows(
  gender_stats_long %>% mutate(dataset = "Gender Statistics"),
  world_dev_long %>% mutate(dataset = "World Development")
)

cat("Combined dataset (long format):", nrow(combined_long), "rows,", ncol(combined_long), "columns\n\n")

# Comprehensive description of long format datasets
cat("=== GENDER STATISTICS DATASET (LONG FORMAT) ===\n")
print(skim(gender_stats_long))

cat("\n=== WORLD DEVELOPMENT DATASET (LONG FORMAT) ===\n")
print(skim(world_dev_long))

cat("\n=== COMBINED DATASET (LONG FORMAT) ===\n")
print(skim(combined_long))

# Analyze unique values and coverage
cat("\n=== DATA COVERAGE ANALYSIS ===\n")

# Series analysis
cat("=== SERIES ANALYSIS ===\n")
series_summary <- combined_long %>%
  group_by(dataset, `Series Name`) %>%
  summarise(
    observations = n(),
    non_missing = sum(!is.na(value)),
    missing = sum(is.na(value)),
    completion_rate = round(non_missing / observations * 100, 2),
    unique_countries = n_distinct(`Country Name`),
    unique_years = n_distinct(year),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(dataset, desc(completion_rate))

cat("Series with highest completion rates:\n")
print(series_summary %>% head(10))

cat("\nSeries with lowest completion rates:\n")
print(series_summary %>% tail(10))

# Country analysis
cat("\n=== COUNTRY ANALYSIS ===\n")
country_summary <- combined_long %>%
  group_by(dataset, `Country Name`) %>%
  summarise(
    observations = n(),
    non_missing = sum(!is.na(value)),
    missing = sum(is.na(value)),
    completion_rate = round(non_missing / observations * 100, 2),
    unique_series = n_distinct(`Series Name`),
    unique_years = n_distinct(year),
    .groups = "drop"
  ) %>%
  arrange(dataset, desc(completion_rate))

cat("Countries with highest completion rates:\n")
print(country_summary %>% head(10))

cat("\nCountries with lowest completion rates:\n")
print(country_summary %>% tail(10))

# Year analysis
cat("\n=== TEMPORAL COVERAGE ANALYSIS ===\n")
year_summary <- combined_long %>%
  group_by(dataset, year) %>%
  summarise(
    observations = n(),
    non_missing = sum(!is.na(value)),
    missing = sum(is.na(value)),
    completion_rate = round(non_missing / observations * 100, 2),
    unique_countries = n_distinct(`Country Name`),
    unique_series = n_distinct(`Series Name`),
    .groups = "drop"
  ) %>%
  arrange(dataset, year)

cat("Temporal coverage by year:\n")
print(year_summary)

# Missing data patterns
cat("\n=== MISSING DATA PATTERNS ===\n")
missing_patterns <- combined_long %>%
  group_by(dataset) %>%
  summarise(
    total_observations = n(),
    total_missing = sum(is.na(value)),
    missing_percentage = round(total_missing / total_observations * 100, 2),
    countries_with_data = n_distinct(`Country Name`[!is.na(value)]),
    series_with_data = n_distinct(`Series Name`[!is.na(value)]),
    years_with_data = n_distinct(year[!is.na(value)]),
    .groups = "drop"
  )

cat("Overall missing data patterns:\n")
print(missing_patterns)

# Value distribution analysis
cat("\n=== VALUE DISTRIBUTION ANALYSIS ===\n")
value_summary <- combined_long %>%
  filter(!is.na(value)) %>%
  group_by(dataset) %>%
  summarise(
    observations = n(),
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE),
    negative_values = sum(value < 0, na.rm = TRUE),
    zero_values = sum(value == 0, na.rm = TRUE),
    positive_values = sum(value > 0, na.rm = TRUE),
    .groups = "drop"
  )

cat("Value distribution by dataset:\n")
print(value_summary)

# Key indicators analysis
cat("\n=== KEY INDICATORS ANALYSIS ===\n")

# Maternal mortality indicators
mmr_indicators <- combined_long %>%
  filter(str_detect(`Series Name`, "(?i)maternal.*mortality")) %>%
  group_by(`Series Name`) %>%
  summarise(
    observations = n(),
    non_missing = sum(!is.na(value)),
    completion_rate = round(non_missing / observations * 100, 2),
    countries = n_distinct(`Country Name`),
    years = n_distinct(year),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    mean_value = round(mean(value, na.rm = TRUE), 2),
    .groups = "drop"
  )

if(nrow(mmr_indicators) > 0) {
  cat("Maternal Mortality Indicators:\n")
  print(mmr_indicators)
}

# GDP indicators
gdp_indicators <- combined_long %>%
  filter(str_detect(`Series Name`, "(?i)gdp")) %>%
  group_by(`Series Name`) %>%
  summarise(
    observations = n(),
    non_missing = sum(!is.na(value)),
    completion_rate = round(non_missing / observations * 100, 2),
    countries = n_distinct(`Country Name`),
    years = n_distinct(year),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    mean_value = round(mean(value, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(completion_rate))

if(nrow(gdp_indicators) > 0) {
  cat("\nGDP Indicators:\n")
  print(gdp_indicators)
}

# Health indicators
health_indicators <- combined_long %>%
  filter(str_detect(`Series Name`, "(?i)life.*expectancy|mortality|health|fertility")) %>%
  group_by(`Series Name`) %>%
  summarise(
    observations = n(),
    non_missing = sum(!is.na(value)),
    completion_rate = round(non_missing / observations * 100, 2),
    countries = n_distinct(`Country Name`),
    years = n_distinct(year),
    .groups = "drop"
  ) %>%
  arrange(desc(completion_rate))

if(nrow(health_indicators) > 0) {
  cat("\nHealth Indicators (top 10 by completion rate):\n")
  print(health_indicators %>% head(10))
}

# Save processed datasets
cat("\n=== SAVING PROCESSED DATASETS ===\n")
write_csv(gender_stats_long, "gender_stats_long.csv")
write_csv(world_dev_long, "world_dev_long.csv")
write_csv(combined_long, "combined_long.csv")
write_csv(series_summary, "series_summary.csv")
write_csv(country_summary, "country_summary.csv")
write_csv(year_summary, "year_summary.csv")

cat("Saved files:\n")
cat("- gender_stats_long.csv\n")
cat("- world_dev_long.csv\n")
cat("- combined_long.csv\n")
cat("- series_summary.csv\n")
cat("- country_summary.csv\n")
cat("- year_summary.csv\n")

cat("\n=== SCRIPT COMPLETE ===\n")
cat("Raw data has been converted to long format and comprehensively described.\n") 

