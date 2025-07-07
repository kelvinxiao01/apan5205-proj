library(tidyverse)

# Load data
wb_data_clean <- read_csv('wb_data_row_filtered.csv', show_col_types = FALSE)
cat("Original data dimensions:", nrow(wb_data_clean), "x", ncol(wb_data_clean), "\n")

year_cols <- grep('\\[YR\\d{4}\\]', names(wb_data_clean), value = TRUE)
cat("Year columns found:", length(year_cols), "\n")

# Check if we have MMR in the original data
mmr_rows <- wb_data_clean %>%
  filter(str_detect(`Series Name`, "(?i)maternal.*mortality"))
cat("MMR rows in original data:", nrow(mmr_rows), "\n")

if(nrow(mmr_rows) > 0) {
  cat("MMR series found:\n")
  for(i in 1:nrow(mmr_rows)) {
    cat(paste0(i, ". ", mmr_rows$`Series Name`[i], "\n"))
  }
}

# Reshape data step by step
cat("\nReshaping data...\n")
mice_long_step1 <- wb_data_clean %>%
  select(`Series Name`, `Series Code`, `Country Name`, `Country Code`, all_of(year_cols))

cat("After selecting columns:", nrow(mice_long_step1), "x", ncol(mice_long_step1), "\n")

mice_long_step2 <- mice_long_step1 %>%
  pivot_longer(cols = all_of(year_cols), 
               names_to = "year_label", 
               values_to = "value")

cat("After pivot_longer:", nrow(mice_long_step2), "x", ncol(mice_long_step2), "\n")

mice_long_step3 <- mice_long_step2 %>%
  mutate(year = as.numeric(str_extract(year_label, "\\d{4}"))) %>%
  select(-year_label)

cat("After extracting year:", nrow(mice_long_step3), "x", ncol(mice_long_step3), "\n")

mice_long <- mice_long_step3 %>%
  pivot_wider(names_from = `Series Name`, 
              values_from = value,
              id_cols = c(`Country Name`, `Country Code`, year))

cat("After pivot_wider:", nrow(mice_long), "x", ncol(mice_long), "\n")

# Check column names
cat("Column names in mice_long:\n")
col_names <- names(mice_long)
for(i in 1:min(10, length(col_names))) {
  cat(paste0(i, ". ", col_names[i], "\n"))
}
if(length(col_names) > 10) {
  cat("... and", length(col_names) - 10, "more columns\n")
}

# Get numeric columns
numeric_cols <- mice_long %>%
  select(-`Country Name`, -`Country Code`, -year) %>%
  select_if(is.numeric) %>%
  names()

cat("Numeric columns found:", length(numeric_cols), "\n")

if(length(numeric_cols) > 0) {
  cat("First 10 numeric columns:\n")
  for(i in 1:min(10, length(numeric_cols))) {
    cat(paste0(i, ". ", numeric_cols[i], "\n"))
  }
  
  # Check specifically for maternal mortality
  maternal_in_numeric <- numeric_cols[str_detect(numeric_cols, "(?i)maternal")]
  cat("\nMaternal mortality variables in numeric_cols:\n")
  if(length(maternal_in_numeric) > 0) {
    for(i in 1:length(maternal_in_numeric)) {
      cat(paste0(i, ". ", maternal_in_numeric[i], "\n"))
    }
  } else {
    cat("No maternal mortality variables found in numeric_cols\n")
  }
} else {
  cat("No numeric columns found after reshape!\n")
}

mice_input <- mice_long %>%
  select(`Country Name`, `Country Code`, year, all_of(numeric_cols))

# Calculate missing percentages
missing_pct <- mice_input %>%
  select(all_of(numeric_cols)) %>%
  summarise_all(~ sum(is.na(.)) / length(.) * 100) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
  filter(missing_pct < 80, missing_pct > 1) %>%
  arrange(missing_pct)

# Show all variables that match the pattern
all_matching <- missing_pct %>%
  filter(str_detect(variable, "mortality|GDP|life expectancy|fertility|population"))

cat("All variables matching the pattern (mortality|GDP|life expectancy|fertility|population):\n")
for(i in 1:nrow(all_matching)) {
  cat(paste0(i, ". ", all_matching$variable[i], " - ", round(all_matching$missing_pct[i], 1), "% missing\n"))
}

# Select priority variables
priority_vars <- missing_pct %>%
  filter(str_detect(variable, "mortality|GDP|life expectancy|fertility|population")) %>%
  head(8) %>%
  pull(variable)

cat("\nTop 8 priority variables selected:\n")
for(i in 1:length(priority_vars)) {
  cat(paste0(i, ". ", priority_vars[i], "\n"))
}

# Check specifically for maternal mortality
maternal_vars <- missing_pct %>%
  filter(str_detect(variable, "maternal"))

cat("\nMaternal mortality variables in missing_pct data:\n")
if(nrow(maternal_vars) > 0) {
  for(i in 1:nrow(maternal_vars)) {
    cat(paste0(i, ". ", maternal_vars$variable[i], " - ", round(maternal_vars$missing_pct[i], 1), "% missing\n"))
  }
} else {
  cat("No maternal mortality variables found in missing_pct data\n")
} 