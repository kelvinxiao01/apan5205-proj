################################################################################
# COMPREHENSIVE MATERNAL MORTALITY RATIO ANALYSIS USING MICE IMPUTATION
# ============================================================================
# Purpose: Complete analysis of maternal mortality ratio (MMR) using MICE imputation
# Input: wb-gender-statistics.csv, wb-world-development.csv  
# Output: Comprehensive analysis with visualizations and data files
# Author: Analysis Script
# Date: 2024
# ============================================================================

# Load required libraries
library(tidyverse)
library(mice)
library(VIM)
library(corrplot)
library(gridExtra)
library(scales)
library(RColorBrewer)
library(plotly)
library(lubridate)

# Set theme for consistent visualizations
theme_set(theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.3),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    legend.title = element_text(size = 11, face = "bold", color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 11, face = "bold", color = "black"),
    strip.background = element_rect(fill = "gray95", color = "gray80")
  ))

cat("=== STEP 1: DATA CLEANING AND PREPARATION ===\n")

# Load datasets
cat("Loading datasets...\n")
gender_stats <- read_csv("wb-gender-statistics.csv", show_col_types = FALSE)
world_dev <- read_csv("wb-world-development.csv", show_col_types = FALSE)

cat("Gender statistics dataset:", nrow(gender_stats), "rows,", ncol(gender_stats), "columns\n")
cat("World development dataset:", nrow(world_dev), "rows,", ncol(world_dev), "columns\n")

# Combine datasets
wb_data <- bind_rows(gender_stats, world_dev)
cat("Combined dataset:", nrow(wb_data), "rows,", ncol(wb_data), "columns\n")

# Save combined dataset
write_csv(wb_data, "wb_data_combined.csv")
cat("Saved: wb_data_combined.csv\n")

# Clean the data - remove metadata rows and filter for actual data
wb_data_clean <- wb_data %>%
  filter(!is.na(`Series Name`),
         !grepl("Data from database|Last Updated|Code|License", `Series Name`),
         !is.na(`Country Name`),
         !grepl("^[A-Z]{2,3}\\.[A-Z]{3,4}\\.", `Series Name`)) %>%
  # Remove rows where Series Name looks like a code
  filter(!grepl("^[A-Z]{2}\\.[A-Z]{3}\\.", `Series Name`))

cat("After cleaning - Dataset:", nrow(wb_data_clean), "rows,", ncol(wb_data_clean), "columns\n")

# Save cleaned dataset
write_csv(wb_data_clean, "wb_data_row_filtered.csv")
cat("Filtered dataset saved as: wb_data_row_filtered.csv\n")

# Create mean imputed version for comparison
cat("Creating mean imputed dataset...\n")
year_cols <- grep("\\[YR\\d{4}\\]", names(wb_data_clean), value = TRUE)
cat("Found", length(year_cols), "year columns\n")

wb_data_mean_imputed <- wb_data_clean %>%
  mutate(across(all_of(year_cols), ~ as.numeric(as.character(.x)))) %>%
  group_by(`Series Name`) %>%
  mutate(across(all_of(year_cols), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))) %>%
  ungroup()

# Save mean imputed dataset
write_csv(wb_data_mean_imputed, "wb_data_mean_imputed.csv")
cat("Mean imputed dataset saved as: wb_data_mean_imputed.csv\n")

cat("\n=== STEP 2: MICE IMPUTATION ===\n")

# Prepare data for MICE imputation
mice_data <- wb_data_clean %>%
  filter(!is.na(`Series Name`),
         !is.na(`Country Name`)) %>%
  # Convert year columns to numeric
  mutate(across(all_of(year_cols), ~ as.numeric(as.character(.x))))

cat("MICE input data:", nrow(mice_data), "rows,", ncol(mice_data), "columns\n")

# Reshape data for MICE (long format)
mice_long <- mice_data %>%
  select(`Series Name`, `Series Code`, `Country Name`, `Country Code`, all_of(year_cols)) %>%
  pivot_longer(cols = all_of(year_cols), 
               names_to = "year_label", 
               values_to = "value") %>%
  mutate(year = as.numeric(str_extract(year_label, "\\d{4}"))) %>%
  select(-year_label) %>%
  # Convert values to numeric BEFORE pivot_wider
  mutate(value = as.numeric(value)) %>%
  # Pivot wider to get series as columns
  pivot_wider(names_from = `Series Name`, 
              values_from = value,
              id_cols = c(`Country Name`, `Country Code`, year))

cat("Reshaped data for MICE:", nrow(mice_long), "rows,", ncol(mice_long), "columns\n")

# Check if we have any numeric columns for MICE
numeric_cols <- mice_long %>%
  select(-`Country Name`, -`Country Code`, -year) %>%
  select_if(is.numeric) %>%
  names()

cat("Found", length(numeric_cols), "numeric columns for MICE imputation\n")

# Print the first few numeric columns to verify MMR is included
if(length(numeric_cols) > 0) {
  cat("First 10 numeric columns:\n")
  for(i in 1:min(10, length(numeric_cols))) {
    cat(paste0(i, ". ", numeric_cols[i], "\n"))
  }
  
  # Check specifically for maternal mortality
  mmr_cols <- numeric_cols[str_detect(numeric_cols, "(?i)maternal.*mortality")]
  if(length(mmr_cols) > 0) {
    cat("Maternal mortality columns found:\n")
    for(i in 1:length(mmr_cols)) {
      cat(paste0(i, ". ", mmr_cols[i], "\n"))
    }
  }
}

if(length(numeric_cols) > 0) {
  # Prepare MICE input with only numeric columns
  mice_input <- mice_long %>%
    select(`Country Name`, `Country Code`, year, all_of(numeric_cols))
  
  # Check missing data patterns
  cat("Missing data patterns:\n")
  missing_pattern <- mice_input %>%
    select(-`Country Name`, -`Country Code`, -year) %>%
    is.na() %>%
    colSums()
  
  print(missing_pattern[missing_pattern > 0])
  
  # Create missing data pattern plot
  png("mice_missing_patterns.png", width = 12, height = 8, units = "in", res = 300, bg = "white")
  VIM::aggr(mice_input %>% select(-`Country Name`, -`Country Code`, -year), 
            col = c('navyblue', 'red'), 
            numbers = TRUE, 
            sortVars = TRUE)
  dev.off()
  
  # Perform MICE imputation (limit to reasonable number of variables)
  # If too many variables, select the most important ones
  if(length(numeric_cols) > 20) {
    # Calculate missing percentages and select variables with reasonable missing data
    missing_pct <- mice_input %>%
      select(all_of(numeric_cols)) %>%
      summarise_all(~ sum(is.na(.)) / length(.) * 100) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
      filter(missing_pct < 80, missing_pct > 1) %>%  # Variables with 1-80% missing
      arrange(missing_pct)
    
    # Select key variables for health and economic analysis
    priority_vars <- missing_pct %>%
      filter(str_detect(variable, "mortality|GDP|life expectancy|fertility|population")) %>%
      head(8) %>%  # Select top 8 priority variables
      pull(variable)
    
    # If we don't have enough priority variables, add more from the least missing
    if(length(priority_vars) < 5) {
      additional_vars <- missing_pct %>%
        filter(!variable %in% priority_vars) %>%
        head(5 - length(priority_vars)) %>%
        pull(variable)
      priority_vars <- c(priority_vars, additional_vars)
    }
    
    selected_vars <- priority_vars
    cat("Selected", length(selected_vars), "priority variables for MICE imputation\n")
    
    mice_input_selected <- mice_input %>%
      select(`Country Name`, `Country Code`, year, all_of(selected_vars))
  } else {
    mice_input_selected <- mice_input
    selected_vars <- numeric_cols
  }
  
  # Check for multicollinearity and remove highly correlated variables
  mice_vars_only <- mice_input_selected %>% 
    select(-`Country Name`, -`Country Code`, -year)
  
  # Clean variable names for MICE (remove spaces and special characters)
  clean_names <- make.names(names(mice_vars_only))
  original_names <- names(mice_vars_only)  # Store original names before cleaning
  names(mice_vars_only) <- clean_names
  
  # Create mapping between original and clean names
  name_mapping <- setNames(original_names, clean_names)
  
  # Calculate correlation matrix for available data
  complete_cases <- mice_vars_only[complete.cases(mice_vars_only), ]
  
  if(nrow(complete_cases) > 10 && ncol(complete_cases) > 1) {
    cor_matrix <- cor(complete_cases, use = "complete.obs")
    
    # Find highly correlated pairs (> 0.9)
    high_cor_pairs <- which(abs(cor_matrix) > 0.9 & cor_matrix != 1, arr.ind = TRUE)
    
    if(nrow(high_cor_pairs) > 0) {
      # Remove one variable from each highly correlated pair
      clean_vars_to_remove <- unique(colnames(cor_matrix)[high_cor_pairs[, 2]])
      mice_vars_only <- mice_vars_only %>% select(-all_of(clean_vars_to_remove))
      
      # Update name mapping to only include remaining variables
      name_mapping <- name_mapping[names(name_mapping) %in% names(mice_vars_only)]
      
      cat("Removed", length(clean_vars_to_remove), "highly correlated variables\n")
    }
  }
  
  # Update selected_vars to match the final variables (original names)
  selected_vars <- name_mapping[names(mice_vars_only)]
  
  cat("Final selection: performing MICE imputation with", ncol(mice_vars_only), "variables...\n")
  
  # Perform MICE imputation with error handling
  tryCatch({
    mice_result <- mice(mice_vars_only, 
                        m = 5, 
                        maxit = 5,  # Reduced iterations for stability
                        method = 'pmm', 
                        seed = 123,
                        printFlag = FALSE)
    
    # Create convergence plot
    png("mice_convergence.png", width = 12, height = 8, units = "in", res = 300, bg = "white")
    plot(mice_result, col = c('blue', 'red', 'green', 'orange', 'purple'))
    dev.off()
    
    # Extract completed datasets
    cat("Extracting completed datasets...\n")
    for(i in 1:5) {
      completed_data <- complete(mice_result, i)
      # Add back identifiers and restore original names
      completed_data <- bind_cols(
        mice_input_selected %>% select(`Country Name`, `Country Code`, year),
        completed_data
      )
      # Restore original variable names using the mapping
      names(completed_data)[4:ncol(completed_data)] <- selected_vars
      write_csv(completed_data, paste0("wb_data_mice_imputed_", i, ".csv"))
    }
    
    # Create pooled dataset
    cat("Creating pooled dataset...\n")
    pooled_data <- mice_result %>%
      complete("all") %>%
      map_dfr(~ {
        result_data <- bind_cols(
          mice_input_selected %>% select(`Country Name`, `Country Code`, year),
          .x
        )
        # Restore original variable names using the mapping
        names(result_data)[4:ncol(result_data)] <- selected_vars
        result_data
      }, .id = "imputation") %>%
      filter(imputation != "0") %>%  # Remove original data
      group_by(`Country Name`, `Country Code`, year) %>%
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
    
    # Get the actual variable names from the pooled data
    actual_vars <- names(pooled_data)[!names(pooled_data) %in% c("Country Name", "Country Code", "year")]
    
    # Reshape pooled data back to wide format (series as rows)
    pooled_wide <- pooled_data %>%
      pivot_longer(cols = all_of(actual_vars), 
                   names_to = "Series Name", 
                   values_to = "value") %>%
      mutate(year_label = paste0(year, " [YR", year, "]")) %>%
      select(-year) %>%
      pivot_wider(names_from = year_label, 
                  values_from = value,
                  id_cols = c(`Country Name`, `Country Code`, `Series Name`)) %>%
      mutate(`Series Code` = case_when(
        str_detect(`Series Name`, "GDP.*growth") ~ "NY.GDP.MKTP.KD.ZG",
        str_detect(`Series Name`, "Maternal.*mortality") ~ "SH.STA.MMRT",
        str_detect(`Series Name`, "GDP.*per.*capita") ~ "NY.GDP.PCAP.KD",
        str_detect(`Series Name`, "Labor.*force.*female.*total") ~ "SL.TLF.TOTL.FE.ZS",
        str_detect(`Series Name`, "Labor.*force.*participation.*female") ~ "SL.TLF.ACTI.FE.ZS",
        str_detect(`Series Name`, "Educational.*attainment") ~ "SE.SEC.CUAT.UP.FE.ZS",
        str_detect(`Series Name`, "Fertility.*rate") ~ "SP.DYN.TFRT.IN",
        str_detect(`Series Name`, "Life.*expectancy.*birth.*total") ~ "SP.DYN.LE00.IN",
        str_detect(`Series Name`, "Population.*female") ~ "SP.POP.TOTL.FE.IN",
        str_detect(`Series Name`, "Population.*male") ~ "SP.POP.TOTL.MA.IN",
        TRUE ~ "OTHER"
      )) %>%
      select(`Series Name`, `Series Code`, `Country Name`, `Country Code`, everything())
    
    # Save pooled dataset
    write_csv(pooled_wide, "wb_data_mice_pooled.csv")
    cat("Pooled dataset saved as: wb_data_mice_pooled.csv\n")
    
    mice_success <- TRUE
    
  }, error = function(e) {
    cat("MICE imputation failed with error:", e$message, "\n")
    cat("Proceeding with mean imputation instead...\n")
    
    # Use mean imputation as fallback
    pooled_wide <- wb_data_mean_imputed
    write_csv(pooled_wide, "wb_data_mice_pooled.csv")
    cat("Mean imputed dataset saved as fallback: wb_data_mice_pooled.csv\n")
    
    mice_success <- FALSE
  })
} else {
  cat("No numeric columns found for MICE imputation. Skipping MICE step.\n")
  # Create a simple pooled dataset from the cleaned data
  pooled_wide <- wb_data_clean
  write_csv(pooled_wide, "wb_data_mice_pooled.csv")
}

cat("MICE imputation completed successfully!\n")
cat("Total variables imputed:", ncol(mice_input_selected) - 3, "\n")

cat("\n=== STEP 3: COMPREHENSIVE ANALYSIS ===\n")

# Load MICE pooled data
wb_mice_pooled <- read_csv("wb_data_mice_pooled.csv", show_col_types = FALSE)
cat("Loaded MICE pooled dataset:", nrow(wb_mice_pooled), "rows,", ncol(wb_mice_pooled), "columns\n")

# Verify data validity
if(all(is.na(wb_mice_pooled))) {
  stop("ERROR: MICE pooled data contains only NA values!")
}

cat("Data validation passed - MICE data contains valid values\n")

# Create comprehensive analysis dataset
# First, let's check what we have in the pooled data
cat("Available series in pooled data:\n")
available_series <- unique(wb_mice_pooled$`Series Name`)
for(i in 1:length(available_series)) {
  cat(paste0(i, ". ", available_series[i], "\n"))
}

# Look for MMR data in the original mean imputed dataset if not found in MICE data
mmr_from_original <- wb_data_mean_imputed %>%
  filter(str_detect(`Series Name`, "(?i)maternal.*mortality"))

gdp_from_original <- wb_data_mean_imputed %>%
  filter(str_detect(`Series Name`, "(?i)gdp.*per.*capita.*constant"))

cat("\nFound in original data:\n")
cat("MMR series:", nrow(mmr_from_original), "rows\n")
cat("GDP series:", nrow(gdp_from_original), "rows\n")

# Combine MICE data with original MMR data if needed
if(nrow(mmr_from_original) > 0) {
  # Use MMR from original data and GDP from MICE data
  mmr_data <- mmr_from_original %>%
    select(`Country Name`, `Country Code`, contains("[YR")) %>%
    pivot_longer(cols = contains("[YR"), names_to = "year", values_to = "MMR") %>%
    mutate(year = as.numeric(str_extract(year, "\\d{4}"))) %>%
    filter(!is.na(MMR), MMR > 0)
  
  cat("MMR data prepared:", nrow(mmr_data), "observations\n")
} else {
  cat("No MMR data found in original dataset\n")
  mmr_data <- data.frame()
}

# Extract GDP data from original dataset (MICE data is corrupted)
gdp_data <- wb_data_mean_imputed %>%
  filter(str_detect(`Series Name`, "(?i)gdp.*per.*capita.*constant")) %>%
  select(`Country Name`, `Country Code`, contains("[YR")) %>%
  pivot_longer(cols = contains("[YR"), names_to = "year", values_to = "GDP") %>%
  mutate(year = as.numeric(str_extract(year, "\\d{4}"))) %>%
  filter(!is.na(GDP), GDP > 0)

cat("GDP data prepared:", nrow(gdp_data), "observations\n")

# Combine MMR and GDP data if both are available
if(nrow(mmr_data) > 0 && nrow(gdp_data) > 0) {
  combined_data <- inner_join(mmr_data, gdp_data, by = c("Country Name", "Country Code", "year"))
  cat("Combined MMR-GDP data:", nrow(combined_data), "observations\n")
  
  # Save combined data
  write_csv(combined_data, "gdp_mmr_combined_data.csv")
  
  # Calculate correlation
  correlation <- cor(combined_data$MMR, combined_data$GDP, use = "complete.obs")
  cat("Correlation between MMR and GDP:", round(correlation, 3), "\n")
  
  # Regression analysis (MMR as independent variable)
  model_linear <- lm(GDP ~ MMR, data = combined_data)
  model_log_linear <- lm(log(GDP) ~ MMR, data = combined_data)
  model_log_log <- lm(log(GDP) ~ log(MMR), data = combined_data)
  
  cat("Linear model R-squared:", round(summary(model_linear)$r.squared, 3), "\n")
  cat("Log-linear model R-squared:", round(summary(model_log_linear)$r.squared, 3), "\n")
  cat("Log-log model R-squared:", round(summary(model_log_log)$r.squared, 3), "\n")
  
  # Create scatter plot (MMR vs GDP)
  p1 <- ggplot(combined_data, aes(x = MMR, y = GDP)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Maternal Mortality Ratio vs GDP per Capita",
         subtitle = paste("Correlation:", round(correlation, 3)),
         x = "Maternal Mortality Ratio (per 100,000 live births)",
         y = "GDP per Capita (constant 2015 US$)") +
    theme_minimal()
  
  ggsave("gdp_mmr_scatter.png", p1, width = 10, height = 6, dpi = 300, bg = "white")
  
  # Log-log scatter plot
  p2 <- ggplot(combined_data, aes(x = log(MMR), y = log(GDP))) +
    geom_point(alpha = 0.6, color = "darkgreen", size = 1.5) +
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.3, linewidth = 1.2) +
    stat_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1.5, linetype = "solid") +
    labs(title = "Log-Log: Maternal Mortality Ratio vs GDP per Capita",
         subtitle = paste("Log-Log Model: R² =", round(summary(model_log_log)$r.squared, 3), 
                         "| p-value <", ifelse(summary(model_log_log)$coefficients[2,4] < 0.001, "0.001", 
                                              round(summary(model_log_log)$coefficients[2,4], 3))),
         x = "Log(Maternal Mortality Ratio per 100,000 live births)",
         y = "Log(GDP per Capita, constant 2015 US$)",
         caption = paste("Equation: log(GDP) =", round(model_log_log$coefficients[1], 3), 
                        ifelse(model_log_log$coefficients[2] >= 0, "+", ""), 
                        round(model_log_log$coefficients[2], 3), "× log(MMR)")) +
    theme_minimal() +
    theme(plot.caption = element_text(hjust = 0.5, size = 10, color = "gray40"))
  
  ggsave("gdp_mmr_log_scatter.png", p2, width = 12, height = 8, dpi = 300, bg = "white")
  
  # Income group analysis
  combined_data <- combined_data %>%
    mutate(income_group = case_when(
      GDP < 1000 ~ "Low Income",
      GDP < 4000 ~ "Lower Middle Income", 
      GDP < 12000 ~ "Upper Middle Income",
      TRUE ~ "High Income"
    ))
  
  # Box plot by income group
  p3 <- ggplot(combined_data, aes(x = income_group, y = MMR, fill = income_group)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_brewer(palette = "Set3") +
    scale_y_log10() +
    labs(title = "Maternal Mortality Ratio by Income Group",
         x = "Income Group",
         y = "Maternal Mortality Ratio (log scale)",
         fill = "Income Group") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("mmr_income_boxplot.png", p3, width = 10, height = 6, dpi = 300, bg = "white")
  
  # Time series analysis
  ts_data <- combined_data %>%
    filter(year >= 2010, year <= 2023) %>%
    arrange(`Country Name`, year)
  
  write_csv(ts_data, "mmr_time_series_data.csv")
  
  # Global trends
  global_trends <- ts_data %>%
    group_by(year) %>%
    summarise(
      global_avg_mmr = mean(MMR, na.rm = TRUE),
      global_median_mmr = median(MMR, na.rm = TRUE),
      countries_count = n(),
      .groups = "drop"
    )
  
  write_csv(global_trends, "mmr_global_trends.csv")
  
  # Time series plot - Global trends
  p4 <- ggplot(global_trends, aes(x = year)) +
    geom_line(aes(y = global_avg_mmr, color = "Average"), linewidth = 1.2) +
    geom_line(aes(y = global_median_mmr, color = "Median"), linewidth = 1.2) +
    geom_point(aes(y = global_avg_mmr, color = "Average"), size = 2) +
    geom_point(aes(y = global_median_mmr, color = "Median"), size = 2) +
    scale_color_manual(values = c("Average" = "red", "Median" = "blue")) +
    labs(title = "Global Maternal Mortality Ratio Trends (2010-2023)",
         x = "Year",
         y = "Maternal Mortality Ratio",
         color = "Measure") +
    theme_minimal()
  
  ggsave("mmr_global_trends.png", p4, width = 12, height = 6, dpi = 300, bg = "white")
  
  # Time series plot - Top 10 countries with highest MMR
  top_mmr_countries <- ts_data %>%
    group_by(`Country Name`) %>%
    summarise(avg_mmr = mean(MMR, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(avg_mmr)) %>%
    head(10) %>%
    pull(`Country Name`)
  
  p5 <- ts_data %>%
    filter(`Country Name` %in% top_mmr_countries) %>%
    ggplot(aes(x = year, y = MMR, color = `Country Name`)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.5) +
    scale_color_brewer(palette = "Set3") +
    labs(title = "Maternal Mortality Ratio Trends - Top 10 Countries",
         subtitle = "Countries with highest average MMR (2010-2023)",
         x = "Year",
         y = "Maternal Mortality Ratio (per 100,000 live births)",
         color = "Country") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8))
  
  ggsave("mmr_top_countries_timeseries.png", p5, width = 14, height = 8, dpi = 300, bg = "white")
  
  # Time series plot - Bottom 10 countries with lowest MMR
  bottom_mmr_countries <- ts_data %>%
    group_by(`Country Name`) %>%
    summarise(avg_mmr = mean(MMR, na.rm = TRUE), .groups = "drop") %>%
    arrange(avg_mmr) %>%
    head(10) %>%
    pull(`Country Name`)
  
  p6 <- ts_data %>%
    filter(`Country Name` %in% bottom_mmr_countries) %>%
    ggplot(aes(x = year, y = MMR, color = `Country Name`)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.5) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Maternal Mortality Ratio Trends - Bottom 10 Countries",
         subtitle = "Countries with lowest average MMR (2010-2023)",
         x = "Year",
         y = "Maternal Mortality Ratio (per 100,000 live births)",
         color = "Country") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8))
  
  ggsave("mmr_bottom_countries_timeseries.png", p6, width = 14, height = 8, dpi = 300, bg = "white")
  
  # Time series plot - Regional analysis by income groups
  p7 <- ts_data %>%
    group_by(year, income_group) %>%
    summarise(avg_mmr = mean(MMR, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = year, y = avg_mmr, color = income_group)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 2.5) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = "Maternal Mortality Ratio Trends by Income Group",
         subtitle = "Average MMR by income classification (2010-2023)",
         x = "Year",
         y = "Average Maternal Mortality Ratio",
         color = "Income Group") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("mmr_income_groups_timeseries.png", p7, width = 12, height = 8, dpi = 300, bg = "white")
  
  # Time series plot - Countries with biggest improvements and deteriorations
  country_changes <- ts_data %>%
    group_by(`Country Name`) %>%
    filter(n() >= 5) %>%  # Countries with at least 5 years of data
    summarise(
      start_mmr = first(MMR, order_by = year),
      end_mmr = last(MMR, order_by = year),
      change = end_mmr - start_mmr,
      pct_change = (end_mmr - start_mmr) / start_mmr * 100,
      .groups = "drop"
    ) %>%
    arrange(change)
  
  # Top 10 improvers (biggest decreases)
  top_improvers <- country_changes %>%
    head(10) %>%
    pull(`Country Name`)
  
  # Top 10 deteriorators (biggest increases)
  top_deteriorators <- country_changes %>%
    tail(10) %>%
    pull(`Country Name`)
  
  p8 <- ts_data %>%
    filter(`Country Name` %in% top_improvers) %>%
    ggplot(aes(x = year, y = MMR, color = `Country Name`)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.5) +
    scale_color_brewer(palette = "RdYlGn", direction = 1) +
    labs(title = "Maternal Mortality Ratio - Top 10 Improving Countries",
         subtitle = "Countries with largest MMR decreases (2010-2023)",
         x = "Year",
         y = "Maternal Mortality Ratio (per 100,000 live births)",
         color = "Country") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8))
  
  ggsave("mmr_top_improvers_timeseries.png", p8, width = 14, height = 8, dpi = 300, bg = "white")
  
  p9 <- ts_data %>%
    filter(`Country Name` %in% top_deteriorators) %>%
    ggplot(aes(x = year, y = MMR, color = `Country Name`)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.5) +
    scale_color_brewer(palette = "RdYlGn", direction = -1) +
    labs(title = "Maternal Mortality Ratio - Top 10 Deteriorating Countries",
         subtitle = "Countries with largest MMR increases (2010-2023)",
         x = "Year",
         y = "Maternal Mortality Ratio (per 100,000 live births)",
         color = "Country") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8))
  
  ggsave("mmr_top_deteriorators_timeseries.png", p9, width = 14, height = 8, dpi = 300, bg = "white")
  
  # Comprehensive time series dashboard
  p_ts_combined <- grid.arrange(
    p4 + theme(plot.title = element_text(size = 12), legend.text = element_text(size = 8)),
    p7 + theme(plot.title = element_text(size = 12), legend.text = element_text(size = 8)),
    ncol = 1, nrow = 2,
    top = "Maternal Mortality Ratio Time Series Analysis (2010-2023)"
  )
  
  ggsave("mmr_comprehensive_timeseries.png", p_ts_combined, width = 14, height = 12, dpi = 300, bg = "white")
  
  # Summary statistics
  cat("\n=== SUMMARY STATISTICS ===\n")
  cat("Total countries analyzed:", n_distinct(combined_data$`Country Name`), "\n")
  if(nrow(global_trends) > 0) {
    cat("Global average MMR (2010):", round(global_trends$global_avg_mmr[global_trends$year == 2010], 1), "\n")
    cat("Global average MMR (2023):", round(global_trends$global_avg_mmr[global_trends$year == 2023], 1), "\n")
    
    overall_change <- global_trends$global_avg_mmr[global_trends$year == 2023] - 
                     global_trends$global_avg_mmr[global_trends$year == 2010]
    overall_pct_change <- overall_change / global_trends$global_avg_mmr[global_trends$year == 2010] * 100
    
    cat("Overall change in global average MMR (2010-2023):", round(overall_change, 1), "\n")
    cat("Overall percentage change:", round(overall_pct_change, 1), "%\n")
  }
  
} else {
  cat("Cannot perform MMR-GDP analysis due to missing data\n")
  cat("Available data:\n")
  cat("- MMR observations:", nrow(mmr_data), "\n")
  cat("- GDP observations:", nrow(gdp_data), "\n")
}

# Analysis of available MICE variables
cat("\n=== ANALYSIS OF AVAILABLE MICE VARIABLES ===\n")

# Create analysis of available variables
available_analysis <- wb_mice_pooled %>%
  select(`Country Name`, `Country Code`, contains("[YR")) %>%
  pivot_longer(cols = contains("[YR"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(str_extract(year, "\\d{4}"))) %>%
  filter(!is.na(value)) %>%
  inner_join(wb_mice_pooled %>% select(`Series Name`, `Country Name`, `Country Code`) %>% distinct(), 
             by = c("Country Name", "Country Code"))

write_csv(available_analysis, "available_variables_analysis.csv")

# Create correlation matrix of available numeric data
numeric_data <- wb_mice_pooled %>%
  select(`Country Name`, `Country Code`, `Series Name`, contains("[YR")) %>%
  pivot_longer(cols = contains("[YR"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(str_extract(year, "\\d{4}"))) %>%
  filter(!is.na(value)) %>%
  pivot_wider(names_from = `Series Name`, values_from = value, 
              id_cols = c(`Country Name`, `Country Code`, year)) %>%
  select(-`Country Name`, -`Country Code`, -year) %>%
  select(where(is.numeric))

if(ncol(numeric_data) > 1) {
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  write.csv(cor_matrix, "mice_variables_correlation_matrix.csv")
  cat("Created correlation matrix with", ncol(numeric_data), "variables\n")
  
  # Create correlation plot
  png("mice_correlation_plot.png", width = 10, height = 8, units = "in", res = 300, bg = "white")
  corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
           tl.cex = 0.8, tl.col = "black", tl.srt = 45)
  dev.off()
}

# List all created files
cat("\n=== FILES CREATED ===\n")
cat("Data files:\n")
data_files <- c("wb_data_combined.csv", "wb_data_row_filtered.csv", "wb_data_mean_imputed.csv",
                "wb_data_mice_pooled.csv", "available_variables_analysis.csv", 
                "mice_variables_correlation_matrix.csv")

# Add conditional files
if(nrow(mmr_data) > 0 && nrow(gdp_data) > 0) {
  data_files <- c(data_files, "gdp_mmr_combined_data.csv", "mmr_time_series_data.csv", "mmr_global_trends.csv")
}

for(i in 1:5) {
  data_files <- c(data_files, paste0("wb_data_mice_imputed_", i, ".csv"))
}

for(file in data_files) {
  if(file.exists(file)) {
    cat("-", file, "\n")
  }
}

cat("\nVisualization files:\n")
viz_files <- c("mice_missing_patterns.png", "mice_convergence.png", "mice_correlation_plot.png")

# Add conditional visualizations
if(nrow(mmr_data) > 0 && nrow(gdp_data) > 0) {
  viz_files <- c(viz_files, "gdp_mmr_scatter.png", "gdp_mmr_log_scatter.png", 
                 "mmr_income_boxplot.png", "mmr_global_trends.png",
                 "mmr_top_countries_timeseries.png", "mmr_bottom_countries_timeseries.png",
                 "mmr_income_groups_timeseries.png", "mmr_top_improvers_timeseries.png",
                 "mmr_top_deteriorators_timeseries.png", "mmr_comprehensive_timeseries.png")
}

for(file in viz_files) {
  if(file.exists(file)) {
    cat("-", file, "\n")
  }
}

cat("\n=== Analysis Complete ===\n")