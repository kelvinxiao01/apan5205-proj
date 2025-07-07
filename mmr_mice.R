# Maternal Mortality Time Series Analysis using MICE Imputed Data
# Author: Data Analysis Team
# Date: 2024
# Purpose: Analyze global trends in maternal mortality ratio over time

# Load required libraries
library(tidyverse)
library(lubridate)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(plotly)

# Load the MICE pooled dataset
cat("=== MATERNAL MORTALITY TIME SERIES ANALYSIS ===\n")
cat("Loading MICE pooled dataset...\n")

# Load MICE pooled data
mice_data <- read.csv("wb_data_mice_pooled.csv")

# Check if MICE data is available and valid
if (nrow(mice_data) > 0 && sum(!is.na(mice_data)) > 0) {
  cat("Using MICE pooled dataset with", nrow(mice_data), "rows and", ncol(mice_data), "columns\n")
  data <- mice_data
} else {
  stop("MICE pooled dataset is not available or contains no valid data. Please run mice_imputation.R first.")
}

# Data preparation
cat("\n=== DATA PREPARATION ===\n")

# Filter for maternal mortality ratio data
mmr_data <- data %>%
  filter(grepl("Maternal mortality ratio", Series.Name))

cat("Preparing maternal mortality data from", length(unique(mmr_data$Country.Name)), "countries\n")

# Get year columns
year_cols <- grep("^X?20[0-9]{2}", names(data), value = TRUE)
if (length(year_cols) == 0) {
  year_cols <- grep("\\[YR20[0-9]{2}\\]", names(data), value = TRUE)
}

# Reshape data for time series analysis
mmr_long <- data.frame()

for (i in 1:nrow(mmr_data)) {
  country <- mmr_data$Country.Name[i]
  
  # Extract time series data
  for (col in year_cols) {
    year <- as.numeric(gsub(".*([0-9]{4}).*", "\\1", col))
    if (year >= 2010 && year <= 2023) {
      mmr_val <- as.numeric(mmr_data[i, col])
      
      if (!is.na(mmr_val) && mmr_val > 0) {
        mmr_long <- rbind(mmr_long, data.frame(
          Country = country,
          Year = year,
          MMR = mmr_val
        ))
      }
    }
  }
}

cat("Time series data prepared:", nrow(mmr_long), "observations\n")

# Save the long format data
write.csv(mmr_long, "mmr_time_series_data.csv", row.names = FALSE)

# Global trends analysis
cat("\n=== GLOBAL TRENDS ANALYSIS ===\n")

# Calculate global trends
global_trends <- mmr_long %>%
  group_by(Year) %>%
  summarise(
    Countries = n(),
    Global_Mean = mean(MMR, na.rm = TRUE),
    Global_Median = median(MMR, na.rm = TRUE),
    Global_Min = min(MMR, na.rm = TRUE),
    Global_Max = max(MMR, na.rm = TRUE),
    .groups = 'drop'
  )

cat("Global trends calculated over", length(unique(global_trends$Year)), "years\n")
print(global_trends)

# Save global trends
write.csv(global_trends, "mmr_global_trends.csv", row.names = FALSE)

# Country-specific analysis
cat("\n=== COUNTRY-SPECIFIC ANALYSIS ===\n")

# Calculate change from 2010 to 2023 for each country
country_changes <- mmr_long %>%
  filter(Year %in% c(2010, 2023)) %>%
  group_by(Country) %>%
  summarise(
    MMR_2010 = MMR[Year == 2010][1],
    MMR_2023 = MMR[Year == 2023][1],
    .groups = 'drop'
  ) %>%
  filter(!is.na(MMR_2010) & !is.na(MMR_2023)) %>%
  mutate(
    Change_Absolute = MMR_2023 - MMR_2010,
    Change_Percent = ((MMR_2023 - MMR_2010) / MMR_2010) * 100,
    Change_Category = case_when(
      Change_Percent <= -20 ~ "Large Improvement (>20% decrease)",
      Change_Percent <= -5 ~ "Moderate Improvement (5-20% decrease)",
      Change_Percent <= 5 ~ "Stable (±5%)",
      Change_Percent <= 20 ~ "Moderate Increase (5-20% increase)",
      TRUE ~ "Large Increase (>20% increase)"
    )
  ) %>%
  arrange(Change_Absolute)

# Countries with largest improvements
largest_improvements <- head(country_changes, 10)
cat("Countries with largest improvements (2010-2023):\n")
print(largest_improvements)

# Countries with largest deteriorations
largest_deteriorations <- tail(country_changes, 10)
cat("\nCountries with largest deteriorations (2010-2023):\n")
print(largest_deteriorations)

# Save country changes
write.csv(country_changes, "mmr_change_analysis.csv", row.names = FALSE)

# Recent MMR analysis (2023 or latest available)
recent_mmr <- mmr_long %>%
  group_by(Country) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  arrange(desc(MMR))

# Countries with highest recent MMR
highest_mmr <- head(recent_mmr, 10)
cat("\nCountries with highest recent MMR:\n")
print(highest_mmr)

# Countries with lowest recent MMR
lowest_mmr <- tail(recent_mmr, 10)
cat("\nCountries with lowest recent MMR:\n")
print(lowest_mmr)

# Change pattern summary
change_summary <- country_changes %>%
  group_by(Change_Category) %>%
  summarise(
    Count = n(),
    Percentage = round(n() / nrow(country_changes) * 100, 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(Count))

cat("\nOverall change patterns (2010-2023):\n")
print(change_summary)

# Focus countries for detailed analysis
focus_countries <- c(
  # Highest improvements
  head(largest_improvements$Country, 3),
  # Highest deteriorations  
  tail(largest_deteriorations$Country, 3),
  # Highest current MMR
  head(highest_mmr$Country, 2),
  # Lowest current MMR
  tail(lowest_mmr$Country, 2)
)

focus_data <- mmr_long %>%
  filter(Country %in% focus_countries)

# Save focus countries data
write.csv(focus_data, "mmr_focus_countries.csv", row.names = FALSE)

cat("\nFocus countries selected for detailed visualization:\n")
print(focus_countries)

# Visualizations
cat("\n=== CREATING VISUALIZATIONS ===\n")

# Set theme for all plots
theme_set(theme_minimal(base_size = 12) +
            theme(
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              panel.grid.major = element_line(color = "grey90", size = 0.5),
              panel.grid.minor = element_line(color = "grey95", size = 0.25),
              text = element_text(color = "black"),
              plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 12, hjust = 0.5),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10),
              legend.title = element_text(size = 11, face = "bold"),
              legend.text = element_text(size = 10)
            ))

# 1. Global trends visualization
p1 <- ggplot(global_trends, aes(x = Year)) +
  geom_line(aes(y = Global_Mean), color = "#2E86AB", size = 1.5) +
  geom_point(aes(y = Global_Mean), color = "#2E86AB", size = 3) +
  geom_line(aes(y = Global_Median), color = "#A23B72", size = 1.2, linetype = "dashed") +
  geom_point(aes(y = Global_Median), color = "#A23B72", size = 2.5) +
  labs(title = "Global Maternal Mortality Trends",
       subtitle = "Mean and median MMR worldwide (2010-2023)",
       x = "Year",
       y = "Maternal Mortality Ratio (per 100,000 live births)") +
  scale_x_continuous(breaks = seq(2010, 2023, 2)) +
  scale_y_continuous(labels = scales::comma) +
  annotate("text", x = 2018, y = global_trends$Global_Mean[global_trends$Year == 2018] + 10,
           label = "Mean", color = "#2E86AB", fontface = "bold") +
  annotate("text", x = 2018, y = global_trends$Global_Median[global_trends$Year == 2018] - 15,
           label = "Median", color = "#A23B72", fontface = "bold")

ggsave("mmr_global_trends.png", p1, width = 10, height = 6, dpi = 300, bg = "white")

# 2. Focus countries visualization
p2 <- ggplot(focus_data, aes(x = Year, y = MMR, color = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  labs(title = "Maternal Mortality Trends: Focus Countries",
       subtitle = "Countries with notable improvements, deteriorations, and extreme values",
       x = "Year",
       y = "Maternal Mortality Ratio (per 100,000 live births)",
       color = "Country") +
  scale_x_continuous(breaks = seq(2010, 2023, 2)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(type = "qual", palette = "Set3") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))

ggsave("mmr_focus_countries.png", p2, width = 12, height = 8, dpi = 300, bg = "white")

# 3. Distribution analysis
p3 <- ggplot(mmr_long, aes(x = factor(Year), y = MMR)) +
  geom_boxplot(fill = "#F18F01", alpha = 0.7, outlier.alpha = 0.5) +
  labs(title = "Distribution of Maternal Mortality Ratios Over Time",
       subtitle = "Boxplots showing median, quartiles, and outliers by year",
       x = "Year",
       y = "Maternal Mortality Ratio (per 100,000 live births)") +
  scale_y_log10(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("mmr_distribution.png", p3, width = 10, height = 6, dpi = 300, bg = "white")

# 4. Change analysis visualization
change_viz_data <- country_changes %>%
  mutate(Country_Label = ifelse(abs(Change_Absolute) > 200, Country, ""))

p4 <- ggplot(change_viz_data, aes(x = MMR_2010, y = Change_Absolute)) +
  geom_point(aes(color = Change_Category), alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_text(aes(label = Country_Label), vjust = -0.5, size = 3, fontface = "bold") +
  labs(title = "Change in Maternal Mortality (2010-2023)",
       subtitle = "Absolute change vs. baseline MMR in 2010",
       x = "Maternal Mortality Ratio in 2010",
       y = "Change in MMR (2023 - 2010)",
       color = "Change Category") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#8B0000")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))

ggsave("mmr_change_analysis.png", p4, width = 12, height = 8, dpi = 300, bg = "white")

# 5. Comprehensive time series dashboard
p5 <- grid.arrange(
  p1 + theme(plot.title = element_text(size = 12), 
             plot.subtitle = element_text(size = 10),
             axis.title = element_text(size = 10)),
  p3 + theme(plot.title = element_text(size = 12), 
             plot.subtitle = element_text(size = 10),
             axis.title = element_text(size = 10)),
  p4 + theme(plot.title = element_text(size = 12), 
             plot.subtitle = element_text(size = 10),
             axis.title = element_text(size = 10),
             legend.position = "none"),
  p2 + theme(plot.title = element_text(size = 12), 
             plot.subtitle = element_text(size = 10),
             axis.title = element_text(size = 10),
             legend.position = "none"),
  ncol = 2, nrow = 2,
  top = "Maternal Mortality Time Series Analysis - Comprehensive View"
)

ggsave("mmr_time_series_comprehensive.png", p5, width = 16, height = 12, dpi = 300, bg = "white")

# Summary
cat("\n=== ANALYSIS SUMMARY ===\n")
cat("Maternal mortality time series analysis completed successfully using MICE imputed data!\n")

# Global summary
start_year <- min(global_trends$Year)
end_year <- max(global_trends$Year)
start_mmr <- global_trends$Global_Mean[global_trends$Year == start_year]
end_mmr <- global_trends$Global_Mean[global_trends$Year == end_year]
global_change <- ((end_mmr - start_mmr) / start_mmr) * 100

cat("Global average MMR decreased from", round(start_mmr, 1), "per 100,000 live births in", start_year,
    "to", round(end_mmr, 1), "in", end_year, "\n")
cat("Overall global change:", round(global_change, 1), "%\n")

# Country patterns
cat("\nCountry change patterns:\n")
for (i in 1:nrow(change_summary)) {
  cat("-", change_summary$Change_Category[i], ":", change_summary$Count[i], 
      "countries (", change_summary$Percentage[i], "%)\n")
}

cat("\nFiles created:\n")
cat("- Visualizations: mmr_global_trends.png, mmr_focus_countries.png, mmr_distribution.png,\n")
cat("  mmr_change_analysis.png, mmr_time_series_comprehensive.png\n")
cat("- Data files: mmr_time_series_data.csv, mmr_global_trends.csv, mmr_change_analysis.csv,\n")
cat("  mmr_focus_countries.csv\n")

cat("\nNote: This analysis uses MICE imputed data, which provides more sophisticated\n")
cat("handling of missing values compared to simple mean imputation.\n") 