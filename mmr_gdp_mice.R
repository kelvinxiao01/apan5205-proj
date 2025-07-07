# GDP vs Maternal Mortality Analysis using MICE Imputed Data
# Author: Data Analysis Team
# Date: 2024
# Purpose: Analyze the relationship between GDP per capita and maternal mortality ratio
# Note: MMR is the independent variable (x-axis), GDP is the dependent variable (y-axis)

# Load required libraries
library(tidyverse)
library(corrplot)
library(plotly)
library(gridExtra)

# Load the MICE pooled dataset
cat("=== GDP vs MATERNAL MORTALITY ANALYSIS ===\n")
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

# Identify MMR and GDP columns
mmr_cols <- grep("Maternal mortality ratio", data$Series.Name, value = FALSE)
gdp_cols <- grep("GDP per capita", data$Series.Name, value = FALSE)

# Extract MMR data
mmr_data <- data[mmr_cols, ]
# Extract GDP data  
gdp_data <- data[gdp_cols, ]

# Reshape data for analysis
year_cols <- grep("^X?20[0-9]{2}", names(data), value = TRUE)
if (length(year_cols) == 0) {
  year_cols <- grep("\\[YR20[0-9]{2}\\]", names(data), value = TRUE)
}

# Prepare combined dataset
combined_data <- data.frame()

for (i in 1:nrow(mmr_data)) {
  country <- mmr_data$Country.Name[i]
  
  # Find corresponding GDP data for this country
  gdp_row <- which(gdp_data$Country.Name == country)
  
  if (length(gdp_row) > 0) {
    gdp_row <- gdp_row[1]  # Take first match
    
    # Extract time series data
    for (col in year_cols) {
      year <- as.numeric(gsub(".*([0-9]{4}).*", "\\1", col))
      if (year >= 2010 && year <= 2023) {
        mmr_val <- as.numeric(mmr_data[i, col])
        gdp_val <- as.numeric(gdp_data[gdp_row, col])
        
        if (!is.na(mmr_val) && !is.na(gdp_val) && mmr_val > 0 && gdp_val > 0) {
          combined_data <- rbind(combined_data, data.frame(
            Country = country,
            Year = year,
            MMR = mmr_val,
            GDP_per_capita = gdp_val
          ))
        }
      }
    }
  }
}

cat("Combined dataset contains", nrow(combined_data), "observations from", 
    length(unique(combined_data$Country)), "countries\n")

# Save combined data
write.csv(combined_data, "gdp_mmr_combined_data.csv", row.names = FALSE)

# Exploratory Data Analysis
cat("\n=== EXPLORATORY DATA ANALYSIS ===\n")

# Summary statistics
summary_stats <- combined_data %>%
  summarise(
    Countries = n_distinct(Country),
    Observations = n(),
    MMR_Mean = mean(MMR, na.rm = TRUE),
    MMR_Median = median(MMR, na.rm = TRUE),
    MMR_Min = min(MMR, na.rm = TRUE),
    MMR_Max = max(MMR, na.rm = TRUE),
    GDP_Mean = mean(GDP_per_capita, na.rm = TRUE),
    GDP_Median = median(GDP_per_capita, na.rm = TRUE),
    GDP_Min = min(GDP_per_capita, na.rm = TRUE),
    GDP_Max = max(GDP_per_capita, na.rm = TRUE),
    Correlation = cor(MMR, GDP_per_capita, use = "complete.obs")
  )

print(summary_stats)
write.csv(summary_stats, "gdp_mmr_summary_stats.csv", row.names = FALSE)

# Yearly correlations
yearly_corr <- combined_data %>%
  group_by(Year) %>%
  summarise(
    Correlation = cor(MMR, GDP_per_capita, use = "complete.obs"),
    N_Countries = n(),
    .groups = 'drop'
  )

print(yearly_corr)
write.csv(yearly_corr, "gdp_mmr_yearly_correlations.csv", row.names = FALSE)

# Regression Analysis
cat("\n=== REGRESSION ANALYSIS ===\n")

# Model 1: Linear relationship (GDP ~ MMR)
model1 <- lm(GDP_per_capita ~ MMR, data = combined_data)
summary1 <- summary(model1)

# Model 2: Log-linear relationship (GDP ~ log(MMR))
model2 <- lm(GDP_per_capita ~ log(MMR), data = combined_data)
summary2 <- summary(model2)

# Model 3: Log-log relationship (log(GDP) ~ log(MMR))
model3 <- lm(log(GDP_per_capita) ~ log(MMR), data = combined_data)
summary3 <- summary(model3)

# Print model summaries
cat("\n--- Model 1: Linear (GDP ~ MMR) ---\n")
print(summary1)

cat("\n--- Model 2: Log-linear (GDP ~ log(MMR)) ---\n")
print(summary2)

cat("\n--- Model 3: Log-log (log(GDP) ~ log(MMR)) ---\n")
print(summary3)

# Model comparison
model_comparison <- data.frame(
  Model = c("Linear", "Log-linear", "Log-log"),
  R_squared = c(summary1$r.squared, summary2$r.squared, summary3$r.squared),
  Adj_R_squared = c(summary1$adj.r.squared, summary2$adj.r.squared, summary3$adj.r.squared),
  AIC = c(AIC(model1), AIC(model2), AIC(model3)),
  BIC = c(BIC(model1), BIC(model2), BIC(model3))
)

print(model_comparison)
write.csv(model_comparison, "gdp_mmr_model_comparison.csv", row.names = FALSE)

# Best model identification
best_model_idx <- which.max(model_comparison$R_squared)
best_model_name <- model_comparison$Model[best_model_idx]
cat("\nBest fitting model:", best_model_name, "with R² =", 
    round(model_comparison$R_squared[best_model_idx], 3), "\n")

# Extract regression results
regression_results <- data.frame(
  Model = rep(c("Linear", "Log-linear", "Log-log"), each = 2),
  Term = rep(c("Intercept", "MMR_coefficient"), 3),
  Estimate = c(coef(model1), coef(model2), coef(model3)),
  Std_Error = c(summary1$coefficients[,"Std. Error"], 
                summary2$coefficients[,"Std. Error"],
                summary3$coefficients[,"Std. Error"]),
  t_value = c(summary1$coefficients[,"t value"],
              summary2$coefficients[,"t value"],
              summary3$coefficients[,"t value"]),
  p_value = c(summary1$coefficients[,"Pr(>|t|)"],
              summary2$coefficients[,"Pr(>|t|)"],
              summary3$coefficients[,"Pr(>|t|)"])
)

write.csv(regression_results, "gdp_mmr_regression_results.csv", row.names = FALSE)

# Income group analysis
cat("\n=== INCOME GROUP ANALYSIS ===\n")

# Create income groups based on GDP per capita
combined_data$Income_Group <- cut(combined_data$GDP_per_capita,
                                  breaks = c(0, 1000, 4000, 12000, Inf),
                                  labels = c("Low", "Lower-middle", "Upper-middle", "High"),
                                  include.lowest = TRUE)

# Income group summary
income_summary <- combined_data %>%
  group_by(Income_Group) %>%
  summarise(
    Countries = n_distinct(Country),
    Observations = n(),
    MMR_Mean = mean(MMR, na.rm = TRUE),
    MMR_Median = median(MMR, na.rm = TRUE),
    GDP_Mean = mean(GDP_per_capita, na.rm = TRUE),
    GDP_Median = median(GDP_per_capita, na.rm = TRUE),
    .groups = 'drop'
  )

print(income_summary)
write.csv(income_summary, "gdp_mmr_income_summary.csv", row.names = FALSE)

# Country-level analysis
cat("\n=== COUNTRY-LEVEL ANALYSIS ===\n")

country_analysis <- combined_data %>%
  group_by(Country) %>%
  summarise(
    Years = n(),
    MMR_Mean = mean(MMR, na.rm = TRUE),
    GDP_Mean = mean(GDP_per_capita, na.rm = TRUE),
    MMR_Latest = MMR[which.max(Year)],
    GDP_Latest = GDP_per_capita[which.max(Year)],
    .groups = 'drop'
  ) %>%
  arrange(desc(MMR_Mean))

# Top 10 countries by MMR
top_mmr_countries <- head(country_analysis, 10)
cat("\nTop 10 countries by average MMR:\n")
print(top_mmr_countries)

# Bottom 10 countries by MMR
bottom_mmr_countries <- tail(country_analysis, 10)
cat("\nBottom 10 countries by average MMR:\n")
print(bottom_mmr_countries)

write.csv(country_analysis, "gdp_mmr_country_analysis.csv", row.names = FALSE)

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

# 1. Basic scatter plot (GDP vs MMR)
p1 <- ggplot(combined_data, aes(x = MMR, y = GDP_per_capita)) +
  geom_point(alpha = 0.6, color = "#2E86AB", size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#A23B72", fill = "#A23B72", alpha = 0.2) +
  labs(title = "GDP per Capita vs Maternal Mortality Ratio",
       subtitle = "Linear relationship with confidence interval",
       x = "Maternal Mortality Ratio (per 100,000 live births)",
       y = "GDP per Capita (constant 2015 US$)") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

ggsave("gdp_mmr_scatter.png", p1, width = 10, height = 6, dpi = 300, bg = "white")

# 2. Log-scale scatter plot
p2 <- ggplot(combined_data, aes(x = log(MMR), y = log(GDP_per_capita))) +
  geom_point(alpha = 0.6, color = "#2E86AB", size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#A23B72", fill = "#A23B72", alpha = 0.2) +
  labs(title = "Log GDP per Capita vs Log Maternal Mortality Ratio",
       subtitle = "Log-log relationship with confidence interval",
       x = "Log(Maternal Mortality Ratio)",
       y = "Log(GDP per Capita)") +
  scale_x_continuous(labels = function(x) round(exp(x), 0)) +
  scale_y_continuous(labels = function(x) scales::comma(round(exp(x), 0)))

ggsave("gdp_mmr_log_scatter.png", p2, width = 10, height = 6, dpi = 300, bg = "white")

# 3. Boxplot by income group
p3 <- ggplot(combined_data, aes(x = Income_Group, y = MMR, fill = Income_Group)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  scale_fill_manual(values = c("#F18F01", "#C73E1D", "#A23B72", "#2E86AB")) +
  labs(title = "Maternal Mortality Ratio by Income Group",
       subtitle = "Distribution across World Bank income classifications",
       x = "Income Group",
       y = "Maternal Mortality Ratio (per 100,000 live births)") +
  theme(legend.position = "none") +
  scale_y_log10(labels = scales::comma)

ggsave("mmr_income_boxplot.png", p3, width = 10, height = 6, dpi = 300, bg = "white")

# 4. Time series by income group
income_time_series <- combined_data %>%
  group_by(Year, Income_Group) %>%
  summarise(MMR_Mean = mean(MMR, na.rm = TRUE), .groups = 'drop')

p4 <- ggplot(income_time_series, aes(x = Year, y = MMR_Mean, color = Income_Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#F18F01", "#C73E1D", "#A23B72", "#2E86AB")) +
  labs(title = "Maternal Mortality Trends by Income Group",
       subtitle = "Average MMR over time (2010-2023)",
       x = "Year",
       y = "Average Maternal Mortality Ratio",
       color = "Income Group") +
  scale_x_continuous(breaks = seq(2010, 2023, 2)) +
  scale_y_continuous(labels = scales::comma)

ggsave("mmr_income_timeseries.png", p4, width = 10, height = 6, dpi = 300, bg = "white")

# 5. Correlation over time
p5 <- ggplot(yearly_corr, aes(x = Year, y = Correlation)) +
  geom_line(color = "#2E86AB", size = 1.2) +
  geom_point(color = "#A23B72", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "GDP-MMR Correlation Over Time",
       subtitle = "Correlation coefficient between GDP per capita and MMR",
       x = "Year",
       y = "Correlation Coefficient") +
  scale_x_continuous(breaks = seq(2010, 2023, 2)) +
  ylim(-1, 1)

ggsave("mmr_gdp_correlation_time.png", p5, width = 10, height = 6, dpi = 300, bg = "white")

# 6. Top countries visualization
top_countries_data <- combined_data %>%
  filter(Country %in% top_mmr_countries$Country[1:5]) %>%
  group_by(Country) %>%
  arrange(Year) %>%
  slice_tail(n = 1)

p6 <- ggplot(top_countries_data, aes(x = reorder(Country, -MMR), y = MMR)) +
  geom_col(fill = "#C73E1D", alpha = 0.8) +
  geom_text(aes(label = round(MMR, 0)), vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(title = "Top 5 Countries by Maternal Mortality Ratio",
       subtitle = "Latest available data (2023)",
       x = "Country",
       y = "Maternal Mortality Ratio (per 100,000 live births)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

ggsave("gdp_mmr_countries.png", p6, width = 10, height = 6, dpi = 300, bg = "white")

# 7. Comprehensive dashboard
p7 <- grid.arrange(
  p1 + theme(plot.title = element_text(size = 10), 
             plot.subtitle = element_text(size = 8),
             axis.title = element_text(size = 9)),
  p2 + theme(plot.title = element_text(size = 10), 
             plot.subtitle = element_text(size = 8),
             axis.title = element_text(size = 9)),
  p3 + theme(plot.title = element_text(size = 10), 
             plot.subtitle = element_text(size = 8),
             axis.title = element_text(size = 9)),
  p4 + theme(plot.title = element_text(size = 10), 
             plot.subtitle = element_text(size = 8),
             axis.title = element_text(size = 9)),
  ncol = 2, nrow = 2,
  top = "GDP vs Maternal Mortality Analysis - Comprehensive View"
)

ggsave("gdp_mmr_comprehensive.png", p7, width = 16, height = 12, dpi = 300, bg = "white")

# Summary
cat("\n=== ANALYSIS SUMMARY ===\n")
cat("Analysis completed successfully using MICE imputed data!\n")
cat("Key findings:\n")
cat("- Overall correlation between MMR and GDP per capita:", 
    round(summary_stats$Correlation, 3), "\n")
cat("- Best fitting model:", best_model_name, "with R² =", 
    round(model_comparison$R_squared[best_model_idx], 3), "\n")

# High vs Low income comparison
high_income_mmr <- income_summary$MMR_Mean[income_summary$Income_Group == "High"]
low_income_mmr <- income_summary$MMR_Mean[income_summary$Income_Group == "Low"]

if (length(high_income_mmr) > 0 && length(low_income_mmr) > 0) {
  cat("- High-income countries average MMR:", round(high_income_mmr, 1), 
      "per 100,000 live births\n")
  cat("- Low-income countries average MMR:", round(low_income_mmr, 1), 
      "per 100,000 live births\n")
  cat("- MMR ratio (Low/High income):", round(low_income_mmr/high_income_mmr, 1), "x higher\n")
}

# Regression interpretation
if (best_model_name == "Linear") {
  mmr_coef <- coef(model1)[2]
  cat("- For every 1-unit increase in MMR, GDP decreases by $", 
      round(abs(mmr_coef), 1), "\n")
} else if (best_model_name == "Log-log") {
  mmr_coef <- coef(model3)[2]
  cat("- Log-log model provides best fit for understanding the relationship\n")
  cat("- Elasticity: 1% increase in MMR associated with ", 
      round(mmr_coef, 2), "% change in GDP\n")
}

cat("\nFiles created:\n")
cat("- Visualizations: gdp_mmr_scatter.png, gdp_mmr_log_scatter.png, mmr_income_boxplot.png,\n")
cat("  mmr_income_timeseries.png, mmr_gdp_correlation_time.png, gdp_mmr_countries.png,\n")
cat("  gdp_mmr_comprehensive.png\n")
cat("- Data files: gdp_mmr_combined_data.csv, gdp_mmr_summary_stats.csv,\n")
cat("  gdp_mmr_regression_results.csv, gdp_mmr_model_comparison.csv,\n")
cat("  gdp_mmr_yearly_correlations.csv, gdp_mmr_income_summary.csv,\n")
cat("  gdp_mmr_country_analysis.csv\n")

cat("\nNote: This analysis uses MICE imputed data, which provides more sophisticated\n")
cat("handling of missing values compared to simple mean imputation.\n")
cat("The analysis treats MMR as the independent variable predicting GDP per capita.\n") 