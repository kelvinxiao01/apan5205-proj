library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Load the mean-imputed dataset
data <- read.csv("wb_data_mean_imputed.csv")

# Check the structure of the data
cat("Dataset dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
cat("Available series (first 10):\n")
print(head(unique(data$Series.Name), 10))

# Extract Maternal Mortality Ratio data
mmr_data <- data %>%
  filter(Series.Name == "Maternal mortality ratio (modeled estimate, per 100,000 live births)") %>%
  select(Country.Name, starts_with("X")) %>%
  pivot_longer(-Country.Name, names_to = "Year", values_to = "MMR") %>%
  mutate(Year = as.numeric(str_extract(Year, "(?<=X)\\d{4}"))) %>%
  filter(!is.na(Year)) %>%
  filter(Year >= 2000 & Year <= 2023)

# Extract GDP per capita data
gdp_data <- data %>%
  filter(Series.Name == "GDP per capita (constant 2015 US$)") %>%
  select(Country.Name, starts_with("X")) %>%
  pivot_longer(-Country.Name, names_to = "Year", values_to = "GDP_per_capita") %>%
  mutate(Year = as.numeric(str_extract(Year, "(?<=X)\\d{4}"))) %>%
  filter(!is.na(Year)) %>%
  filter(Year >= 2000 & Year <= 2023)

# Combine the datasets
combined_data <- mmr_data %>%
  inner_join(gdp_data, by = c("Country.Name", "Year")) %>%
  filter(!is.na(MMR) & !is.na(GDP_per_capita)) %>%
  # Remove extreme outliers for better visualization
  filter(MMR > 0 & GDP_per_capita > 0)

# Check if we have data
cat("\nCombined MMR-GDP data found for", length(unique(combined_data$Country.Name)), "countries\n")
cat("Year range:", min(combined_data$Year), "to", max(combined_data$Year), "\n")
cat("MMR range:", round(min(combined_data$MMR), 2), "to", round(max(combined_data$MMR), 2), "\n")
cat("GDP per capita range: $", round(min(combined_data$GDP_per_capita), 2), "to $", round(max(combined_data$GDP_per_capita), 2), "\n")

# Sample of data
cat("\nSample of combined data:\n")
print(head(combined_data))

# Create the scatter plot with regression line
p <- ggplot(combined_data, aes(x = MMR, y = GDP_per_capita)) +
  geom_point(alpha = 0.6, size = 1.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1.2, alpha = 0.3) +
  labs(
    title = "Relationship Between Maternal Mortality Rate and GDP per Capita",
    subtitle = "Data from 2000-2023 across countries (with trend line)",
    x = "Maternal Mortality Ratio (deaths per 100,000 live births)",
    y = "GDP per Capita (constant 2015 US$)"
  ) +
  theme_classic() +  # White background with black axes
  theme(
    # Plot styling
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Text styling for better readability
    plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 13, color = "gray30", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    
    # Grid lines for better readability
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.3),
    
    # Plot margins
    plot.margin = margin(20, 20, 20, 20)
  ) +
  # Limit axes to focus on main data clusters
  scale_x_continuous(
    limits = c(0, 800),  # Focus on MMR 0-800 instead of full range
    labels = scales::comma_format(),
    breaks = seq(0, 800, 100)
  ) +
  scale_y_continuous(
    limits = c(0, 50000),  # Focus on GDP 0-50k instead of full range
    labels = scales::dollar_format(),
    breaks = seq(0, 50000, 10000)
  ) +
  # Add a subtle border around the plot
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

# Display the plot
print(p)

# Save the plot
ggsave("mmr_gdp_relationship.png", plot = p, width = 12, height = 8, dpi = 300)
cat("\nPlot saved as 'mmr_gdp_relationship.png'\n")

# Calculate correlation and regression statistics
correlation <- cor(combined_data$MMR, combined_data$GDP_per_capita, use = "complete.obs")
lm_model <- lm(GDP_per_capita ~ MMR, data = combined_data)

cat("\n=== STATISTICAL ANALYSIS ===\n")
cat("Correlation coefficient:", round(correlation, 4), "\n")
cat("R-squared:", round(summary(lm_model)$r.squared, 4), "\n")
cat("Regression equation: GDP per capita = ", round(coef(lm_model)[1], 2), " + ", round(coef(lm_model)[2], 2), " * MMR\n")

# Summary statistics by MMR ranges
cat("\n=== SUMMARY BY MMR RANGES ===\n")
mmr_ranges <- combined_data %>%
  mutate(
    MMR_Range = case_when(
      MMR <= 50 ~ "Low (≤50)",
      MMR <= 200 ~ "Medium (51-200)",
      MMR <= 500 ~ "High (201-500)",
      TRUE ~ "Very High (>500)"
    )
  ) %>%
  group_by(MMR_Range) %>%
  summarise(
    Countries = n_distinct(Country.Name),
    Observations = n(),
    Mean_MMR = round(mean(MMR), 2),
    Mean_GDP = round(mean(GDP_per_capita), 2),
    Median_GDP = round(median(GDP_per_capita), 2),
    .groups = 'drop'
  )

print(mmr_ranges)

# Recent data analysis (2020-2023)
cat("\n=== RECENT DATA ANALYSIS (2020-2023) ===\n")
recent_data <- combined_data %>%
  filter(Year >= 2020) %>%
  group_by(Country.Name) %>%
  summarise(
    Avg_MMR = mean(MMR),
    Avg_GDP = mean(GDP_per_capita),
    .groups = 'drop'
  ) %>%
  arrange(desc(Avg_GDP))

cat("Countries with highest GDP per capita (2020-2023 average):\n")
print(head(recent_data, 10))

cat("\nCountries with lowest GDP per capita (2020-2023 average):\n")
print(tail(recent_data, 10)) 