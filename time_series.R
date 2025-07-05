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

# Filter for Maternal Mortality Ratio data
# Note: R's read.csv() converts column names, so "Series Name" becomes "Series.Name"
mmr_df <- data %>%
  filter(Series.Name == "Maternal mortality ratio (modeled estimate, per 100,000 live births)") %>%
  # Select country name and all year columns (which start with "X" followed by year)
  select(Country.Name, starts_with("X")) %>%
  # Reshape from wide to long format
  pivot_longer(-Country.Name, names_to = "Year", values_to = "MMR") %>%
  # Extract the 4-digit year from column names like "X2000..YR2000."
  # The regex "(?<=X)\\d{4}" means: find 4 digits that come after "X"
  mutate(Year = as.numeric(str_extract(Year, "(?<=X)\\d{4}"))) %>%
  # Remove any rows with missing years (shouldn't happen but good practice)
  filter(!is.na(Year))

# Check if we have data
cat("\nMaternal Mortality data found for", length(unique(mmr_df$Country.Name)), "countries\n")
cat("Year range:", min(mmr_df$Year), "to", max(mmr_df$Year), "\n")
cat("Sample of data:\n")
print(head(mmr_df))

# Create the time series plot
p <- ggplot(mmr_df, aes(x = Year, y = MMR, group = Country.Name, color = Country.Name)) +
  geom_line(alpha = 0.6, linewidth = 0.8) +
  labs(
    title = "Maternal Mortality Ratio Trends Over Time (2000-2024)",
    subtitle = "Modeled estimate per 100,000 live births",
    y = "Maternal Mortality Ratio",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Too many countries to show legend
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60")
  ) +
  scale_x_continuous(breaks = seq(2000, 2024, 5)) +
  scale_y_continuous(labels = scales::comma_format())

# Display the plot
print(p)

# Save the plot
ggsave("maternal_mortality_trends.png", plot = p, width = 12, height = 8, dpi = 300)
cat("\nPlot saved as 'maternal_mortality_trends.png'\n")

# Summary statistics
cat("\nSummary Statistics:\n")
summary_stats <- mmr_df %>%
  group_by(Year) %>%
  summarise(
    Countries = n(),
    Mean_MMR = round(mean(MMR, na.rm = TRUE), 2),
    Median_MMR = round(median(MMR, na.rm = TRUE), 2),
    Min_MMR = round(min(MMR, na.rm = TRUE), 2),
    Max_MMR = round(max(MMR, na.rm = TRUE), 2),
    .groups = 'drop'
  )

print(summary_stats)
  