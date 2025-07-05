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
  geom_line(alpha = 0.7, linewidth = 0.9) +
  labs(
    title = "Maternal Mortality Ratio Trends Over Time (2000-2024)",
    subtitle = "Modeled estimate per 100,000 live births",
    y = "Maternal Mortality Ratio",
    x = "Year"
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
    
    # Remove legend (too many countries)
    legend.position = "none",
    
    # Plot margins
    plot.margin = margin(20, 20, 20, 20)
  ) +
  scale_x_continuous(breaks = seq(2000, 2024, 5)) +
  scale_y_continuous(labels = scales::comma_format()) +
  # Add a subtle border around the plot
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

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
  