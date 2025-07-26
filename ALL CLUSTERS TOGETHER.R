# Load required libraries
library(tidyverse)
library(mclust)
library(factoextra)

# 1️⃣ Define the indicators you want to include
selected_indicators <- c(
  "Adolescent fertility rate (births per 1,000 women ages 15-19)",
  "GDP growth (annual %)",
  "Labor force participation rate, female (% of female population ages 15+)",
  "GDP per capita (constant 2015 US$)",
  "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
  "Gross savings (% of GDP)",
  "Women's share of population ages 15+ living with HIV (%)",
  "Prevalence of HIV, total (% of population ages 15-49)"
)

# Load the dataset
data <- read.csv("wb_data_mice_pooled (4).csv")

# Check structure and summary
str(data)
summary(data)

# Check if there are missing values
sum(is.na(data))

colnames(data)

#Rename the year columns
colnames(data) <- gsub("X([0-9]{4})\\..*", "\\1", colnames(data))

# Remove the 2024 column right away
data <- data %>% select(-`2024`)

# 3️⃣ Keep only the series we need
ind_data <- data %>%
  filter(Series.Name %in% selected_indicators)

# 4️⃣ Pivot to wide: one row per Country.Name–Country.Code–Year, one column per indicator
wide_data <- ind_data %>%
  # turn your year‑columns into a single Year variable
  pivot_longer(
    cols      = matches("^20"), 
    names_to  = "Year", 
    values_to = "Value"
  ) %>%
  # spread indicators into their own columns, *keeping* Country.Name & Country.Code as id columns
  pivot_wider(
    id_cols     = c(Country.Name, Country.Code, Year),
    names_from  = Series.Name,
    values_from = Value
  ) %>%
  mutate(
    Year = as.integer(Year)
  )

# 5️⃣ Year-by-year clustering
cluster_results <- list()

for (yr in sort(unique(wide_data$Year))) {
  # 5.1 Subset just this year
  year_df <- wide_data %>% filter(Year == yr)
  
  # 5.2 Determine which of our selected_indicators are present
  avail_inds <- intersect(selected_indicators, colnames(year_df))
  if (length(avail_inds) < 2) {
    warning(sprintf("Year %d: fewer than 2 indicators present. Skipping.", yr))
    next
  }
  
  # 5.3 Drop rows with any NA in those indicators
  year_df <- year_df %>% drop_na(all_of(avail_inds))
  if (nrow(year_df) < length(avail_inds)) {
    warning(sprintf("Year %d: too few complete rows. Skipping.", yr))
    next
  }
  
  # 5.4 Extract data matrix and remove zero‑variance columns
  X_raw    <- year_df %>% select(all_of(avail_inds))
  var_vec  <- apply(X_raw, 2, var, na.rm = TRUE)
  keep_cols <- names(var_vec)[var_vec > 0]
  if (length(keep_cols) < 2) {
    warning(sprintf("Year %d: fewer than 2 varying columns. Skipping.", yr))
    next
  }
  
  # 5.5 Scale and run model‑based clustering
  X      <- scale(X_raw[, keep_cols, drop = FALSE])
  mc_res <- tryCatch(
    Mclust(X),
    error = function(e) {
      warning(sprintf("Year %d: Mclust error: %s", yr, e$message))
      NULL
    }
  )
  if (is.null(mc_res)) next
  
  # 5.6 Save the labels, preserving Country.Name
  cluster_results[[as.character(yr)]] <- tibble(
    Country.Name = year_df$Country.Name,
    Country.Code = year_df$Country.Code,
    Year         = yr,
    Cluster      = mc_res$classification
  )
}

# 6️⃣ Combine all years’ results
all_clusters <- bind_rows(cluster_results)

# 7️⃣ Inspect trajectories for a few countries
all_clusters %>%
  filter(Country.Name %in% c("Mexico", "India", "Brazil", "Nigeria")) %>%
  arrange(Country.Name, Year) %>%
  print(n = Inf)

# 8️⃣ Visualize clusters in PCA space for a chosen year
plot_year <- 2023
plot_df <- all_clusters %>%
  filter(Year == plot_year) %>%
  left_join(wide_data, by = c("Country.Name", "Country.Code", "Year"))

# Determine which indicators to plot (in case some are missing)
avail_inds_plot <- intersect(selected_indicators, colnames(plot_df))
X_plot <- scale(plot_df %>% select(all_of(avail_inds_plot)))

fviz_cluster(
  list(data    = X_plot,
       cluster = plot_df$Cluster),
  main         = paste("Country Clusters in", plot_year),
  geom         = "point",
  ellipse.type = "convex"
)
