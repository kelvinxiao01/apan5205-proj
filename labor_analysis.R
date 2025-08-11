# Load required libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
#install.packages("plm")
#install.packages("tidyverse")
#install.packages("factoextra")

#Preliminary data loading (taken from other files)

# Load the dataset
data <- read.csv("wb_data_mice_pooled.csv")

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

# Keep only MMR rows
mmr_data <- data %>%
  filter(`Series.Name` == "Maternal mortality ratio (modeled estimate, per 100,000 live births)") %>%
  select(Country.Name, Country.Code, starts_with("2"))

# Check the structure
str(mmr_data)

# Prepare numeric year data for clustering
mmr_matrix <- mmr_data %>% select(starts_with("2"))

# Standardize
mmr_scaled <- scale(mmr_matrix)

# Use Elbow Method to determine optimal number of clusters
fviz_nbclust(mmr_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters (K-means – MMR)")

# Compute distances and hierarchical clustering
dist_mmr <- dist(mmr_scaled, method = "euclidean")
hc_mmr  <- hclust(dist_mmr, method = "ward.D2")

# Plot dendrogram and add rectangles in one run
plot(hc_mmr, main = "Hierarchical Clustering of Countries by MMR",
     xlab = "", sub = "", cex = 0.5)
rect.hclust(hc_mmr, k = 3, border = "red")  # adjust k based on elbow

# Cut into 3 clusters
clusters_mmr <- cutree(hc_mmr, k = 3)

# Add raw cluster labels back to mmr_data
mmr_data <- mmr_data %>%
  mutate(cluster = clusters_mmr)

# Programmatically rank clusters by their 2023 mean MMR and assign Low/Medium/High
cluster_ranks <- mmr_data %>%
  group_by(cluster) %>%
  summarise(mean_mmr_2023 = mean(`2023`, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_mmr_2023) %>%
  mutate(MMR_Level = c("Low MMR", "Medium MMR", "High MMR"))

# Join the level labels back into mmr_data
mmr_data <- mmr_data %>%
  left_join(cluster_ranks %>% select(cluster, MMR_Level), by = "cluster")

# Check number of countries in each cluster level
table(mmr_data$MMR_Level)

# Visualize the average MMR trend for each cluster level
mmr_profiles <- mmr_data %>%
  group_by(MMR_Level) %>%
  summarise(across(starts_with("2"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("2"),
               names_to = "Year",
               values_to = "Avg_MMR") %>%
  mutate(Year = as.integer(Year))

ggplot(mmr_profiles, aes(x = Year, y = Avg_MMR, color = MMR_Level)) +
  geom_line(size = 1.2) +
  labs(title = "Average Maternal Mortality Trends by Cluster Level",
       x = "Year", y = "Maternal mortality ratio", color = "Cluster Level") +
  theme_minimal()

# LABOR FORCE ANALYSIS

#VARIABLE 4 - Labor Force ####

# 1. Inspect available series names (run interactively)
# unique(data$Series.Name)

# 2. Filter more flexibly
pattern <- "labor force participation"
lfp_data <- data %>%
  filter(grepl(pattern, Series.Name, ignore.case = TRUE)) %>%
  select(Country.Name, Country.Code, starts_with("2"))

# 3. Fail fast if nothing matched
if (nrow(lfp_data) == 0) {
  stop("No rows matched. Check your filter against unique(data$Series.Name).")
}

# 4. Continue as before
lfp_matrix <- lfp_data %>% select(starts_with("2"))
lfp_scaled <- scale(lfp_matrix)

print(lfp_matrix)
print(lfp_scaled)

# 5. Elbow/WSS plot
fviz_nbclust(lfp_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters")

# 6. Compute distances and hierarchical clustering
dist_lfp <- dist(lfp_scaled, method = "euclidean")
hc_lfp  <- hclust(dist_lfp, method = "ward.D2")

# 7. Plot dendrogram and add rectangles in one run
plot(hc_lfp, main = "Hierarchical Clustering of Countries by Female Labor Force Participation",
     xlab = "", sub = "", cex = 0.5)
rect.hclust(hc_lfp, k = 3, border = "purple")  # adjust k based on elbow

# 8. Cut into 3 clusters
clusters_lfp <- cutree(hc_lfp, k = 3)

# 9. Add raw cluster labels back to lfp_data
lfp_data <- lfp_data %>%
  mutate(cluster = clusters_lfp)

# 10. Programmatically rank clusters by their 2023 mean participation and assign Low/Medium/High
cluster_ranks <- lfp_data %>%
  group_by(cluster) %>%
  summarise(mean_lfp2023 = mean(`2023`, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_lfp2023) %>%
  mutate(Participation_Level = c("Low participation", "Medium participation", "High participation"))

# 11. Join the level labels back into lfp_data
lfp_data <- lfp_data %>%
  left_join(cluster_ranks %>% select(cluster, Participation_Level), by = "cluster")

# 12. Check number of countries in each cluster level
table(lfp_data$Participation_Level)

# 13. Visualize the average participation trend for each cluster level
lfp_profiles <- lfp_data %>%
  group_by(Participation_Level) %>%
  summarise(across(starts_with("2"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("2"),
               names_to = "Year",
               values_to = "Avg_LFP") %>%
  mutate(Year = as.integer(Year))

ggplot(lfp_profiles, aes(x = Year, y = Avg_LFP, color = Participation_Level)) +
  geom_line(size = 1.2) +
  labs(title = "Average Female Labor Force Participation Trends by Cluster Level",
       x = "Year", y = "Participation Rate (%)", color = "Cluster Level") +
  theme_minimal()



# 1. Install / load the necessary packages
# install.packages(c("sf","rnaturalearth","rnaturalearthdata","ggspatial"))
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggplot2)

# 2. Get a world‐country shapefile via rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")  
# this has a column iso_a3 for 3-letter codes

# 3. Prepare your data
# assume lfp_data has columns Country.Code (ISO3) and Participation_Level
# if your codes are named differently, rename them to iso_a3
map_data <- world %>%
  left_join(
    lfp_data %>% 
      select(iso_a3 = Country.Code, Participation_Level) %>% 
      distinct(),
    by = "iso_a3"
  )

# 4. Plot
ggplot(map_data) +
  geom_sf(aes(fill = Participation_Level), color = "gray70", size = 0.1) +
  scale_fill_manual(
    values = c(
      "Low participation"    = "#e41a4c",
      "Medium participation" = "#377eb8",
      "High participation"   = "#4daf4a"
    ),
    na.value = "lightgray",
    drop = FALSE
  ) +
  labs(
    title = "Female Labor Force Participation Clusters, 2023",
    fill  = "Participation Level"
  ) +
  theme_minimal() +
  theme(
    panel.background   = element_blank(),
    panel.grid.major   = element_line(color = "white", linetype = "solid"),  # ← fixed
    legend.position    = "bottom"
  )


library(dplyr)
library(ggplot2)

# 1. Prepare a combined data frame for 2023
corr_2023 <- lfp_data %>%
  select(Country.Code, LFP_2023 = `2023`, Participation_Level) %>%
  inner_join(
    mmr_data %>% select(Country.Code, MMR_2023 = `2023`),
    by = "Country.Code"
  )

# 2. Compute and print the Pearson correlation coefficient
cor_coef <- cor(corr_2023$LFP_2023, corr_2023$MMR_2023, use = "complete.obs")
message("Pearson r (2023): ", round(cor_coef, 3))

# 3. Scatter-plot with smoothing line
ggplot(corr_2023, aes(x = LFP_2023, y = MMR_2023, color = Participation_Level)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = sprintf("MMR vs Female LFP, 2023 (r = %.2f)", cor_coef),
    x = "Female Labor Force Participation, 2023 (%)",
    y = "Maternal Mortality Rate, 2023 (deaths per 100,000 live births)",
    color = "LFP Cluster"
  ) +
  theme_minimal()


library(tidyr)
library(purrr)

# 1. Pivot both data sets to long form
lfp_long <- lfp_data %>%
  select(Country.Code, starts_with("2")) %>%
  pivot_longer(cols = starts_with("2"), names_to = "Year", values_to = "LFP") %>%
  mutate(Year = as.integer(Year))

mmr_long <- mmr_data %>%
  select(Country.Code, starts_with("2")) %>%
  pivot_longer(cols = starts_with("2"), names_to = "Year", values_to = "MMR") %>%
  mutate(Year = as.integer(Year))

# 2. Join and compute correlation per year
corr_time <- lfp_long %>%
  inner_join(mmr_long, by = c("Country.Code", "Year")) %>%
  group_by(Year) %>%
  summarize(
    r = cor(LFP, MMR, use = "complete.obs"),
    .groups = "drop"
  )

# 3. Plot the year‐by‐year correlation
ggplot(corr_time, aes(x = Year, y = r)) +
  geom_line(size = 1) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Yearly Pearson Correlation between Female LFP and MMR",
    subtitle = "Negative values indicate higher participation ↔ lower mortality",
    x = "Year",
    y = "Pearson’s r"
  ) +
  theme_minimal()

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Specify the snapshot years you want
years <- c(2000, 2005, 2010, 2015, 2020)

# 2. Prepare a combined long‐format data frame for only those years
corr_multi <- lfp_data %>%
  select(Country.Code, Participation_Level, all_of(as.character(years))) %>%
  pivot_longer(
    cols      = as.character(years),
    names_to  = "Year",
    values_to = "LFP"
  ) %>%
  mutate(Year = as.integer(Year)) %>%
  inner_join(
    mmr_data %>%
      select(Country.Code, all_of(as.character(years))) %>%
      pivot_longer(
        cols      = as.character(years),
        names_to  = "Year",
        values_to = "MMR"
      ) %>%
      mutate(Year = as.integer(Year)),
    by = c("Country.Code", "Year")
  )

# 3. Compute per‐year Pearson’s r for annotation (optional)
r_per_year <- corr_multi %>%
  group_by(Year) %>%
  summarize(r = cor(LFP, MMR, use = "complete.obs"), .groups = "drop")

# 4. Plot with facet_wrap
ggplot(corr_multi, aes(x = LFP, y = MMR, color = Participation_Level)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  # add per‐facet corr coeff as text
  geom_text(
    data = r_per_year,
    aes(x = Inf, y = Inf, label = sprintf("r = %.2f", r)),
    hjust = 1.1, vjust = 1.5, inherit.aes = FALSE
  ) +
  facet_wrap(~ Year, ncol = 3) +
  labs(
    title = "MMR vs Female LFP Across Selected Years",
    x     = "Female Labor Force Participation (%)",
    y     = "Maternal Mortality Rate (per 100,000 live births)",
    color = "LFP Cluster"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )


