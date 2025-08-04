# Load required libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
#install.packages("plm")
#install.packages("tidyverse")
#install.packages("factoextra")

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

