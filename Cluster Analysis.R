# Load required libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)

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

cluster_profiles$Year <- gsub("X|\\.\\.YR.*", "", cluster_profiles$Year)



#VARIABLE 2 - AFR
# Keep only Adolescent fertility rate rows
afr_data <- data %>%
  filter(`Series.Name` == "Adolescent fertility rate (births per 1,000 women ages 15-19)") %>%
  select(Country.Name, Country.Code, starts_with("2"))

# Check the structure
str(afr_data)

# Prepare numeric year data for clustering
afr_matrix <- afr_data %>% select(starts_with("2"))

# Standardize
afr_scaled <- scale(afr_matrix)

# Use Elbow Method to determine optimal number of clusters
fviz_nbclust(afr_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters (K-means – AFR)")

# Compute distances and hierarchical clustering
dist_afr <- dist(afr_scaled, method = "euclidean")
hc_afr  <- hclust(dist_afr, method = "ward.D2")

# Plot dendrogram and add rectangles in one run
plot(hc_afr, main = "Hierarchical Clustering of Countries by Adolescent Fertility",
     xlab = "", sub = "", cex = 0.5)
rect.hclust(hc_afr, k = 3, border = "blue")  # adjust k based on elbow

# Cut into 3 clusters
clusters_afr <- cutree(hc_afr, k = 3)

# Add raw cluster labels back to afr_data
afr_data <- afr_data %>%
  mutate(cluster = clusters_afr)

# Programmatically rank clusters by their 2023 mean AFR and assign Low/Medium/High
cluster_ranks <- afr_data %>%
  group_by(cluster) %>%
  summarise(mean_afr_2023 = mean(`2023`, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_afr_2023) %>%
  mutate(AFR_Level = c("Low AFR", "Medium AFR", "High AFR"))

# Join the level labels back into afr_data
afr_data <- afr_data %>%
  left_join(cluster_ranks %>% select(cluster, AFR_Level), by = "cluster")

# Check number of countries in each cluster level
table(afr_data$AFR_Level)

# Visualize the average AFR trend for each cluster level
afr_profiles <- afr_data %>%
  group_by(AFR_Level) %>%
  summarise(across(starts_with("2"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("2"),
               names_to = "Year",
               values_to = "Avg_AFR") %>%
  mutate(Year = as.integer(Year))

ggplot(afr_profiles, aes(x = Year, y = Avg_AFR, color = AFR_Level)) +
  geom_line(size = 1.2) +
  labs(title = "Average Adolescent Fertility Trends by Cluster Level",
       x = "Year", y = "Adolescent fertility rate", color = "Cluster Level") +
  theme_minimal()


#VARIABLE 3- GDP GROWTH ANUAL
# Keep only GDP growth rows
gdp_data <- data %>%
  filter(`Series.Name` == "GDP growth (annual %)") %>%
  select(Country.Name, Country.Code, starts_with("2"))

# Check the structure
str(gdp_data)

# Prepare numeric year data for clustering
gdp_matrix <- gdp_data %>% select(starts_with("2"))

# Standardize
gdp_scaled <- scale(gdp_matrix)

# Use Elbow Method to determine optimal number of clusters
fviz_nbclust(gdp_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters (K-means – GDP Growth)")

# Compute distances and hierarchical clustering
dist_gdp <- dist(gdp_scaled, method = "euclidean")
hc_gdp  <- hclust(dist_gdp, method = "ward.D2")

# Plot dendrogram and add rectangles in one run
plot(hc_gdp, main = "Hierarchical Clustering of Countries by GDP Growth",
     xlab = "", sub = "", cex = 0.5)
rect.hclust(hc_gdp, k = 3, border = "darkgreen")  # adjust k based on elbow

# Cut into 3 clusters
clusters_gdp <- cutree(hc_gdp, k = 3)

# Add raw cluster labels back to gdp_data
gdp_data <- gdp_data %>%
  mutate(cluster = clusters_gdp)

# Programmatically rank clusters by their 2023 mean growth and assign Low/Medium/High
cluster_ranks <- gdp_data %>%
  group_by(cluster) %>%
  summarise(mean_gdp2023 = mean(`2023`, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_gdp2023) %>%
  mutate(Growth_Level = c("Low growth", "Medium growth", "High growth"))

# Join the level labels back into gdp_data
gdp_data <- gdp_data %>%
  left_join(cluster_ranks %>% select(cluster, Growth_Level), by = "cluster")

# Check number of countries in each cluster level
table(gdp_data$Growth_Level)

# Visualize the average GDP growth trend for each cluster level
gdp_profiles <- gdp_data %>%
  group_by(Growth_Level) %>%
  summarise(across(starts_with("2"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("2"),
               names_to = "Year",
               values_to = "Avg_Growth") %>%
  mutate(Year = as.integer(Year))

ggplot(gdp_profiles, aes(x = Year, y = Avg_Growth, color = Growth_Level)) +
  geom_line(size = 1.2) +
  labs(title = "Average GDP Growth Trends by Cluster Level",
       x = "Year", y = "GDP growth (annual %)", color = "Cluster Level") +
  theme_minimal()


#VARIABLE 4 - Labor Force
# Keep only female labor force participation rate rows
lfp_data <- data %>%
  filter(`Series.Name` == "Labor force participation rate, female (% of female population ages 15+)") %>%
  select(Country.Name, Country.Code, starts_with("2"))

# Check the structure
str(lfp_data)

# Prepare numeric year data for clustering
lfp_matrix <- lfp_data %>% select(starts_with("2"))

# Standardize
lfp_scaled <- scale(lfp_matrix)

# Use Elbow Method to determine optimal number of clusters
fviz_nbclust(lfp_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters (K-means – Female Labor Participation)")

# Compute distances and hierarchical clustering
dist_lfp <- dist(lfp_scaled, method = "euclidean")
hc_lfp  <- hclust(dist_lfp, method = "ward.D2")

# Plot dendrogram and add rectangles in one run
plot(hc_lfp, main = "Hierarchical Clustering of Countries by Female Labor Force Participation",
     xlab = "", sub = "", cex = 0.5)
rect.hclust(hc_lfp, k = 3, border = "purple")  # adjust k based on elbow

# Cut into 3 clusters
clusters_lfp <- cutree(hc_lfp, k = 3)

# Add raw cluster labels back to lfp_data
lfp_data <- lfp_data %>%
  mutate(cluster = clusters_lfp)

# Programmatically rank clusters by their 2023 mean participation and assign Low/Medium/High
cluster_ranks <- lfp_data %>%
  group_by(cluster) %>%
  summarise(mean_lfp2023 = mean(`2023`, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_lfp2023) %>%
  mutate(Participation_Level = c("Low participation", "Medium participation", "High participation"))

# Join the level labels back into lfp_data
lfp_data <- lfp_data %>%
  left_join(cluster_ranks %>% select(cluster, Participation_Level), by = "cluster")

# Check number of countries in each cluster level
table(lfp_data$Participation_Level)

# Visualize the average participation trend for each cluster level
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

#VARIABLE 5 - GDP PER CAPITA
# Keep only GDP per capita rows
gdp_pc_data <- data %>%
  filter(`Series.Name` == "GDP per capita (constant 2015 US$)") %>%
  select(Country.Name, Country.Code, starts_with("2"))

# Check the structure
str(gdp_pc_data)

# Prepare numeric year data for clustering
gdp_pc_matrix <- gdp_pc_data %>% select(starts_with("2"))

# Standardize
gdp_pc_scaled <- scale(gdp_pc_matrix)

# Use Elbow Method to determine optimal number of clusters
fviz_nbclust(gdp_pc_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters (K-means – GDP per Capita)")

# Compute distances and hierarchical clustering
dist_gdp_pc <- dist(gdp_pc_scaled, method = "euclidean")
hc_gdp_pc  <- hclust(dist_gdp_pc, method = "ward.D2")

# Plot dendrogram and add rectangles in one run
plot(hc_gdp_pc, main = "Hierarchical Clustering by GDP per Capita",
     xlab = "", sub = "", cex = 0.5)
rect.hclust(hc_gdp_pc, k = 3, border = "darkorange")  # adjust k based on elbow

# Cut into 3 clusters
clusters_gdp_pc <- cutree(hc_gdp_pc, k = 3)

# Add raw cluster labels back to gdp_pc_data
gdp_pc_data <- gdp_pc_data %>%
  mutate(cluster = clusters_gdp_pc)

# Programmatically rank clusters by their 2023 mean value and assign Low/Medium/High
cluster_ranks <- gdp_pc_data %>%
  group_by(cluster) %>%
  summarise(mean_gdp_pc_2023 = mean(`2023`, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_gdp_pc_2023) %>%
  mutate(PC_Level = c("Low GDPpc", "Medium GDPpc", "High GDPpc"))

# Join the level labels back into gdp_pc_data
gdp_pc_data <- gdp_pc_data %>%
  left_join(cluster_ranks %>% select(cluster, PC_Level), by = "cluster")

# Check number of countries in each cluster level
table(gdp_pc_data$PC_Level)

# Visualize the average GDP per capita trend for each cluster level
gdp_pc_profiles <- gdp_pc_data %>%
  group_by(PC_Level) %>%
  summarise(across(starts_with("2"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("2"),
               names_to = "Year",
               values_to = "Avg_GDPpc") %>%
  mutate(Year = as.integer(Year))

ggplot(gdp_pc_profiles, aes(x = Year, y = Avg_GDPpc, color = PC_Level)) +
  geom_line(size = 1.2) +
  labs(title = "Average GDP per Capita Trends by Cluster Level",
       x = "Year", y = "GDP per Capita (constant 2015 US$)", color = "Cluster Level") +
  theme_minimal()

#VARIABLE 6- GROSS SAVINGS
# Keep only Gross savings (% of GDP) rows
gs_data <- data %>%
  filter(`Series.Name` == "Gross savings (% of GDP)") %>%
  select(Country.Name, Country.Code, starts_with("2"))

# Check the structure
str(gs_data)

# Prepare numeric year data for clustering
gs_matrix <- gs_data %>% select(starts_with("2"))

# Standardize
gs_scaled <- scale(gs_matrix)

# Use Elbow Method to determine optimal number of clusters
fviz_nbclust(gs_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters (K-means – Gross Savings)")

# Compute distances and hierarchical clustering
dist_gs <- dist(gs_scaled, method = "euclidean")
hc_gs  <- hclust(dist_gs, method = "ward.D2")

# Plot dendrogram and add rectangles in one run
plot(hc_gs, main = "Hierarchical Clustering by Gross Savings (% of GDP)",
     xlab = "", sub = "", cex = 0.5)
rect.hclust(hc_gs, k = 3, border = "darkblue")  # adjust k based on elbow

# Cut into 3 clusters
clusters_gs <- cutree(hc_gs, k = 3)

# Add raw cluster labels back to gs_data
gs_data <- gs_data %>%
  mutate(cluster = clusters_gs)

# Programmatically rank clusters by their 2023 mean savings and assign Low/Medium/High
cluster_ranks <- gs_data %>%
  group_by(cluster) %>%
  summarise(mean_gs_2023 = mean(`2023`, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_gs_2023) %>%
  mutate(Savings_Level = c("Low savings", "Medium savings", "High savings"))

# Join the level labels back into gs_data
gs_data <- gs_data %>%
  left_join(cluster_ranks %>% select(cluster, Savings_Level), by = "cluster")

# Check number of countries in each cluster level
table(gs_data$Savings_Level)

# Visualize the average savings trend for each cluster level
gs_profiles <- gs_data %>%
  group_by(Savings_Level) %>%
  summarise(across(starts_with("2"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("2"),
               names_to = "Year",
               values_to = "Avg_Savings") %>%
  mutate(Year = as.integer(Year))

ggplot(gs_profiles, aes(x = Year, y = Avg_Savings, color = Savings_Level)) +
  geom_line(size = 1.2) +
  labs(title = "Average Gross Savings Trends by Cluster Level",
       x = "Year", y = "Gross Savings (% of GDP)", color = "Cluster Level") +
  theme_minimal()

#VARIABLE 7 - WOMAN + HIV
# Keep only Women’s share of population ages 15+ living with HIV (%) rows
hivw_data <- data %>%
  filter(`Series.Name` == "Women's share of population ages 15+ living with HIV (%)") %>%
  select(Country.Name, Country.Code, starts_with("2"))

# Check the structure
str(hivw_data)

# Prepare numeric year data for clustering
hivw_matrix <- hivw_data %>% select(starts_with("2"))

# Standardize
hivw_scaled <- scale(hivw_matrix)

# Use Elbow Method to determine optimal number of clusters
fviz_nbclust(hivw_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters (K-means – Women’s HIV Share)")

# Compute distances and hierarchical clustering
dist_hivw <- dist(hivw_scaled, method = "euclidean")
hc_hivw  <- hclust(dist_hivw, method = "ward.D2")

# Plot dendrogram and add rectangles in one run
plot(hc_hivw, main = "Hierarchical Clustering by Women’s HIV Share",
     xlab = "", sub = "", cex = 0.5)
rect.hclust(hc_hivw, k = 3, border = "magenta")  # adjust k based on elbow

# Cut into 3 clusters
clusters_hivw <- cutree(hc_hivw, k = 3)

# Add raw cluster labels back to hivw_data
hivw_data <- hivw_data %>%
  mutate(cluster = clusters_hivw)

# Programmatically rank clusters by their 2023 mean value and assign Low/Medium/High
cluster_ranks <- hivw_data %>%
  group_by(cluster) %>%
  summarise(mean_hivw_2023 = mean(`2023`, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_hivw_2023) %>%
  mutate(HIVW_Level = c("Low share", "Medium share", "High share"))

# Join the level labels back into hivw_data
hivw_data <- hivw_data %>%
  left_join(cluster_ranks %>% select(cluster, HIVW_Level), by = "cluster")

# Check number of countries in each cluster level
table(hivw_data$HIVW_Level)

# Visualize the average Women’s HIV share trend for each cluster level
hivw_profiles <- hivw_data %>%
  group_by(HIVW_Level) %>%
  summarise(across(starts_with("2"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("2"),
               names_to = "Year",
               values_to = "Avg_HIVW") %>%
  mutate(Year = as.integer(Year))

ggplot(hivw_profiles, aes(x = Year, y = Avg_HIVW, color = HIVW_Level)) +
  geom_line(size = 1.2) +
  labs(title = "Average Women’s HIV Share Trends by Cluster Level",
       x = "Year", y = "Women’s HIV Share (%)", color = "Cluster Level") +
  theme_minimal()

#VARIABLE 8 - PREVALANCE OF HIV
# Keep only Prevalence of HIV, total (% of population ages 15-49) rows
hiv_total_data <- data %>%
  filter(`Series.Name` == "Prevalence of HIV, total (% of population ages 15-49)") %>%
  select(Country.Name, Country.Code, starts_with("2"))

# Check the structure
str(hiv_total_data)

# Prepare numeric year data for clustering
hiv_total_matrix <- hiv_total_data %>% select(starts_with("2"))

# Standardize
hiv_total_scaled <- scale(hiv_total_matrix)

# Use Elbow Method to determine optimal number of clusters
fviz_nbclust(hiv_total_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters (K-means – HIV Prevalence Total)")

# Compute distances and hierarchical clustering
dist_hiv_total <- dist(hiv_total_scaled, method = "euclidean")
hc_hiv_total  <- hclust(dist_hiv_total, method = "ward.D2")

# Plot dendrogram and add rectangles in one run
plot(hc_hiv_total, main = "Hierarchical Clustering by HIV Prevalence (Total)",
     xlab = "", sub = "", cex = 0.5)
rect.hclust(hc_hiv_total, k = 3, border = "darkred")  # adjust k based on elbow

# Cut into 3 clusters
clusters_hiv_total <- cutree(hc_hiv_total, k = 3)

# Add raw cluster labels back to hiv_total_data
hiv_total_data <- hiv_total_data %>%
  mutate(cluster = clusters_hiv_total)

# Programmatically rank clusters by their 2023 mean prevalence and assign Low/Medium/High
cluster_ranks <- hiv_total_data %>%
  group_by(cluster) %>%
  summarise(mean_hiv_total_2023 = mean(`2023`, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_hiv_total_2023) %>%
  mutate(HIV_Total_Level = c("Low prevalence", "Medium prevalence", "High prevalence"))

# Join the level labels back into hiv_total_data
hiv_total_data <- hiv_total_data %>%
  left_join(cluster_ranks %>% select(cluster, HIV_Total_Level), by = "cluster")

# Check number of countries in each cluster level
table(hiv_total_data$HIV_Total_Level)

# Visualize the average HIV prevalence trend for each cluster level
hiv_total_profiles <- hiv_total_data %>%
  group_by(HIV_Total_Level) %>%
  summarise(across(starts_with("2"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("2"),
               names_to = "Year",
               values_to = "Avg_HIV_Total") %>%
  mutate(Year = as.integer(Year))

ggplot(hiv_total_profiles, aes(x = Year, y = Avg_HIV_Total, color = HIV_Total_Level)) +
  geom_line(size = 1.2) +
  labs(title = "Average HIV Prevalence (Total) Trends by Cluster Level",
       x = "Year", y = "HIV Prevalence Total (%)", color = "Cluster Level") +
  theme_minimal()

#VARIABLE 9 - EDUCATIONAL ATTAINMENT
# Keep only female upper secondary educational attainment rows
edu_data <- data %>%
  filter(`Series.Name` == "Educational attainment, at least completed upper secondary, population 25+, female (%) (cumulative)") %>%
  select(Country.Name, Country.Code, starts_with("2"))

# Check the structure
str(edu_data)

# Prepare numeric year data for clustering
edu_matrix <- edu_data %>% select(starts_with("2"))

# Standardize
edu_scaled <- scale(edu_matrix)

# Use Elbow Method to determine optimal number of clusters
fviz_nbclust(edu_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters (K-means – Female Education)")

# Compute distances and hierarchical clustering
dist_edu <- dist(edu_scaled, method = "euclidean")
hc_edu  <- hclust(dist_edu, method = "ward.D2")

# Plot dendrogram and add rectangles in one run
plot(hc_edu, main = "Hierarchical Clustering by Female Upper Secondary Education",
     xlab = "", sub = "", cex = 0.5)
rect.hclust(hc_edu, k = 3, border = "darkgreen")  # adjust k based on elbow

# Cut into 3 clusters
clusters_edu <- cutree(hc_edu, k = 3)

# Add raw cluster labels back to edu_data
edu_data <- edu_data %>%
  mutate(cluster = clusters_edu)

# Programmatically rank clusters by their 2023 mean attainment and assign Low/Medium/High
cluster_ranks <- edu_data %>%
  group_by(cluster) %>%
  summarise(mean_edu2023 = mean(`2023`, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_edu2023) %>%
  mutate(Edu_Level = c("Low education", "Medium education", "High education"))

# Join the level labels back into edu_data
edu_data <- edu_data %>%
  left_join(cluster_ranks %>% select(cluster, Edu_Level), by = "cluster")

# Check number of countries in each cluster level
table(edu_data$Edu_Level)

# Visualize the average education trend for each cluster level
edu_profiles <- edu_data %>%
  group_by(Edu_Level) %>%
  summarise(across(starts_with("2"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("2"),
               names_to = "Year",
               values_to = "Avg_Edu") %>%
  mutate(Year = as.integer(Year))

ggplot(edu_profiles, aes(x = Year, y = Avg_Edu, color = Edu_Level)) +
  geom_line(size = 1.2) +
  labs(title = "Average Female Upper Secondary Education Trends by Cluster Level",
       x = "Year", y = "Completion Rate (%)", color = "Cluster Level") +
  theme_minimal()

#SUMMARIZE TO ANALYZE!
library(purrr)

#  Build one summary table for each variable

mmr_lvl <- mmr_data %>%
  select(Country.Name, Country.Code, MMR_Level)

afr_lvl <- afr_data %>%
  select(Country.Name, Country.Code, AFR_Level)

# GDP growth
gdp_lvl <- gdp_data %>%
  select(Country.Name, Country.Code, Growth_Level)

# Female labor force participation
lfp_lvl <- lfp_data %>%
  select(Country.Name, Country.Code, Participation_Level)

# GDP per capita
gdp_pc_lvl <- gdp_pc_data %>%
  select(Country.Name, Country.Code, PC_Level)

# Gross savings
gs_lvl <- gs_data %>%
  select(Country.Name, Country.Code, Savings_Level)

# Women’s share HIV
hivw_lvl <- hivw_data %>%
  select(Country.Name, Country.Code, HIVW_Level)

# HIV prevalence total
hiv_tot_lvl <- hiv_total_data %>%
  select(Country.Name, Country.Code, HIV_Total_Level)

# Female upper‑secondary education
edu_lvl <- edu_data %>%
  select(Country.Name, Country.Code, Edu_Level)

# Named list
cluster_list <- list(
  mmr       = mmr_lvl,
  afr       = afr_lvl,
  gdp       = gdp_lvl,
  lfp       = lfp_lvl,
  gdp_pc    = gdp_pc_lvl,
  savings   = gs_lvl,
  hiv_w     = hivw_lvl,
  hiv_total = hiv_tot_lvl,
  edu       = edu_lvl
)

# Full‑join them all together by Country.Name & Country.Code
all_clusters <- reduce(
  cluster_list,
  full_join,
  by = c("Country.Name", "Country.Code")
)

# CHECK-VIEW
glimpse(all_clusters)
View(all_clusters) 




