setwd()
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
rect.hclust(hc_mmr, k = 3, border = "red")  
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


#VARIABLE 2 - AFR ####
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
rect.hclust(hc_afr, k = 3, border = "blue")  
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


#VARIABLE 3- GDP GROWTH ANUAL ####
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
rect.hclust(hc_gdp, k = 3, border = "darkgreen")  
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


#VARIABLE 4 - Labor Force ####
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
rect.hclust(hc_lfp, k = 3, border = "purple")  
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

#VARIABLE 5 - GDP PER CAPITA ####
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
rect.hclust(hc_gdp_pc, k = 3, border = "darkorange")  
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

#VARIABLE 6- GROSS SAVINGS ####
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

#VARIABLE 7 - WOMAN + HIV ####
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

#VARIABLE 8 - PREVALANCE OF HIV ####
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

#VARIABLE 9 - EDUCATIONAL ATTAINMENT ####
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

#SUMMARIZE TO ANALYZE!####
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

# 10. SCORING SECTION ####

# 10.1 Define mappings: higher = “better”
score_maps <- list(
  MMR_Level           = c("Low MMR"    = 3, "Medium MMR"   = 2, "High MMR"       = 1),
  AFR_Level           = c("Low AFR"    = 3, "Medium AFR"   = 2, "High AFR"       = 1),
  Growth_Level        = c("High growth"= 3, "Medium growth"= 2, "Low growth"     = 1),
  Participation_Level = c("High participation"= 3, "Medium participation"= 2, "Low participation"= 1),
  PC_Level            = c("High GDPpc" = 3, "Medium GDPpc" = 2, "Low GDPpc"      = 1),
  Savings_Level       = c("High savings"= 3, "Medium savings"= 2, "Low savings"    = 1),
  HIVW_Level          = c("Low share"  = 3, "Medium share"  = 2, "High share"      = 1),
  HIV_Total_Level     = c("Low prevalence"= 3, "Medium prevalence"= 2, "High prevalence"= 1),
  Edu_Level           = c("High education"= 3, "Medium education"= 2, "Low education"  = 1)
)

# 10.2 Start from all_clusters
all_clusters_scored <- all_clusters

# 10.3 For each “Level” column, create a <Level>_score column
for (lvl in names(score_maps)) {
  map   <- score_maps[[lvl]]
  score_col <- paste0(lvl, "_score")
  all_clusters_scored[[score_col]] <- dplyr::recode(
    all_clusters_scored[[lvl]],
    !!!map
  )
}

# 10.4 Compute unweighted composite (row‑mean of all *_score columns)
score_cols <- grep("_score$", names(all_clusters_scored), value = TRUE)

all_clusters_scored$composite_score <- rowMeans(
  all_clusters_scored[score_cols],
  na.rm = TRUE
)

# 10.5 (Optional) Weighted composite
weights <- c(
  MMR_Level_score           = 0.20,
  AFR_Level_score           = 0.10,
  Growth_Level_score        = 0.15,
  Participation_Level_score = 0.10,
  PC_Level_score            = 0.15,
  Savings_Level_score       = 0.10,
  HIVW_Level_score          = 0.05,
  HIV_Total_Level_score     = 0.05,
  Edu_Level_score           = 0.10
)

all_clusters_scored$weighted_score <- rowSums(
  sweep(
    all_clusters_scored[score_cols],
    2,
    weights[score_cols],
    "*"
  ),
  na.rm = TRUE
)

# 10.6 Inspect
dplyr::glimpse(all_clusters_scored)
View(all_clusters_scored)


# 11. GROUPING INTO LOW/MEDIUM/HIGH ####

# 11.1 Compute tertile cut‑points for composite_score
tertiles <- quantile(
  all_clusters_scored$composite_score,
  probs = c(0, 1/3, 2/3, 1),
  na.rm = TRUE
)

# 11.2 Create a factor “score_group” based on those cut‑points
all_clusters_scored <- all_clusters_scored %>%
  mutate(
    score_group = cut(
      composite_score,
      breaks   = tertiles,
      include.lowest = TRUE,
      labels   = c("Low", "Medium", "High")
    )
  )

# 11.3 (Optional) If you prefer to group on weighted_score instead, repeat:
tertiles_w <- quantile(
  all_clusters_scored$weighted_score,
  probs = c(0, 1/3, 2/3, 1),
  na.rm = TRUE
)

all_clusters_scored <- all_clusters_scored %>%
  mutate(
    weighted_group = cut(
      weighted_score,
      breaks   = tertiles_w,
      include.lowest = TRUE,
      labels   = c("Low", "Medium", "High")
    )
  )

# 11.4 Inspect how many countries fall into each group
table(all_clusters_scored$score_group)
table(all_clusters_scored$weighted_group)

# 11.5 View final data
View(all_clusters_scored)

# 12. RELATIONSHIP ANALYSIS ####

library(dplyr)
library(ggplot2)
library(corrplot)

# 12.1 Pull each country’s raw 2023 value for each indicator
raw_2023 <- tibble(
  Country.Code = mmr_data$Country.Code,
  mmr_2023 = mmr_data[["2023"]],
  afr_2023      = afr_data[["2023"]],
  growth_2023   = gdp_data[["2023"]],
  lfp_2023      = lfp_data[["2023"]],
  gdp_pc_2023   = gdp_pc_data[["2023"]],
  savings_2023  = gs_data[["2023"]],
  hivw_2023     = hivw_data[["2023"]],
  hivtot_2023   = hiv_total_data[["2023"]],
  edu_2023      = edu_data[["2023"]]
)

# 12.2 Merge raw values with your scored & grouped data
analysis_data <- all_clusters_scored %>%
  left_join(raw_2023, by = "Country.Code")

# 12.3 Correlation matrix of raw 2023 indicators
corr_vars <- analysis_data %>% 
  select(mmr_2023, afr_2023, growth_2023, lfp_2023,
         gdp_pc_2023, savings_2023, hivw_2023, hivtot_2023, edu_2023) %>%
  na.omit() %>% 
  cor(use = "pairwise.complete.obs")

corrplot(
  corr_vars, 
  method = "color", 
  type   = "upper", 
  tl.cex = 0.8, 
  addCoef.col = "black",
  title = "Correlations among 2023 Indicators",
  mar = c(0,0,1,0)
)


# 12.1 Pull each country’s raw 2000 value for each indicator
raw_2000 <- tibble(
  Country.Code = mmr_data$Country.Code,
  mmr_2000 = mmr_data[["2000"]],
  afr_2000     = afr_data[["2000"]],
  growth_2000   = gdp_data[["2000"]],
  lfp_2000      = lfp_data[["2000"]],
  gdp_pc_2000   = gdp_pc_data[["2000"]],
  savings_2000  = gs_data[["2000"]],
  hivw_2000     = hivw_data[["2000"]],
  hivtot_2000   = hiv_total_data[["2000"]],
  edu_2000      = edu_data[["2000"]]
)

# 12.2 Merge raw values with your scored & grouped data
analysis_data <- analysis_data %>%
  left_join(raw_2000, by = "Country.Code")

# 12.3 Correlation matrix of raw 2000 indicators
corr_vars <- analysis_data %>% 
  select(mmr_2000, afr_2000, growth_2000, lfp_2000,
         gdp_pc_2000, savings_2000, hivw_2000, hivtot_2000, edu_2000) %>%
  na.omit() %>% 
  cor(use = "pairwise.complete.obs")

corrplot(
  corr_vars, 
  method = "color", 
  type   = "upper", 
  tl.cex = 0.8, 
  addCoef.col = "black",
  title = "Correlations among 2000 Indicators",
  mar = c(0,0,1,0)
)

# 13. GROUPED ANALYSIS ####

library(dplyr)
library(ggplot2)
library(corrplot)

# Make sure score_group is a factor with sensible order:
analysis_data <- analysis_data %>%
  mutate(score_group = factor(score_group, levels = c("Low", "Medium", "High")))

vars_2023 <- c("mmr_2023", "afr_2023", "growth_2023", "lfp_2023",
               "gdp_pc_2023", "savings_2023", "hivw_2023", "hivtot_2023", "edu_2023")
vars_2023

# 13.1 Correlation matrix per group
for (grp in levels(analysis_data$score_group)) {
  df_grp <- filter(analysis_data, score_group == grp) %>%
    select(all_of(vars_2023)) %>%
    na.omit()
  
  mat <- cor(df_grp, use = "pairwise.complete.obs")
  corrplot(
    mat,
    method      = "color",
    type        = "upper",
    tl.cex      = 0.8,
    addCoef.col = "black",
    title       = paste("Correlations (2023) —", grp, "Score"),
    mar         = c(0,0,1,0)
  )
}

# 13.2 Regression per group
models <- analysis_data %>%
  group_by(score_group) %>%
  group_map(~ lm(
    mmr_2023 ~ afr_2023 + growth_2023 + lfp_2023 +
      gdp_pc_2023 + savings_2023 + edu_2023 +
      hivw_2023 + hivtot_2023,
    data = .x
  ), .keep = TRUE)

# Print summaries
names(models) <- levels(analysis_data$score_group)
for (grp in names(models)) {
  cat("\n\n=== Regression summary for", grp, "score group ===\n")
  print(summary(models[[grp]]))
}

# 13.3 Faceted scatterplots for key predictors
key_preds <- c("gdp_pc_2023", "growth_2023", "lfp_2023")

for (var in key_preds) {
  p <- ggplot(analysis_data, aes_string(x = var, y = "mmr_2023")) +
    geom_point(aes(color = score_group), alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ score_group) +
    labs(
      title = paste("MMR (2023) vs.", gsub("_2023", "", var),
                    "by Score Group"),
      x = gsub("_2023", "", var),
      y = "Maternal Mortality Ratio (2023)",
      color = "Score Group"
    ) +
    theme_minimal()
  print(p)
}

# 14. General Indicators ####
# 1. Build a long‐form table of average MMR by cluster and year
cluster_mmr_ts <- mmr_data %>%
  pivot_longer(
    cols      = starts_with("2"),    # all year columns
    names_to  = "Year",              # pivot column names into “Year”
    values_to = "mmr"                # pivot values into “mmr”
  ) %>%
  mutate(
    Year = as.integer(Year),         # convert Year to integer
    mmr  = as.numeric(mmr)           # ensure mmr is numeric
  ) %>%
  group_by(MMR_Level, Year) %>%      # group by cluster level and year
  summarise(
    avg_mmr = mean(mmr, na.rm = TRUE),  # compute average MMR
    .groups  = "drop"
  )

# 2. For each cluster, fit an ARIMA model and produce a 5-year forecast
library(purrr)
library(forecast)
library(ggplot2)

forecast_dfs <- cluster_mmr_ts %>%
  split(.$MMR_Level) %>%             # split by cluster level
  imap_dfr(function(df, level) {
    # convert the average MMR series into a ts object
    ts_data <- ts(df$avg_mmr,
                  start     = min(df$Year),
                  frequency = 1)
    # fit an automatic ARIMA
    fit <- auto.arima(ts_data)
    # forecast 5 years ahead at 95% confidence
    fc  <- forecast(fit, h = 5, level = 0.95)
    
    # bind historical data
    hist_df <- tibble(
      MMR_Level = level,
      Year      = df$Year,
      Value     = df$avg_mmr,
      Type      = "Historical",
      Lo95      = NA_real_,
      Hi95      = NA_real_
    )
    # bind forecast data
    fc_df <- tibble(
      MMR_Level = level,
      Year      = (max(df$Year) + 1):(max(df$Year) + 5),
      Value     = as.numeric(fc$mean),
      Type      = "Forecast",
      Lo95      = as.numeric(fc$lower[,1]),
      Hi95      = as.numeric(fc$upper[,1])
    )
    
    bind_rows(hist_df, fc_df)
  })

# 3. Plot all three clusters on one chart with ribbons for the forecast intervals
ggplot(forecast_dfs, aes(x = Year, y = Value, color = MMR_Level, linetype = Type)) +
  # shaded ribbon only for forecast period
  geom_ribbon(
    data = filter(forecast_dfs, Type == "Forecast"),
    aes(ymin = Lo95, ymax = Hi95, fill = MMR_Level),
    alpha = 0.2, color = NA
  ) +
  # lines for both historical (solid) and forecast (dashed)
  geom_line(size = 1) +
  scale_linetype_manual(values = c(Historical = "solid", Forecast = "dashed")) +
  labs(
    title    = "Average MMR Trends and 5-Year ARIMA Forecast by Cluster",
    x        = "Year",
    y        = "Average MMR",
    color    = "Cluster Level",
    linetype = ""
  ) +
  theme_minimal()

# 14.1 Map ####
# 1. Load necessary libraries
# install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "ggplot2", "dplyr"))
library(sf)             
library(rnaturalearth)  
library(rnaturalearthdata)
library(dplyr)         
library(ggplot2)        

# 2. Fetch a global ‘sf’ object at medium resolution
world <- ne_countries(scale = "medium", returnclass = "sf")

# 3. Prepare cluster label table
mmr_labels <- mmr_data %>%
  distinct(Country.Code, MMR_Level)

# 4. Join the cluster labels onto the spatial data
world_labeled <- world %>%
  left_join(mmr_labels, by = c("iso_a3" = "Country.Code"))

# 5. Plot: fill countries by their MMR cluster, gray out missing
ggplot(world_labeled) +
  geom_sf(aes(fill = MMR_Level), color = "white", size = 0.2) +
  scale_fill_manual(
    name    = "MMR Cluster",
    values  = c(
      "High MMR"   = "#e41a1c",
      "Medium MMR" = "#377eb8",
      "Low MMR"    = "#4daf4a"
    ),
    na.value = "grey90"
  ) +
  labs(
    title    = "Global Map of Maternal Mortality Clusters",
    subtitle = "Countries colored by MMR cluster level",
    caption  = "Data source: WB pooled dataset"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid.major = element_line(color = "white"),
    legend.position  = "bottom"
  )

# 14.2 Interactive Map ####
library(leaflet)
library(rnaturalearth)
library(sf)
library(dplyr)

# 1. Load world map as an sf object
world <- ne_countries(scale = "medium", returnclass = "sf")

# 2. Join MMR cluster labels by ISO3 country code
world_labeled <- world %>%
  left_join(
    mmr_data %>%
      distinct(Country.Code, MMR_Level) %>%
      # ensure the factor levels are in the desired order
      mutate(MMR_Level = factor(
        MMR_Level,
        levels = c("Low MMR", "Medium MMR", "High MMR")
      )),
    by = c("iso_a3" = "Country.Code")
  )

# 3. Print and inspect factor levels in case of mismatches
print(levels(world_labeled$MMR_Level))
print(table(world_labeled$MMR_Level, useNA = "ifany"))

# 4. Build a color palette
cluster_pal <- colorFactor(
  palette = c("#4daf4a", "#377eb8", "#e41a1c"),  # green, blue, red
  domain  = world_labeled$MMR_Level,             # factor values
  na.color = "grey90"                            # color for missing data
)

# 5. Render interactive leaflet map
leaflet(world_labeled) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%      # base map tiles
  addPolygons(
    fillColor   = ~cluster_pal(MMR_Level),             # fill by cluster
    weight      = 0.3,                                 # polygon border width
    color       = "white",                             # border color
    fillOpacity = 0.7                                  # transparency
  ) %>%
  addLegend(
    pal      = cluster_pal,                            # same palette
    values   = world_labeled$MMR_Level,                # legend categories
    title    = "MMR Cluster",                          # legend title
    position = "bottomright"                           # legend position
  )

#--- 14.3 Health economic indicators - Filter only indicators of interest
indicators_of_interest <- c(
  "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
  "Women's share of population ages 15+ living with HIV (%)",
  "Prevalence of HIV, total (% of population ages 15-49)",
  "Adolescent fertility rate (births per 1,000 women ages 15-19)"  # added AFR
)

df_filtered <- data %>%
  filter(Series.Name %in% indicators_of_interest)

#--- 15 - Transform data to long format (pivot years into rows)
df_long <- df_filtered %>%
  pivot_longer(
    cols = matches("^\\d{4}$"), 
    names_to = "Year", 
    values_to = "Value"
  ) %>%
  mutate(Year = as.integer(Year))

#--- 16 - Prepare unique version of all_clusters_scored by Country.Code (to avoid many-to-many join)
all_clusters_unique <- all_clusters_scored %>%
  group_by(Country.Code) %>%
  slice(1) %>%  # take only the first row per country
  ungroup()

#--- 17 - Join df_long with clusters
# Use only Country.Code for join
df_with_clusters <- df_long %>%
  left_join(
    all_clusters_unique %>%
      select(Country.Code, MMR_Level, HIVW_Level, HIV_Total_Level, AFR_Level),
    by = "Country.Code"
  ) %>%
  mutate(Cluster = case_when(
    Series.Name == "Maternal mortality ratio (modeled estimate, per 100,000 live births)" ~ MMR_Level,
    Series.Name == "Women's share of population ages 15+ living with HIV (%)" ~ HIVW_Level,
    Series.Name == "Prevalence of HIV, total (% of population ages 15-49)" ~ HIV_Total_Level,
    Series.Name == "Adolescent fertility rate (births per 1,000 women ages 15-19)" ~ AFR_Level,
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Cluster), !is.na(Value), !is.na(Year))

#--- 18 - Calculate mean per cluster, year, and indicator
df_summary <- df_with_clusters %>%
  group_by(Series.Name, Cluster, Year) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE), .groups = "drop")

#--- 19 - ARIMA Forecast for each cluster and indicator
library(forecast)
forecast_dfs <- df_summary %>%
  group_by(Series.Name, Cluster) %>%
  group_split() %>%
  map_dfr(function(df) {
    ts_data <- ts(df$avg_value, start = min(df$Year), frequency = 1)
    fit <- auto.arima(ts_data)
    fc <- forecast(fit, h = 5, level = 0.95)
    
    hist_df <- tibble(
      Series.Name = df$Series.Name[1],
      Cluster = df$Cluster[1],
      Year = df$Year,
      Value = df$avg_value,
      Type = "Historical",
      Lo95 = NA_real_,
      Hi95 = NA_real_
    )
    
    fc_df <- tibble(
      Series.Name = df$Series.Name[1],
      Cluster = df$Cluster[1],
      Year = (max(df$Year) + 1):(max(df$Year) + 5),
      Value = as.numeric(fc$mean),
      Type = "Forecast",
      Lo95 = as.numeric(fc$lower[,1]),
      Hi95 = as.numeric(fc$upper[,1])
    )
    
    bind_rows(hist_df, fc_df)
  })

#--- 20 - Plot trend + forecast
library(ggplot2)
forecast_dfs <- forecast_dfs %>%
  mutate(Series.Name = case_when(
    Series.Name == "Maternal mortality ratio (modeled estimate, per 100,000 live births)" ~ "Maternal mortality ratio",
    Series.Name == "Women's share of population ages 15+ living with HIV (%)" ~ "Women 15+ living with HIV (%)",
    Series.Name == "Prevalence of HIV, total (% of population ages 15-49)" ~ "Prevalence of HIV (15-49)",
    Series.Name == "Adolescent fertility rate (births per 1,000 women ages 15-19)" ~ "Adolescent fertility rate",
    TRUE ~ Series.Name
  ))

ggplot(forecast_dfs, aes(x = Year, y = Value, color = Cluster, linetype = Type)) +
  geom_ribbon(data = filter(forecast_dfs, Type == "Forecast"),
              aes(ymin = Lo95, ymax = Hi95, fill = Cluster), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~ Series.Name, scales = "free_y") +
  scale_linetype_manual(values = c(Historical = "solid", Forecast = "dashed")) +
  labs(title = "Trend and Forecast for MMR, HIV, and AFR by Cluster",
       x = "Year", y = "Average Value", color = "Cluster", linetype = "") +
  theme_minimal()

#--- 21 - Identify Max and Min values for each series
library(dplyr)
extremes_table <- df_summary %>%
  group_by(Series.Name, Cluster) %>%
  summarise(
    Max_Value = max(avg_value, na.rm = TRUE),
    Max_Year = Year[which.max(avg_value)],
    Min_Value = min(avg_value, na.rm = TRUE),
    Min_Year = Year[which.min(avg_value)],
    .groups = "drop"
  ) %>%
  arrange(Series.Name, Cluster)

# Display formatted table
library(kableExtra)
extremes_table %>%
  kable(caption = "Minimum and Maximum Values by Indicator and Cluster") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

#--- 22 - Table with min and max highlighted
extremes_summary <- extremes_table %>%
  select(Series.Name, Cluster,
         Min_Value, Min_Year,
         Max_Value, Max_Year) %>%
  arrange(Series.Name, Cluster)

print(extremes_summary)

#--- 23 - Calculate change and classify trends
library(gt)
extremes_table <- extremes_table %>%
  mutate(
    change = Max_Value - Min_Value,
    trend = case_when(
      change > 0 ~ "Increase",
      change < 0 ~ "Decrease",
      TRUE ~ "Stable"
    )
  )

#--- 24 - Bar plot comparing change by Cluster and Indicator
change_plot <- ggplot(extremes_table, aes(x = Cluster, y = change, fill = Cluster)) +
  geom_col() +
  facet_wrap(~ Series.Name, scales = "free_y") +
  labs(
    title = "Change between Max and Min Values by Cluster and Indicator",
    x = "Cluster",
    y = "Difference (Max - Min)"
  ) +
  theme_minimal()
print(change_plot)

#--- 25 - Table showing max and min years
year_table <- extremes_table %>%
  select(Series.Name, Cluster, Max_Year, Min_Year) %>%
  gt() %>%
  tab_header(title = "Years of Maximum and Minimum Values by Indicator and Cluster")
print(year_table)

#--- 26 - Convert original dataset to long format for all indicators
data_long <- data %>%
  pivot_longer(
    cols = `2000`:`2023`,
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(Year = as.integer(Year))

#--- 27 - Filter for MMR
mmr <- data_long %>%
  filter(Series.Name == "Maternal mortality ratio (modeled estimate, per 100,000 live births)") %>%
  rename(MMR = Value) %>%
  select(Country.Name, Country.Code, Year, MMR)

#--- 28 - Filter for total HIV prevalence
hiv_total <- data_long %>%
  filter(Series.Name == "Prevalence of HIV, total (% of population ages 15-49)") %>%
  rename(HIV_Total = Value) %>%
  select(Country.Name, Country.Code, Year, HIV_Total)

#--- 29 - Filter for female HIV prevalence
hiv_female <- data_long %>%
  filter(Series.Name == "Prevalence of HIV, female (% of population ages 15-49)") %>%
  rename(HIV_Female = Value) %>%
  select(Country.Name, Country.Code, Year, HIV_Female)

#--- 30 - Filter for adolescent fertility rate
adolescent_fertility <- data_long %>%
  filter(Series.Name == "Adolescent fertility rate (births per 1,000 women ages 15-19)") %>%
  rename(Adol_Fertility = Value) %>%
  select(Country.Name, Country.Code, Year, Adol_Fertility)

#--- 31 - Merge all variables by Country and Year
merged_all <- mmr %>%
  left_join(hiv_total, by = c("Country.Name", "Country.Code", "Year")) %>%
  left_join(hiv_female, by = c("Country.Name", "Country.Code", "Year")) %>%
  left_join(adolescent_fertility, by = c("Country.Name", "Country.Code", "Year"))

#--- 32 - Fit mixed effects model: MMR predicted by HIV and AFR with random intercept per country
library(lme4)
library(lmerTest)  # for p-values and detailed summary
model_mixed <- lmer(MMR ~ HIV_Total + Adol_Fertility + (1 | Country.Name), data = merged_all)
summary(model_mixed)

# Add Women’s share of population 15+ living with HIV (%)
hiv_share <- data_long %>%
  filter(Series.Name == "Women's share of population ages 15+ living with HIV (%)") %>%
  rename(HIV_WomenShare = Value) %>%
  select(Country.Name, Country.Code, Year, HIV_WomenShare)

# 1) Bring AFR cluster level and make it numeric (higher number = higher AFR)
afr_key <- afr_data %>%
  distinct(Country.Code, AFR_Level) %>%
  mutate(
    AFR_Level_num = case_when(
      AFR_Level == "Low AFR"    ~ 1L,
      AFR_Level == "Medium AFR" ~ 2L,
      AFR_Level == "High AFR"   ~ 3L,
      TRUE ~ NA_integer_
    )
  )

# 2) Join into the panel used for correlations
merged_wc <- merged_with_clusters %>%           # <- the long panel you just fixed
  left_join(afr_key, by = "Country.Code")

# 3) Recompute the per-year, per-cluster correlations
corr_cluster <- merged_wc %>%
  group_by(Cluster, Year) %>%
  summarise(
    corr_MMR_AFR        = safe_cor(MMR, AFR_Level_num),  # <- back to ordinal code
    corr_MMR_HIV_Total  = safe_cor(MMR, HIV_Total),
    corr_MMR_HIV_Women  = safe_cor(MMR, HIV_Women),
    .groups = "drop"
  )

# 4) Plot
ggplot(corr_cluster, aes(x = Year)) +
  geom_line(aes(y = corr_MMR_AFR,       color = "MMR vs AFR")) +
  geom_line(aes(y = corr_MMR_HIV_Total, color = "MMR vs HIV Total")) +
  geom_line(aes(y = corr_MMR_HIV_Women, color = "MMR vs HIV Women")) +
  facet_wrap(~ Cluster, nrow = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Correlation", color = "Correlation Type",
       title = "Dynamic Correlations of MMR with HIV and AFR by Cluster") +
  theme_minimal()


# 35. Period flag
merged_wc <- merged_wc %>%
  mutate(period = ifelse(Year < 2010, "Before 2010", "2010 and after"),
         period = factor(period, levels = c("Before 2010","2010 and after")))

# 36. Correlations by cluster × period
corr_period <- merged_wc %>%
  group_by(Cluster, period) %>%
  summarise(
    corr_MMR_HIV_Women = safe_cor(MMR, HIV_Women),   # or HIV_Female if that's your column
    corr_MMR_HIV_Total = safe_cor(MMR, HIV_Total),
    corr_MMR_AFR       = safe_cor(MMR, AFR_Level_num),  # use Adol_Fertility for continuous AFR
    .groups = "drop"
  )
print(corr_period)

# Plot
corr_long <- corr_period %>%
  tidyr::pivot_longer(
    cols = starts_with("corr_"),
    names_to = "Correlation_Type",
    values_to = "Correlation"
  )

ggplot(corr_long, aes(x = period, y = Correlation, fill = Correlation_Type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  facet_wrap(~ Cluster) +
  labs(
    title = "Correlation between MMR and HIV/AFR by Cluster and Period",
    x = "Period", y = "Pearson r", fill = "Pair"
  ) +
  theme_minimal()

# 37–38. High-MMR only: country-level correlations by period
high_mmr_data <- merged_wc %>% filter(Cluster == "High MMR")

country_corr <- high_mmr_data %>%
  group_by(Country.Name, period) %>%
  summarise(
    corr_MMR_HIV_Women = safe_cor(MMR, HIV_Women),
    corr_MMR_HIV_Total = safe_cor(MMR, HIV_Total),
    corr_MMR_AFR       = safe_cor(MMR, AFR_Level_num),
    .groups = "drop"
  )

country_corr_wide <- country_corr %>%
  tidyr::pivot_wider(
    names_from  = period,
    values_from = c(corr_MMR_HIV_Women, corr_MMR_HIV_Total, corr_MMR_AFR),
    names_sep   = "_"
  ) %>%
  # make syntactically safe names (spaces -> .)
  { setNames(., make.names(names(.))) } %>%
  mutate(
    delta_HIV_Women = corr_MMR_HIV_Women_2010.and.after - corr_MMR_HIV_Women_Before.2010,
    delta_HIV_Total = corr_MMR_HIV_Total_2010.and.after - corr_MMR_HIV_Total_Before.2010,
    delta_AFR       = corr_MMR_AFR_2010.and.after       - corr_MMR_AFR_Before.2010
  )

# 40–43. Differences for all clusters, top/bottom lists
corr_country_period <- merged_wc %>%
  group_by(Country.Name, Cluster, period) %>%
  summarise(
    corr_MMR_HIV_Women = safe_cor(MMR, HIV_Women),
    corr_MMR_HIV_Total = safe_cor(MMR, HIV_Total),
    corr_MMR_AFR       = safe_cor(MMR, AFR_Level_num),
    .groups = "drop"
  )

corr_country_wide <- corr_country_period %>%
  tidyr::pivot_wider(
    names_from = period,
    values_from = c(corr_MMR_HIV_Women, corr_MMR_HIV_Total, corr_MMR_AFR),
    names_sep = "_"
  ) %>%
  { setNames(., make.names(names(.))) } %>%
  mutate(
    diff_HIV_Women = corr_MMR_HIV_Women_2010.and.after - corr_MMR_HIV_Women_Before.2010,
    diff_HIV_Total = corr_MMR_HIV_Total_2010.and.after - corr_MMR_HIV_Total_Before.2010,
    diff_AFR       = corr_MMR_AFR_2010.and.after       - corr_MMR_AFR_Before.2010,
    diff_mean      = rowMeans(dplyr::across(starts_with("diff_")), na.rm = TRUE)
  )

top_bottom_by_cluster <- corr_country_wide %>%
  group_by(Cluster) %>%
  summarise(
    top_beneficiaries = paste(Country.Name[order(-diff_mean)][1:5], collapse = ", "),
    worst_performers  = paste(Country.Name[order(diff_mean)][1:5],  collapse = ", "),
    .groups = "drop"
  )

top_bottom_by_cluster %>% gt::gt()
