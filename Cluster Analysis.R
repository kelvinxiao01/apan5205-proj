  # Load required libraries
  library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(factoextra)
  library(purrr)
  
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
  
  dev.new() 
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
  
  #View data
  mmr_clusters <- mmr_data %>%
    group_by(cluster, MMR_Level) %>%
    summarise(
      Countries = paste(sort(Country.Name), collapse = ", "),
      .groups = "drop"
    )
  
  # View in console
  print(mmr_clusters)
  
  mmr_cluster_counts <- mmr_data %>%
    distinct(Country.Name, cluster, MMR_Level) %>%  # ensure one row per country
    count(cluster, MMR_Level, name = "Num_Countries")
  
  print(mmr_cluster_counts)
  table(mmr_data$MMR_Level)
  
  
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
  
  # List countries in each cluster level for GDP growth
  gdp_growth_clusters <- gdp_data %>%
    group_by(cluster, Growth_Level) %>%
    summarise(
      Countries = paste(sort(Country.Name), collapse = ", "),
      .groups = "drop"
    )
  print(gdp_growth_clusters)
  View(gdp_growth_clusters)
  
  
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
  
  
  gdp_pc_clusters <- gdp_pc_data %>%
    group_by(PC_Level) %>%
    summarise(
      Countries = paste(sort(Country.Name), collapse = ", "),
      Num_Countries = n_distinct(Country.Name),
      .groups = "drop"
    )
  
  print(gdp_pc_clusters)
  View(gdp_pc_clusters)
  
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
  
  gs_clusters <- gs_data %>%
    group_by(Savings_Level) %>%
    summarise(
      Countries = paste(sort(Country.Name), collapse = ", "),
      Num_Countries = n_distinct(Country.Name),
      .groups = "drop"
    )
  
  print(gs_clusters)
  View(gs_clusters)
  
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
  
  write.csv(all_clusters,"All_Clusters_DataSet.csv")
  
  
  
  #10. SCORING SECTION (NO across())
  # -------------------------------
  
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
  
  # -------------------------------
  # 11. GROUPING INTO LOW/MEDIUM/HIGH
  # -------------------------------
  
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
  
  # -------------------------------
  # 12. RELATIONSHIP ANALYSIS
  # -------------------------------
  
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
  
  #REPLICATE FOR EACH YEAR!
  
  for (yr in 2000:2022) {
    
    #  Build raw_year, then dedupe on Country.Code
    raw_year <- tibble(
      Country.Code = mmr_data$Country.Code,
      mmr = mmr_data[[as.character(yr)]],
      afr = afr_data[[as.character(yr)]],
      growth = gdp_data[[as.character(yr)]],
      lfp = lfp_data[[as.character(yr)]],
      gdp_pc = gdp_pc_data[[as.character(yr)]],
      savings = gs_data[[as.character(yr)]],
      hivw = hivw_data[[as.character(yr)]],
      hivtot = hiv_total_data[[as.character(yr)]],
      edu = edu_data[[as.character(yr)]]
    ) %>%
      # rename columns to include year suffix
      rename_with(~ paste0(.x, "_", yr), -Country.Code) %>%
      distinct(Country.Code, .keep_all = TRUE)
    
    #  Join to your scored data, specifying many-to-many if needed
    analysis_data <- all_clusters_scored %>%
      left_join(raw_year, by = "Country.Code", relationship = "many-to-many")
    
    # Identify the newly created columns for this year
    corr_cols <- grep(paste0("_", yr, "$"), names(analysis_data), value = TRUE)
    if (length(corr_cols) < 2) {
      warning("Year ", yr, ": fewer than 2 indicators available for correlation—skipping.")
      next
    }
    
    #  Compute correlation matrix
    corr_mat <- analysis_data %>%
      select(all_of(corr_cols)) %>%
      na.omit() %>%
      cor(use = "pairwise.complete.obs")
    
    # Plot
    corrplot(
      corr_mat,
      method      = "color",
      type        = "upper",
      tl.cex      = 0.8,
      addCoef.col = "black",
      title       = paste("Correlations among", yr, "Indicators"),
      mar         = c(0,0,1,0)
    )
  }
  ----
# Define other indicators to compare with MMR
other_prefixes <- c("afr", "growth", "lfp", "gdp_pc", "savings", "hivw", "hivtot", "edu")
  
# Initialize history tibble with proper columns
  mmr_cor_history <- tibble(
    year = integer(),
    variable = character(),
    correlation = double()
  )
  
  for (yr in 2000:2022) {
    # 1. Build raw-year panel for that year
    raw_year <- tibble(
      Country.Code = mmr_data$Country.Code,
      mmr    = mmr_data[[as.character(yr)]],
      afr    = afr_data[[as.character(yr)]],
      growth = gdp_data[[as.character(yr)]],
      lfp    = lfp_data[[as.character(yr)]],
      gdp_pc = gdp_pc_data[[as.character(yr)]],
      savings= gs_data[[as.character(yr)]],
      hivw   = hivw_data[[as.character(yr)]],
      hivtot = hiv_total_data[[as.character(yr)]],
      edu    = edu_data[[as.character(yr)]]
    ) %>%
      rename_with(~ paste0(.x, "_", yr), -Country.Code) %>%
      distinct(Country.Code, .keep_all = TRUE)
    
    # 2. Merge (if you need score_group context; otherwise you could skip joining)
    analysis_data_year <- all_clusters_scored %>%
      left_join(raw_year, by = "Country.Code", relationship = "many-to-many")
    
    mmr_var <- paste0("mmr_", yr)
    others <- paste0(other_prefixes, "_", yr)
    
    # 3. For each other variable, compute correlation with MMR
    for (other_var in others) {
      if (!(mmr_var %in% names(analysis_data_year)) || !(other_var %in% names(analysis_data_year))) next
      
      df_pair <- analysis_data_year %>%
        select(all_of(c(mmr_var, other_var))) %>%
        rename(mmr_val = all_of(mmr_var), other_val = all_of(other_var)) %>%
        filter(!is.na(mmr_val) & !is.na(other_val))
      
      if (nrow(df_pair) < 10) next  # skip if too few observations
      
      r_val <- cor(df_pair$mmr_val, df_pair$other_val, use = "pairwise.complete.obs")
      
      mmr_cor_history <- mmr_cor_history %>%
        add_row(
          year = yr,
          variable = gsub(paste0("_", yr), "", other_var),
          correlation = r_val
        )
    }
  }
  
  # Wide version for inspection
  mmr_cor_wide <- mmr_cor_history %>%
    pivot_wider(names_from = variable, values_from = correlation)
  
  View(mmr_cor_wide)
  
  # Plot trajectories of MMR correlations with each indicator over time
  ggplot(mmr_cor_history, aes(x = year, y = correlation, color = variable)) +
    geom_line(size = 1) +
    geom_hline(yintercept = c(-0.5, -0.3, 0.3, 0.5), linetype = "dashed", alpha = 0.5) +
    labs(
      title = "MMR Correlation with Other Indicators Over Time",
      x = "Year",
      y = "Pearson r",
      color = "Indicator",
      caption = "Dashed lines: |r|=0.3 (moderate), |r|=0.5 (strong)"
    ) +
    theme_minimal()
  
  
# -------------------------------
# 13. GROUPED ANALYSIS  
# -------------------------------

library(dplyr)
library(ggplot2)
library(corrplot)

# --- Merge 2023 raw values in as mmr_2023, afr_2023, etc. ---
raw_2023 <- tibble(
  Country.Code = mmr_data$Country.Code,
  mmr_2023     = mmr_data[["2023"]],
  afr_2023     = afr_data[["2023"]],
  growth_2023  = gdp_data[["2023"]],
  lfp_2023     = lfp_data[["2023"]],
  gdp_pc_2023  = gdp_pc_data[["2023"]],
  savings_2023 = gs_data[["2023"]],
  hivw_2023    = hivw_data[["2023"]],
  hivtot_2023  = hiv_total_data[["2023"]],
  edu_2023     = edu_data[["2023"]]
)

analysis_data <- all_clusters_scored %>%
  left_join(raw_2023, by = "Country.Code")

# Make sure score_group is a factor with sensible order:
analysis_data <- analysis_data %>%
  mutate(score_group = factor(score_group, levels = c("Low", "Medium", "High")))

vars_2023 <- c("mmr_2023", "afr_2023", "growth_2023", "lfp_2023",
               "gdp_pc_2023", "savings_2023", "hivw_2023", "hivtot_2023", "edu_2023")

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


#REPLICATE FOR EACH YEAR (2000-2022)

# Ensure score_group is ordered
all_clusters_scored <- all_clusters_scored %>%
  mutate(score_group = factor(score_group, levels = c("Low", "Medium", "High")))

# Loop through each year
for (yr in 2000:2022) {
  
  # 13.a Build and merge raw values for this year
  raw_year <- tibble(
    Country.Code = mmr_data$Country.Code,
    mmr    = mmr_data[[as.character(yr)]],
    afr    = afr_data[[as.character(yr)]],
    growth = gdp_data[[as.character(yr)]],
    lfp    = lfp_data[[as.character(yr)]],
    gdp_pc = gdp_pc_data[[as.character(yr)]],
    savings= gs_data[[as.character(yr)]],
    hivw   = hivw_data[[as.character(yr)]],
    hivtot = hiv_total_data[[as.character(yr)]],
    edu    = edu_data[[as.character(yr)]]
  ) %>%
    rename_with(~ paste0(.x, "_", yr), -Country.Code) %>%
    distinct(Country.Code, .keep_all = TRUE)
  
  analysis_data <- all_clusters_scored %>%
    left_join(raw_year, by = "Country.Code", relationship = "many-to-many")
  
  # Prepare variable names
  vars_y <- paste0(c("mmr","afr","growth","lfp","gdp_pc","savings","hivw","hivtot","edu"), "_", yr)
  key_preds <- paste0(c("gdp_pc","growth","lfp"), "_", yr)
  
  # 13.1 Correlation matrix per score_group
  for (grp in levels(analysis_data$score_group)) {
    df_grp <- analysis_data %>%
      filter(score_group == grp) %>%
      select(all_of(vars_y)) %>%
      na.omit()
    if (ncol(df_grp) < 2) next
    
    mat <- cor(df_grp, use = "pairwise.complete.obs")
    corrplot(
      mat,
      method      = "color",
      type        = "upper",
      tl.cex      = 0.8,
      addCoef.col = "black",
      title       = paste("Correlations (", yr, ") —", grp, "Score"),
      mar         = c(0,0,1,0)
    )
  }
  
  # 13.2 Regression per score_group
  models <- analysis_data %>%
    group_by(score_group) %>%
    group_map(~ {
      rhs  <- paste(vars_y[-1], collapse = " + ")
      fmla <- as.formula(paste0(vars_y[1], " ~ ", rhs))
      lm(fmla, data = .x)
    }, .keep = TRUE)
  
  names(models) <- levels(analysis_data$score_group)
  for (grp in names(models)) {
    cat("\n\n=== Regression summary for", grp, "score group (", yr, ") ===\n")
    print(summary(models[[grp]]))
  }
  
  # 13.3 Faceted scatterplots for key predictors
  for (var in key_preds) {
    p <- ggplot(analysis_data, aes_string(x = var, y = vars_y[1])) +
      geom_point(aes(color = score_group), alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~ score_group) +
      labs(
        title = paste0("MMR (", yr, ") vs. ", gsub(paste0("_", yr), "", var),
                       " by Score Group"),
        x = gsub(paste0("_", yr), "", var),
        y = paste0("Maternal Mortality Ratio (", yr, ")"),
        color = "Score Group"
      ) +
      theme_minimal()
    print(p)
  }
}

#ANALYSIS OF ECON INDICATORS EFFECT ON MMR:
#This one is a general graph, does not consider the clustering
library(tidyverse)

## 1. Build tidy long panel ---------------------------------------------
long_four <- list(
  mmr   = mmr_data,
  grow  = gdp_data,
  gdpPC = gdp_pc_data,
  save  = gs_data
) %>%
  imap(~ .x %>%
         select(Country.Code, starts_with("2")) %>%
         pivot_longer(-Country.Code,
                      names_to  = "Year",
                      values_to = .y)) %>%   # .y is the list name
  reduce(full_join, by = c("Country.Code", "Year")) %>%
  mutate(Year = as.integer(Year))              # make numeric

# columns are: Country.Code | Year | mmr | grow | gdpPC | save


## 2. Pearson r by year --------------------------------------------------
cor_by_year <- long_four %>%
  group_by(Year) %>%
  summarise(
    r_Growth  = cor(mmr, grow,  use = "pairwise.complete.obs"),
    r_GDPpc   = cor(mmr, gdpPC, use = "pairwise.complete.obs"),
    r_Savings = cor(mmr, save,  use = "pairwise.complete.obs")
  ) %>%
  pivot_longer(-Year, names_to = "Indicator", values_to = "r") %>%
  filter(!is.na(r))                    # drop years that failed


## 3. Plot ---------------------------------------------------------------
ggplot(cor_by_year, aes(Year, r, colour = Indicator)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = c(-0.5, -0.3, 0.3, 0.5),
             linetype = "dashed", alpha = .4) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 3)) +
  scale_colour_manual(values = c(
    r_Growth  = "#1b9e77",
    r_GDPpc   = "#d95f02",
    r_Savings = "#7570b3"),
    labels  = c(r_Growth = "GDP growth",
                r_GDPpc  = "GDP per capita",
                r_Savings= "Gross savings")) +
  labs(title   = "MMR vs Economic Indicators ‒ Pearson r by Year",
       y       = "Pearson r",
       colour  = "Indicator",
       caption = "Dashed lines: |r| = 0.3 (moderate), 0.5 (strong)") +
  theme_minimal(base_size = 12)

#Several graphs, this stratifies the effects
library(dplyr)
library(tidyr)
library(ggplot2)

## ------------------------------------------------------------
## 1.  Build a yearly long panel with the variables we need
## ------------------------------------------------------------
econ_long <- purrr::map_dfr(
  2000:2023,
  \(yr) {
    tibble(
      Country.Code = mmr_data$Country.Code,
      Year         = yr,
      mmr          = mmr_data[[as.character(yr)]],
      growth       = gdp_data[[as.character(yr)]],
      gdp_pc       = gdp_pc_data[[as.character(yr)]],
      savings      = gs_data[[as.character(yr)]]
    )
  }
) |>
  left_join(all_clusters_scored |> select(Country.Code, score_group), 
            by = "Country.Code") |>
  filter(!is.na(score_group))          # keep rows that have a score-group

## ------------------------------------------------------------
## 2.  Correlation (Pearson r) by year × score_group × indicator
## ------------------------------------------------------------
indicators <- c("growth", "gdp_pc", "savings")

cor_by_year_grp <- econ_long |>
  pivot_longer(all_of(indicators), names_to = "Indicator", values_to = "value") |>
  group_by(Year, score_group, Indicator) |>
  summarise(
    r = cor(mmr, value, use = "pairwise.complete.obs"),
    .groups = "drop"
  )

## ------------------------------------------------------------
## 3.  One plot per economic indicator
## ------------------------------------------------------------
for (ind in indicators) {
  
  p <- ggplot(
    filter(cor_by_year_grp, Indicator == ind),
    aes(Year, r, colour = score_group, group = score_group)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    geom_hline(
      yintercept = c(-0.5, -0.3, 0.3, 0.5),
      linetype = "dashed", alpha = 0.4
    ) +
    scale_x_continuous(breaks = seq(2000, 2023, 3)) +
    labs(
      title   = paste("Pearson correlation: MMR vs", ind, "by cluster"),
      y       = "Pearson r",
      colour  = "Score group",
      caption = "Dashed lines: |r| = 0.3 (moderate)  |r| = 0.5 (strong)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  print(p)      # **important** – actually prints the plot inside the loop
}

# Countries in the “Low” composite-score tier
low_countries <- all_clusters_scored %>% 
  filter(score_group == "Low") %>%              # keep only the low tier
  arrange(desc(composite_score)) %>%            # optional: sort by score
  select(Country.Name, composite_score)         # keep just country + score

## 1) Coerce to a tibble first  ── keeps the nice tibble display
low_countries %>% 
  tibble::as_tibble() %>% 
  print(n = Inf)

## 2) Use base print without the extra argument
print(low_countries)              # shows as many rows as will fit
View(low_countries)  

#OTHER WAY OF ANALYZING


#Economic indicators only

econ_indicators <- c(
  "GDP per capita (constant 2015 US$)",
  "GDP growth (annual %)",
  "Gross savings (% of GDP)"
)

df_econ <- data %>%
  filter(Series.Name %in% econ_indicators)

# Long format 
econ_long <- df_econ %>%
  pivot_longer(
    cols  = matches("^\\d{4}$"),
    names_to  = "Year",
    values_to = "Value"
  ) %>%
  mutate(Year = as.integer(Year))

# one row per country with cluster levels 

all_clu_uni <- all_clusters_scored %>% 
  dplyr::group_by(Country.Code) %>% 
  dplyr::slice(1L) %>%          # <-- note the dplyr:: prefix
  dplyr::ungroup()

# attach the right cluster label for each indicator
econ_with_clu <- econ_long %>%
  left_join(
    all_clu_uni %>% 
      select(Country.Code, Growth_Level, PC_Level, Savings_Level),
    by = "Country.Code"
  ) %>%
  mutate(Cluster = case_when(
    Series.Name == "GDP growth (annual %)"                       ~ Growth_Level,
    Series.Name == "GDP per capita (constant 2015 US$)"          ~ PC_Level,
    Series.Name == "Gross savings (% of GDP)"                    ~ Savings_Level,
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Cluster), !is.na(Value))

#yearly mean per indicator × cluster
econ_summary <- econ_with_clu %>%
  group_by(Series.Name, Cluster, Year) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE), .groups = "drop")

# ARIMA forecast (5 yrs)
library(forecast)

econ_fc <- econ_summary %>%
  group_by(Series.Name, Cluster) %>% 
  group_split() %>%                      # list of data frames
  purrr::map_dfr(function(df) {
    ts_dat <- ts(df$avg_value, start = min(df$Year), frequency = 1)
    fit    <- auto.arima(ts_dat)
    fc     <- forecast(fit, h = 5, level = 0.95)
    
    hist <- tibble(
      Series.Name = df$Series.Name[1],
      Cluster     = df$Cluster[1],
      Year        = df$Year,
      Value       = df$avg_value,
      Type        = "Historical",
      Lo95        = NA_real_,
      Hi95        = NA_real_
    )
    
    fut <- tibble(
      Series.Name = df$Series.Name[1],
      Cluster     = df$Cluster[1],
      Year        = (max(df$Year)+1):(max(df$Year)+5),
      Value       = as.numeric(fc$mean),
      Type        = "Forecast",
      Lo95        = as.numeric(fc$lower[,1]),
      Hi95        = as.numeric(fc$upper[,1])
    )
    
    bind_rows(hist, fut)
  })

# Plot
nice_names <- c(
  "GDP per capita (constant 2015 US$)" = "GDP per capita",
  "GDP growth (annual %)"              = "GDP growth (%)",
  "Gross savings (% of GDP)"           = "Gross savings (% GDP)"
)

econ_fc$Series.Name <- nice_names[econ_fc$Series.Name]

library(ggplot2)

ggplot(econ_fc, aes(Year, Value, colour = Cluster, linetype = Type)) +
  geom_ribbon(
    data = subset(econ_fc, Type == "Forecast"),
    aes(ymin = Lo95, ymax = Hi95, fill = Cluster),
    alpha = 0.20, colour = NA
  ) +
  geom_line(size = 1) +
  facet_wrap(~ Series.Name, scales = "free_y") +
  scale_linetype_manual(values = c(Historical = "solid", Forecast = "dashed")) +
  labs(
    title    = "Trend and 5-Year Forecast for Key Economic Indicators, by Cluster",
    x        = "Year", 
    y        = "Cluster-Average Value",
    colour   = "Cluster", 
    fill     = "Cluster", 
    linetype = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Extremes & Change  (ECONOMIC INDICATORS)

# Identify min / max values for every indicator × cluster
econ_extremes <- econ_summary %>%          
  group_by(Series.Name, Cluster) %>%       
  summarise(
    Max_Value = max(avg_value, na.rm = TRUE),
    Max_Year  = Year[which.max(avg_value)],
    Min_Value = min(avg_value, na.rm = TRUE),
    Min_Year  = Year[which.min(avg_value)],
    .groups   = "drop"
  ) %>% 
  arrange(Series.Name, Cluster)

#  Add absolute change & a simple trend tag
econ_extremes <- econ_extremes %>% 
  mutate(
    Change = Max_Value - Min_Value,
    Trend  = case_when(
      Change >  0  ~ "Increase",
      Change <  0  ~ "Decrease",
      TRUE         ~ "Stable"
    )
  )

# Neat HTML-style table
library(kableExtra)

econ_extremes %>% 
  select(Series.Name, Cluster,
         Min_Value, Min_Year,
         Max_Value, Max_Year,
         Change, Trend) %>% 
  kable(digits = 1,
        caption = "Extremes and Net Change by Economic Indicator & Cluster") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE)

# Bar plot – how much did each cluster move?
library(ggplot2)

ggplot(econ_extremes,
       aes(x     = Cluster,
           y     = Change,
           fill  = Cluster)) +
  geom_col(width = 0.7) +
  facet_wrap(~ Series.Name, scales = "free_y") +
  labs(title = "Change Between Minimum and Maximum Values (2000-2023)",
       subtitle = "GDP growth, GDP per capita & Gross savings — by MMR composite cluster",
       x = NULL,
       y = "Δ  (Max − Min)",
       fill = "Cluster") +
  theme_minimal(base_size = 11)

# Quick table of the actual max/min years  (gt format)
library(gt)

econ_extremes %>% 
  select(Series.Name, Cluster, Min_Year, Max_Year) %>% 
  gt() %>% 
  tab_header(
    title = md("**Years of Minimum & Maximum Average Values**"),
    subtitle = "Economic indicators by MMR composite cluster"
  )

# 26. Long format – keep four series

data_long <- data %>% 
  pivot_longer(
    cols  = any_of(as.character(2000:2023)),   # use the names, not positions
    names_to  = "Year",
    values_to = "Value"
  ) %>% 
  mutate(Year = as.integer(Year))

vars_keep <- c(
  "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
  "GDP per capita (constant 2015 US$)",
  "GDP growth (annual %)",
  "Gross savings (% of GDP)"
)

econ_long  <- data_long %>% 
  filter(Series.Name %in% vars_keep) %>% 
  mutate(Series.Short = case_when(
    Series.Name == vars_keep[1] ~ "MMR",
    Series.Name == vars_keep[2] ~ "GDPpc",
    Series.Name == vars_keep[3] ~ "GDPg",
    Series.Name == vars_keep[4] ~ "Savings"
  )) %>% 
  select(Country.Name, Country.Code, Year, Series.Short, Value) %>% 
  pivot_wider(names_from = Series.Short, values_from = Value)


# Add one cluster label per country

cluster_lu <- all_clusters_scored %>%           # <- created earlier
  select(Country.Code, score_group) %>% 
  distinct()                                    # one row per country

econ_long <- econ_long %>% 
  left_join(cluster_lu, by = "Country.Code") %>% 
  filter(!is.na(score_group))

# factor order
econ_long$score_group <- factor(econ_long$score_group,
                                levels = c("Low", "Medium", "High"))


# Mixed-effects model
#     (Does higher GDPpc / GDPg / Saving predict lower MMR?)
library(lme4)
mix_mod <- lmer(
  MMR ~ GDPpc + GDPg + Savings + (1 | Country.Name),
  data = econ_long
)
summary(mix_mod)

# Helper to avoid NA/zero-variance problems

safe_cor <- function(x, y) {
  if (length(na.omit(x)) < 3 || sd(x, na.rm = TRUE) == 0 ||
      sd(y, na.rm = TRUE) == 0) return(NA_real_)
  cor(x, y, use = "pairwise.complete.obs")
}


# Dynamic correlations: one line per cluster

corr_ts <- econ_long %>% 
  group_by(score_group, Year) %>% 
  summarise(
    corr_MMR_GDPpc = safe_cor(MMR, GDPpc),
    corr_MMR_GDPg  = safe_cor(MMR, GDPg),
    corr_MMR_Sav   = safe_cor(MMR, Savings),
    .groups = "drop"
  ) %>% 
  pivot_longer(starts_with("corr_"),
               names_to  = "Pair",
               values_to = "r")

# nicer labels
corr_ts$Pair <- recode(corr_ts$Pair,
                       corr_MMR_GDPpc = "MMR vs GDP per capita",
                       corr_MMR_GDPg  = "MMR vs GDP growth",
                       corr_MMR_Sav   = "MMR vs Gross saving")


# Plot
library(ggplot2)
ggplot(corr_ts, aes(Year, r, colour = score_group)) +
  geom_line(size = 1) +
  facet_wrap(~ Pair, ncol = 1) +
  geom_hline(yintercept = c(-0.5, -0.3, 0, 0.3, 0.5),
             lty = "dashed", linewidth = 0.3, alpha = 0.5) +
  scale_colour_brewer(palette = "Set2", name = "Composite-score\ncluster") +
  labs(
    title = "Pearson correlation between MMR and macro variables by development cluster",
    subtitle = "Negative r ⇒ higher value of the macro variable is associated with lower maternal mortality",
    y = "Pearson r",
    x = "Year"
  ) +
  theme_minimal(base_size = 11)


# Pull the economic variables in long form


# quick filter-and-rename
grab_var <- function(df, indicator, newname){
  df %>% 
    filter(Series.Name == indicator) %>% 
    rename({{ newname }} := Value) %>% 
    select(Country.Name, Country.Code, Year, {{ newname }})
}

mmr_long  <- grab_var(data_long,
                      "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
                      MMR)

gdp_pc_long <- grab_var(data_long,
                        "GDP per capita (constant 2015 US$)",
                        GDP_pc)

gdp_g_long  <- grab_var(data_long,
                        "GDP growth (annual %)",
                        GDP_growth)

savings_long <- grab_var(data_long,
                         "Gross savings (% of GDP)",
                         Savings)

#  Merge them into one panel
econ_panel <- mmr_long %>% 
  left_join(gdp_pc_long, by = c("Country.Name","Country.Code","Year")) %>% 
  left_join(gdp_g_long , by = c("Country.Name","Country.Code","Year")) %>% 
  left_join(savings_long, by = c("Country.Name","Country.Code","Year"))

# Attach the composite-score cluster for each country
econ_panel <- econ_panel %>% 
  left_join(all_clu_uni %>%         # created in step-16b
              select(Country.Code, score_group),
            by = "Country.Code")

#  Mixed-effects model: MMR ~ the 3 economic vars
library(lme4)
library(lmerTest)                   # adds p-values

m_econ <- lmer(
  MMR ~ scale(GDP_pc) + scale(GDP_growth) + scale(Savings) +
    (1 | Country.Name),            # random intercept for each country
  data = econ_panel
)
summary(m_econ)

# Per-cluster Pearson r, year-by-year
safe_cor <- function(x, y){
  if(length(na.omit(x))<2 || sd(x,na.rm=TRUE)==0 ||
     sd(y,na.rm=TRUE)==0) return(NA_real_)
  cor(x, y, use = "complete.obs")
}

econ_corr <- econ_panel %>% 
  mutate(score_group = factor(score_group, levels = c("Low","Medium","High"))) %>% 
  group_by(score_group, Year) %>% 
  summarise(
    r_MMR_GDPpc   = safe_cor(MMR, GDP_pc),
    r_MMR_Growth  = safe_cor(MMR, GDP_growth),
    r_MMR_Savings = safe_cor(MMR, Savings),
    .groups = "drop"
  ) %>% 
  pivot_longer(starts_with("r_"),
               names_to  = "Corr_Type",
               values_to = "r")

# Plot dynamic correlations
library(ggplot2)

ggplot(econ_corr, aes(Year, r, colour = Corr_Type))+
  geom_line(size = 1)+
  facet_wrap(~ score_group)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c(
    r_MMR_GDPpc   = "#1b9e77",
    r_MMR_Growth  = "#7570b3",
    r_MMR_Savings = "#d95f02"))+
  labs(
    title  = "Year-by-Year Pearson correlations of MMR with\nGDP per-capita, GDP growth and Gross Savings",
    y      = "Pearson r",
    colour = "Pair"
  )+
  theme_minimal()

# Identify min / max average values per indicator × cluster
econ_summary <- econ_panel %>% 
  left_join(all_clu_uni %>% 
              select(Country.Code, Cluster = score_group),
            by = "Country.Code") %>% 
  pivot_longer(cols = c(GDP_pc, GDP_growth, Savings),
               names_to  = "Indicator",
               values_to = "Value") %>% 
  group_by(Indicator, Cluster, Year) %>% 
  summarise(avg_value = mean(Value, na.rm = TRUE), .groups = "drop")

econ_extremes <- econ_summary %>% 
  group_by(Indicator, Cluster) %>% 
  summarise(
    Max_Value = max(avg_value, na.rm = TRUE),
    Max_Year  = Year[which.max(avg_value)],
    Min_Value = min(avg_value, na.rm = TRUE),
    Min_Year  = Year[which.min(avg_value)],
    .groups   = "drop"
  )

#  Quick table
library(kableExtra)

econ_extremes %>% 
  kable(digits = 2,
        caption = "Maximum and Minimum Cluster Averages (2000-2023)") %>% 
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","hover"))
