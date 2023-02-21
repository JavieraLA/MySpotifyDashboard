library(tidyverse)
library(dplyr)
library(tm)
library(viridis)


saved_tracks_info <- read_csv("../data/saved_tracks.csv")

k_data <- 
  saved_tracks_info %>%
  select(Acousticness,
         Danceability,
         Energy,
         Instrumentalness,
         Liveness,
         Loudness,
         Speechiness,
         Valence,
         Tempo
  ) %>%
  mutate_if(is.numeric,scale)

# find the optimal number of clusters using the silhouette method
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(k_data, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(k_data))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:20
library(cluster)

set.seed(13437885)
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

k_values <- data.frame(k=k.values, avg_sil=avg_sil_values)
ggplot(k_values, aes(x=k, y=avg_sil)) + 
  geom_point(shape=19, size=5) + 
  geom_line() + 
  xlab("Number of clusters K") + 
  ylab("Average Silhouettes") + 
  ggtitle("Average Silhouettes vs Number of Clusters") + 
  theme_classic()

max_index <- which.max(avg_sil_values)
k_value_with_max_avg_sil <- k.values[max_index]

set.seed(13437885)
fit <- kmeans(k_data, centers = k_value_with_max_avg_sil, nstart = 25)
fit$size

# we assign a cluster number to each song in both of out datasets
k_data$cluster <- as.factor(fit$cluster)
saved_tracks_info$cluster <- as.factor(fit$cluster)

#Grouping the Clusters
cluster_mean <- 
  k_data %>%
  group_by(cluster) %>% 
  select(Acousticness,
         Danceability,
         Energy,
         Instrumentalness,
         Liveness,
         Loudness,
         Speechiness,
         Valence,
         Tempo) %>% 
  mutate_if(is.numeric, .funs = "round", digits = 2)

#Bar Plots for Clusters
b1 <- k_data %>% 
  ggplot(aes(x = cluster, 
             y = Energy, 
             fill = cluster)) +
  geom_boxplot() + 
  scale_fill_viridis(option = "D",discrete = TRUE, alpha=0.6) + 
  ggtitle("Clusters and Energy") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

b2 <- k_data %>% 
  ggplot(aes(x = cluster, 
             y = Acousticness, 
             fill = cluster)) +
  geom_boxplot() + 
  scale_fill_viridis(option = "D",discrete = TRUE, alpha=0.6) + 
  ggtitle("Clusters and Acousticness") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

b3 <- k_data %>% 
  ggplot(aes(x = cluster, 
             y = Danceability, 
             fill = cluster)) +
  geom_boxplot() + 
  scale_fill_viridis(option = "D",discrete = TRUE, alpha=0.6) + 
  ggtitle("Clusters and Danceability") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

b4 <- k_data %>% 
  ggplot(aes(x = cluster, 
             y = Valence, 
             fill = cluster)) +
  geom_boxplot() + 
  scale_fill_viridis(option = "D",discrete = TRUE, alpha=0.6) + 
  ggtitle("Clusters and Valence") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() 

features <- c("Acousticness", "Danceability", "Energy", "Instrumentalness", 
             "Liveness", "Loudness", "Speechiness", "Valence", "Tempo")

for (feature in features) {
  # Create a box plot of the feature values for each cluster
  print(feature)
  plot <- k_data %>% 
    ggplot(aes(x = cluster, 
               y = .data[[feature]], 
               fill = cluster)) +
    geom_boxplot() + 
    scale_fill_viridis(option = "D",discrete = TRUE, alpha=0.6) + 
    labs(title = paste("Box Plot of", feature, "by Cluster")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_minimal() 
  print(plot)
}

