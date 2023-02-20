library(tidyverse)
library(dplyr)
library(tm)
dirname(getwd())
saved_tracks_info <- read_csv("../data/saved_tracks.csv")

saved_tracks_info %>% 
  count(Artist, sort = TRUE) %>%
  rename(count = n) %>%
  top_n(n=30) %>%
  mutate(Artist=factor(Artist, levels = Artist)) %>% #convert to factor to preserve order
  ggplot(aes(x=Artist, y=count)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Top 30 Genres") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


saved_tracks_info %>%
  select(Acousticness,
         Danceability,
         Energy,
         Instrumentalness,
         Liveness,
         Loudness,
         Speechiness,
         Valence,
         Tempo) %>% #hist only for numeric
  gather() %>% #converts to key value
  ggplot(aes(value, fill = key)) + 
  facet_wrap(~ key, scales = "free", ncol =3) +
  geom_histogram(alpha = 0.7, bins = 30) + 
  ggtitle("My Saved Songs: Distribution of Track Feauture Values") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) 



# most popular genre
genres <- sort(table(unlist(strsplit(saved_tracks_info$Genres, ","))), 
               decreasing = TRUE) %>%
  as.data.frame() %>%
  rename(Genre = Var1)


genres %>%
  top_n(n = 30) %>%
  ggplot(aes(x=Genre, y=Freq)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Top 30 Genres") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  



k_data <- 
  saved_tracks_info %>%
  select(Acousticness,
         Danceability,
         Energy,
         Instrumentalness,
         Liveness,
         Loudness,
         Speechiness,
         Valence
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
print(k_value_with_max_avg_sil)


plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


# Load the data and keep only the relevant features

find_elbow <- function(data, k_max = 15) {
  wss <- sapply(1:k_max, function(k) {
    kmeans(data, centers = k)$tot.withinss
  })
  
  diffs <- diff(wss)
  elbow_index <- which(diffs < (mean(diffs) - 2 * sd(diffs)))[1] + 1
  
  return(elbow_index)
}

optimal_k <- find_elbow(saved_tracks_info)


set.seed(13437885)
fit <- kmeans(k_data, centers = k_value_with_max_avg_sil, nstart = 25)
fit$size


# we assign a cluster number to each song in both of out datasets
k_data$cluster <- as.factor(fit$cluster)
saved_tracks_info$cluster <- as.factor(fit$cluster)

library(viridis)

#Grouping the Clusters by Mean
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



library(httr)
library(spotifyr)
library(tidyverse)
library(knitr)
library(gtools) 
library(tm)
library(tidytext)
library(DBI)
library(RSQLite)
library(lubridate)
library(plotly)
library(dplyr)
library(stringr)
library(cluster)
library(factoextra)
library(viridis)
library(gridExtra)
library(wesanderson)
library(gt)
library(gtsummary)


lyrics <- read_csv("mini_lyrics_corpus1.csv") 
lyrics_corpus <- Corpus(VectorSource(lyrics))

# Convert the text to lower case
test <- tm_map(lyrics_corpus, content_transformer(tolower))
# Remove numbers
test <- tm_map(test, removeNumbers)
test <- tm_map(test, removeWords,c("verse",
                                   "chorus",
                                   "urlcopyembedcopy",
                                   "fail60embedshare",
                                   "record",
                                   "studio",
                                   "spotify",
                                   stopwords("english"))
)
# Text stemming - which reduces words to their root form
test <- tm_map(test, stemDocument)
#
test <- tm_map(test, removeWords,c(
  "record",
  "studio",
  "spotifi",
  "’s")
)
# Remove punctuations
test <- tm_map(test, removePunctuation)
# Eliminate extra white spaces
test <- tm_map(test, stripWhitespace)
TextDoc_dtm <- TermDocumentMatrix(test)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by decreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
head(dtm_d, 20)
nrc <- get_sentiments("nrc")

sentiment_analysis <- 
  dtm_d %>%
  inner_join(nrc)

sentiment_analysis %>% 
  group_by(sentiment) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(x = (sentiment),
             y = cnt,
             fill = sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE, name = "Sentiment") +
  theme_minimal() +
  theme(#legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    #axis.line = element_line(),
    axis.ticks = element_line()) +
  labs( title = "Number of words associated with each sentiment",
        x = "Sentiment",
        y = "Number of words") +
  coord_flip()



sentiment_analysis %>% 
  group_by(sentiment) %>%
  top_n(7, freq) %>%
  ungroup() %>%
  mutate(word = reorder(word,freq)) %>%
  ggplot(aes(x = word, 
             y = freq, fill = sentiment)) +
  theme_minimal() +
  geom_col() +
  facet_wrap(~sentiment, 
             scales = "free") +
  coord_flip() +
  theme(axis.text.x = element_text(size = 5), 
        axis.text.y = element_text(size = 7),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        #axis.line = element_line(),
        axis.ticks = element_line()) +
  scale_fill_viridis(discrete = TRUE, name = "Sentiment") +
  labs( title = "Most frequent words by sentiment category",
        x = "Word",
        y = "Frequency") 





  
  

pop.genre %>% 
  head(10) %>%
  ggplot(aes(y = reorder(Genres, total), x = total)) +
  geom_bar(aes(fill = total), stat = "identity") +
  scale_fill_gradient(low = "#F9CCDA", high = "#5A3DDA") +
  labs(title = "Top 10 most popular genre",
       y = "genre",
       x = "total of popular songs") +
  theme_minimal()




library(cluster)

# Load your Spotify data into a data frame

# Normalize the data if necessary
df_norm <- scale(saved_tracks_info)

# Create a function that uses kmeans and calculates the average silhouette for each k
calculate_silhouette <- function(df_norm, k) {
  kmeans_result <- kmeans(df_norm, k, nstart=25)
  silhouette_result <- silhouette(kmeans_result$cluster, dist(df_norm))
  return(mean(silhouette_result[,3]))
}

# Use the function to calculate the average silhouette for each k in a range of 2 to 20
k_values <- 2:20
silhouette_values <- sapply(k_values, calculate_silhouette, df_norm=df_norm)

# Plot the silhouette values and find the k with the highest value
plot(k_values, silhouette_values, type="b", xlab="Number of Clusters", ylab="Average Silhouette")
max_silhouette <- which.max(silhouette_values)
abline(v=k_values[max_silhouette], col="red")

#########################
#########################
#########################
# SHINY APP THAT WORKS


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

library(tidyverse)
library(dplyr)
library(tm)

saved_tracks_info <- read_csv("../data/saved_tracks.csv")
genres <- sort(table(unlist(strsplit(saved_tracks_info$Genres, ","))), 
               decreasing = TRUE) %>%
  as.data.frame() %>%
  rename(Genre = Var1)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("My Saved Spotify Songs Analysis"),
  
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("genrePlot"),
      plotOutput("artistPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$genrePlot <- renderPlot({
    
    genres %>%
      top_n(n = input$bins) %>%
      ggplot(aes(x=Genre, y=Freq)) + 
      geom_bar(stat = "identity") + 
      ggtitle("My Top Music Genres") + 
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(#axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    
  })
  
  
  output$artistPlot <- renderPlot({
    
    saved_tracks_info %>% 
      count(Artist, sort = TRUE) %>%
      rename(Freq = n) %>%
      top_n(n=input$bins) %>%
      mutate(Artist=factor(Artist, levels = Artist)) %>% #convert to factor to preserve order
      ggplot(aes(x=Artist, y=Freq)) + 
      geom_bar(stat = "identity") + 
      ggtitle("My Top Artists") + 
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(#axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  })
  
  
  
  output$distPlot <- renderPlot({
    
    saved_tracks_info %>%
      select(Acousticness,
             Danceability,
             Energy,
             Instrumentalness,
             Liveness,
             Loudness,
             Speechiness,
             Valence,
             Tempo) %>% #hist only for numeric
      gather() %>% #converts to key value
      ggplot(aes(value, fill = key)) + 
      facet_wrap(~ key, scales = "free", ncol =2) +
      geom_histogram(alpha = 0.7, bins = 30) + 
      ggtitle("My Saved Songs: Distribution of Track Feauture Values") + 
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(#axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
