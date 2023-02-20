#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
#library(shinyWidgets)
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
    titlePanel("Spotify Analysis"),
    tabsetPanel(
        tabPanel("Project Description",
                 h3("Project Description"),
                 "This is where I will write all about the project and the purpose dskjadasjdad
                 sdasdasdsadasdasd
                 sdasdasdasdasdasdasdas
                 d
                 sadsadsa
                 "
        ),
        
        
        tabPanel("Descriptive Analysis",
                 fluidRow(
                     column(12,
                            sliderInput("artist_n",
                                        "Number of Artists:",
                                        min = 1,
                                        max = 50,
                                        value = 30)
                     )
                 ), 
                 
                 fluidRow(
                     column(12, 
                            plotOutput("artistPlot")
                     )
                 ),
                 tags$hr(),
                 
                 fluidRow(
                     column(12,
                            sliderInput("genre_n",
                                        "Number of Genres:",
                                        min = 1,
                                        max = 50,
                                        value = 30)
                     )
                 ), 
                 
                 fluidRow(
                     column(12, 
                            plotOutput("genrePlot")
                     )
                 ),
                 
                 tags$hr(),
                 
                 
                 fluidRow(
                     column(12, 
                            plotOutput("distPlot")
                     )
                 ),
                 
                 tags$hr()
                 
                 
                 
                 
                 
        ),
        tabPanel("Clustering",
                 h3("K-means clustering"),
                 "Avg silhouettes vs num clusters",
                 "Percentage and/or number of songs per cluster",
                 "Boxplots of features for the different clusters"
        ),
        tabPanel("Sentiment Analysis",
                 h3("Sentiment Analysis by Cluster"),
                 "Sentiment Analysis for each cluster",
                 "Include textbox to add stopwords",
                 "Filter by language"
        )
    ),
    
    tags$hr(),
    

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$genrePlot <- renderPlot({
        
        genres %>%
            top_n(n = input$genre_n) %>%
            ggplot(aes(x=Genre, y=Freq, fill = Genre)) + 
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
            top_n(n=input$artist_n) %>%
            mutate(Artist=factor(Artist, levels = Artist)) %>% #convert to factor to preserve order
            ggplot(aes(x=Artist, y=Freq, fill = Artist)) + 
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
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
