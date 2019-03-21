#code to make a Shiny leaflet map for Irma Tweets
#EBG 3/2019

#https://rstudio.github.io/leaflet/shiny.html

#Install the needed libraries
library(shiny)
library(leaflet)
library(htmlwidgets)
library(sp)
library(tidyverse)
library(mapview)
library(shiny)
library(RColorBrewer)

#import the IRMA tweets (n.b., after the hydration step)
Irma_Tweets <- read_csv("~/Irma_Tweets.csv")

#URL field for each tweet
URL  = paste0("'","<a"," href", "=", Irma_Tweets$image_url , " target = '_blank'", ">", "TWEET</a>","'") #create hyperlinks from tweets

########################################
#build the Shiny UI with 4 sliders, color input, and legend check box: 
# S1) Image Score
# S2) Text Score
# S3) User Score
# S4) GIS Score
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Image Score", 0,1,
                            value = range(Irma_Tweets$imageScore), step = 0.1),
                sliderInput("rangetwo", "Text Score", 0,100,
                            value = range(Irma_Tweets$textScore), step = 0.1),
                sliderInput("rangethree", "User Score", 0,1,
                            value = range(Irma_Tweets$userScore), step = 0.1),
                sliderInput("rangefour", "GIS Score", 0,1,
                            value = range(Irma_Tweets$gisScore), step = 0.1),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
                checkboxInput("legend", "Show legend", TRUE)
  )
)
#########################

server <- function(input, output, session) {
  
  # These are the 4 reactive expressions for the sliders
  filteredData <- reactive({
    Irma_Tweets[Irma_Tweets$imageScore >= input$range[1] & Irma_Tweets$imageScore <= input$range[2] &
                  Irma_Tweets$textScore >= input$rangetwo[1] & Irma_Tweets$textScore <= input$rangetwo[2] & 
                  Irma_Tweets$userScore >= input$rangethree[1] & Irma_Tweets$userScore <= input$rangethree[2] & 
                  Irma_Tweets$gisScore >= input$rangefour[1] & Irma_Tweets$gisScore <= input$rangefour[2]
                ,]
  })
  
  # Reactive expression for the slider color, with colors coresponding to image score. 
  colorpal <- reactive({
    colorNumeric(input$colors, Irma_Tweets$imageScore)
  })
  
  output$map <- renderLeaflet({
    # Make the leaflet map. Pull the basemap from OSM. 
    leaflet(Irma_Tweets) %>% 
      addProviderTiles(providers$OpenStreetMap) %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  # observer for sliders:
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(
        stroke = TRUE, radius = 2000, weight = 1, color = "#777777",
        fillColor = ~pal(imageScore), fillOpacity = 0.7,
        popup = ~paste(text, "<img height='200' src = ", image_url, ">")
      )
  })
  
  #Observer for legend:
  observe({
    proxy <- leafletProxy("map", data = Irma_Tweets)
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Irma_Tweets$imageScore
      )
    }
  })
}

#Make it real:
shinyApp(ui, server)


  
  
  
  
  
