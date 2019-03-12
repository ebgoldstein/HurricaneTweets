#code to make a Shiny leaflet map for Irma Tweets
#EBG 3/2019

#https://rstudio.github.io/leaflet/shiny.html

#install package to map (leaflet),  save html (htmlwidgets).drawing objects (sp)
#install.packages(c("shiny", "leaflet", "htmlwidgets", "sp", "tidyverse"))

##get leaflet
library(shiny)
library(leaflet)
library(htmlwidgets)
library(sp)
library(tidyverse)
library(mapview)
library(shiny)
library(RColorBrewer)

#import the IRMA tweets
Irma_Tweets <- read_csv("~/Irma_Tweets.csv")

#URL field
URL  = paste0("'","<a"," href", "=", Irma_Tweets$image_url , " target = '_blank'", ">", "TWEET</a>","'") #create hyperlinks from tweets

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

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    Irma_Tweets[Irma_Tweets$imageScore >= input$range[1] & Irma_Tweets$imageScore <= input$range[2] &
                  Irma_Tweets$textScore >= input$rangetwo[1] & Irma_Tweets$textScore <= input$rangetwo[2] & 
                  Irma_Tweets$userScore >= input$rangethree[1] & Irma_Tweets$userScore <= input$rangethree[2] & 
                  Irma_Tweets$gisScore >= input$rangefour[1] & Irma_Tweets$gisScore <= input$rangefour[2]
                ,]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, Irma_Tweets$imageScore)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(Irma_Tweets) %>% 
      addProviderTiles(providers$OpenStreetMap) %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(
                       #lng = Irma_Tweets$longitude, 
                       #lat = Irma_Tweets$latitude,
                       
                       #popup=popupImage(Irma_Tweets$image_url,embed=TRUE,height=200),
                       #popup = paste0(Irma_Tweets$text, "<img height='200' src = ", Irma_Tweets$image_url, ">"),
                       
                       stroke = TRUE, radius = 2000, weight = 1, color = "#777777",
                       fillColor = ~pal(imageScore), fillOpacity = 0.7,
                       popup = ~paste(text, "<img height='200' src = ", image_url, ">")
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
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

shinyApp(ui, server)


  
  
  
  
  
