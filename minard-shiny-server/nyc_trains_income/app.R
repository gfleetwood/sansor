library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(scales)
library(sf)
#library(crosstalk)

server <- function(input, output) {
  
  df <- reactive(
    train_stops %>%
      filter(train == input$trains) %>% 
      arrange(station_order) #%>% SharedData$new()
  )
  
  output$plot <- renderPlotly({
    
    plot_ly(df(), x = ~station_order, hoverinfo = 'text') %>%
      add_trace(
        y = ~median_income, 
        mode = 'lines+markers', 
        text = ~paste(NAME, "\n", dollar(median_income), "\n", borough)
        )
    
  })

  output$map <- renderLeaflet({
    
    leaflet(data = df(), width = "100%") %>% 
      addTiles() %>% 
      addCircleMarkers(~longitude, ~latitude, label = ~as.character(NAME), color = "#F00", radius = 2) %>% 
      addPolylines(~longitude, ~latitude, weight = 3, opacity = 5)
    
  })
  
  output$plot2 <- renderPlotly({
    
    p <- df() %>%
      ggplot(aes(x = station_order, y =  median_income)) +
      geom_line() + 
      geom_point(
        aes(
          color = "red", 
          text = paste("Name:", NAME, "\nMedian Income: ", format(median_income, big.mark = ",", scientific = F))
          )
        ) +
      theme_bw() +
      guides(color = FALSE) 
    
    ggplotly(p, tooltip = c("text")) %>% 
      layout(showlegend = FALSE)
    
  })
  
  output$map2 <- renderLeaflet({
    
    leaflet(data = df(), width = "100%") %>% 
      addTiles() %>% 
      addCircleMarkers(~longitude, ~latitude, label = ~as.character(NAME), color = "#F00", radius = 2) %>% 
      addPolylines(~longitude, ~latitude, weight = 3, opacity = 5)
    
  })
  
}

train_stops <- read_csv("nyc_train_stops_income_order_borough.csv") %>% 
  mutate(income = map_chr(median_income, ~ format(.x, big.mark = ",", scientific = F)))

train_stops_sf = st_as_sf(train_stops, coords = c("longitude", "latitude"))

# https://data.cityofnewyork.us/City-Government/Borough-Boundaries/tqmj-j8zm
nyc_sf <- st_read("Borough Boundaries")

trains <- sort(as.character(unique(train_stops$train)))

r1 = fluidRow(
  column(12, h3("NYC Subway & Income"), align = 'center'),
  column(
    12,
    radioButtons("trains", "Trains", trains, selected = trains[1], inline = T),
    align = 'center'
    )
  )

r2 = fluidRow(
  #column(6, plotlyOutput("plot")), 
  #column(6, leafletOutput("map"))
  column(6, plotlyOutput("plot2")), 
  column(6, leafletOutput("map2"))
)

ui <- fluidPage(r1, r2)

shinyApp(ui = ui, server = server)












