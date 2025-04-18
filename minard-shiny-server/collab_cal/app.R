library(shiny)
library(tidyverse)
library(lubridate)
library(rhandsontable)
library(reactable)
library(odbc)
library(DBI)
library(pool)

server <- function(input, output, session) {
  
  long_to_wide <- function(df){
    hot_to_r(df) %>% 
      select(-day) %>% 
      pivot_longer(!date, names_to = "time", values_to = "available")
  }

  output$p1 <- renderRHandsontable({
    
    enhanced_cal %>% 
      rhandsontable(width = 1000, height = 325) %>% 
      hot_col("date", readOnly = TRUE) %>% 
      hot_col("day", readOnly = TRUE)
    
    })
  
  output$p2 <- renderRHandsontable({
    
    enhanced_cal %>% 
      rhandsontable(width = 1000, height = 325) %>% 
      hot_col("date", readOnly = TRUE) %>% 
      hot_col("day", readOnly = TRUE)
    
    })
  
  output$p3 <- renderRHandsontable({
    
    enhanced_cal %>% 
      rhandsontable(width = 1000, height = 325) %>% 
      hot_col("date", readOnly = TRUE) %>% 
      hot_col("day", readOnly = TRUE)
    
    })
  
  output$overlap <- renderReactable({
    
    long_to_wide(input$p1) %>%
      inner_join(
        long_to_wide(input$p2),
        by = c("date", "time"),
        suffix = c("_p1", "_p2")
      ) %>%
      inner_join(
        long_to_wide(input$tre),
        by = c("date", "time")
      ) %>% 
      rename(available_p3 = available) %>%
      filter(available_p1, available_p2, available_p3) %>% 
      select(date, time) %>% 
      reactable(defaultPageSize = 5)

  })
  
}

hours <- 9:18

base_cal <- tibble(date = seq.Date(Sys.Date(), Sys.Date() %m+% months(2), by = 1)) %>% 
  mutate(day = wday(date, label = TRUE)) 

enhanced_cal <- seq(1, length(hours) - 1) %>% 
  map_chr(~ paste(hours[.x], hours[.x + 1], sep = " - ")) %>% 
  reduce(~ mutate(.x, !!as.symbol(.y) := FALSE), .init = base_cal)

main <- mainPanel(
  h3("p1"),
  rHandsontableOutput("p1"),
  h3("p2"),
  rHandsontableOutput("p2"),
  h3("p3"),
  rHandsontableOutput("p3")
)

side <- sidebarPanel(fluidRow(p("Time Overlap"), reactableOutput("overlap")))

ui <- fluidPage(side, main)

shinyApp(ui = ui, server = server)
