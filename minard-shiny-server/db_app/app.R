library(shiny)
library(tidyverse)
library(odbc)
library(DBI)
library(purrr)
library(DT)
library(glue)

server <- function(input, output, session) {
  
  values <- reactiveValues()
  values$history <- data.frame(original = character(), change = character(), stringsAsFactors = FALSE) 
  
  observeEvent(
    input$con, 
    {
      values$db_con <- dbConnect(
        odbc::odbc(),
        Driver = input$driver, 
        Database = input$db,
        Port = input$port,
        Server = input$server,
        UID = input$un,
        PWD = input$pw,
        sslmode = "require"
      )
      values$connection_msg <- "Database Connection Established"
    }
  )
  
  observeEvent(
    input$trigger_query,
    {values$tbl <- dbGetQuery(values$db_con, "SELECT * FROM iris")} 
  )
  
  observeEvent(
    input$write_db,
    {dbWriteTable(values$db_con, input$db_tbl, values$tbl, overwrite = TRUE)}
  )
  
  output$con_msg <- renderText({values$connection_msg})
  output$table <- renderDT(values$tbl, editable = 'cell')
  
  
  observeEvent(
    input$table_cell_edit, 
    {
      info <- input$table_cell_edit
      original_val <- values$tbl[info$row, info$col]
      
      changes <- data.frame(
        row = info$row,
        col = info$col,
        original = original_val, 
        change = info$value, 
        stringsAsFactors = FALSE
      ) 
      
      values$history <- changes %>% 
        mutate(row_nums = 0) %>% 
        rbind(values$history, .) %>%
        mutate(row_nums = seq(1, n(), 1))
      values$tbl <- editData(values$tbl, input$table_cell_edit, 'table')
      
    })
  
  output$history <- renderDT(values$history)
  
  # output$download_data <- downloadHandler(
  #   filename = function() {
  #     "test.csv"
  #   },
  #   content = function(file) {
  #     write.csv(values$tbl, file, row.names = FALSE)
  #   }
  # )
  # 
  # output$download_history <- downloadHandler(
  #   filename = function() {
  #     "test.csv"
  #   },
  #   content = function(file) {
  #     write.csv(values$history, file, row.names = FALSE)
  #   }
  # )
  
}

sidebar <- sidebarPanel(
  textInput("driver", "Driver", "PostgreSQL ANSI"),
  textInput("db", "Database", "DATABASE"),
  textInput("port", "Port", "5432"),
  textInput("server", "Server", "SERVER"),
  textInput("un", "Username", "USERNAME"),
  textInput("pw", "Password", "PASSWORD"),
  textInput("query", "Query", "SELECT * FROM iris"),
  textInput("db_tbl", "DB Table", "iris"),
  actionButton("con", "Connect To Database"),
  actionButton("read", "Read From Database"),
  actionButton("trigger_query", "Run Query"),
  actionButton("write_db", "Update Database")
  # downloadButton("download_data", "Download Data"),
  # downloadButton("download_history", "Download History")
)

main_area <- mainPanel(
  h3("Data"),
  textOutput("con_msg"),
  DTOutput("table"),
  h3("Change History"),
  DTOutput("history")
)

layout <- sidebarLayout(sidebar, main_area)
title <- titlePanel("DB RU Technical App")
ui <- fluidPage(title, layout)
app <- shinyApp(ui = ui, server = server)


# library(shiny)
# library(tidyverse)
# library(odbc)
# library(DBI)
# library(DT)
# library(glue)
# 
# server <- function(input, output, session) {
#   
#   values <- reactiveValues()
#   
#   observeEvent(
#     input$con, 
#     {
#       values$db_con <- dbConnect(
#         odbc::odbc(),
#         Driver = Sys.getenv("DRIVER"), 
#         Database = Sys.getenv("DATABASE"), 
#         Port = Sys.getenv("PORT"), 
#         Server = Sys.getenv("SERVER"), 
#         UID = Sys.getenv("USERNAME"), 
#         PWD = Sys.getenv("PASSWORD"),
#         sslmode = "require"
#       )
#       values$connection_msg <- "Database Connection Established"
#       
#       insertUI(
#         selector = '#placeholder',
#         ui = selectInput("tbl", "Choose Table", dbListTables(values$db_con))
#       )
#       
#     }
#     )
#   
#   
#   output$con_msg <- renderText({values$connection_msg})
#   output$history <- renderDT(values$history)
#   
#   observeEvent(
#     input$read,
#     {
#       values$tbl <- dbGetQuery(values$db_con, glue("SELECT * FROM {input$tbl}"))
#       
#       # values$history <- data.frame(
#       #   row = integer(),
#       #   col = integer(),
#       #   original_value = character(), 
#       #   updated_value = character(), 
#       #   row_nums = integer(),
#       #   stringsAsFactors = FALSE
#       #   ) 
#       
#       values$history <- dbGetQuery(values$db_con, glue("SELECT * FROM {input$tbl}_change_log"))
#       } 
#   )
#   
#   output$table <- renderDT(values$tbl, editable = 'cell')
#   
#   observeEvent(
#     input$table_cell_edit, 
#     {
#       
#     info <- input$table_cell_edit
#     original_val <- values$tbl[info$row, info$col]
#     
#     changes <- data.frame(
#       row = info$row,
#       col = info$col,
#       original_value = original_val, 
#       updated_value = info$value, 
#       created = Sys.time() %>% as.POSIXlt("UTC", "%Y-%m-%dT%H:%M:%S") %>% strftime("%Y-%m-%dT%H:%M:%S%z"),
#       stringsAsFactors = FALSE
#       ) 
#     
#     values$history <- changes %>% 
#       mutate(row_nums = 0) %>% 
#       rbind(values$history, .) %>%
#       mutate(row_nums = seq(1, n(), 1))
#     
#     values$tbl <- editData(values$tbl, input$table_cell_edit, 'table')
#     
#   }
#   )
#   
#   observeEvent(
#     input$update,
#     {
#       dbWriteTable(values$db_con, input$tbl, values$tbl, overwrite = TRUE)
#       dbWriteTable(values$db_con, glue("{input$tbl}_change_log"), values$history, append = TRUE)
#     }
#   )
#   
# }
# 
# sidebar <- sidebarPanel(
#   actionButton("con", "Connect To Database"),
#   tags$div(id = 'placeholder'),
#   actionButton("read", "Read From Database"),
#   actionButton("update", "Update Database")
# )
# 
# main_area <- mainPanel(
#   h3("Data"),
#   textOutput("con_msg"),
#   DTOutput("table"),
#   h3("Change History"),
#   DTOutput("history")
#   )
# 
# layout <- sidebarLayout(sidebar, main_area)
# title <- titlePanel("DB RU App")
# ui <- fluidPage(title, layout)
# app <- shinyApp(ui = ui, server = server)
