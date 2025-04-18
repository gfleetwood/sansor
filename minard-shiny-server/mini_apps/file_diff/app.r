library(shiny)
library(tidyverse)
library(diffr)

server <- \(input, output, session) {
  
  # val = reactiveVal(0)
  # 
  # observeEvent(input$diff, {
  #   
  #   diff <- diffr(
  #     file1 = input$files[[1, 'datapath']], 
  #     file2 = input$files[[2, 'datapath']], 
  #     wordWrap = TRUE,
  #     before = "f1", 
  #     after = "f2"
  #   )
  #   
  #   val = val(diff)
  #   
  # })
  # 
  # output$file_diff <- renderDiffr({diff})
  
  output$exdiff <- renderDiffr({
    
    req(input$files)
    
    diffr(
      file1 = input$files[[1, 'datapath']], 
      file2 = input$files[[2, 'datapath']], 
      wordWrap = TRUE,
      before = "f1", 
      after = "f2"
    )
    
  })
  
}

r1 <- fluidRow(
  column(12, p("Assumes both files are in the same location"), align = "center"),
  column(6, fileInput("files", "Choose Files", multiple = TRUE), align = "center"),
  column(6, actionButton("diff", "Get Diff"), align = "center")
)

r2 <- fluidRow(column(12, diffrOutput("exdiff"), align = "center"))

ui <- fluidPage(r1, r2)

shinyApp(ui = ui, server = server)
