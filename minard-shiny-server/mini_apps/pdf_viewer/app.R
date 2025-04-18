# https://stackoverflow.com/questions/47395465/upload-and-view-a-pdf-in-r-shiny

library(shiny)

server <- function(input, output) {
  
  observe({
    
    req(input$file_input)
    file.copy(input$file_input$datapath, "www/input.pdf", overwrite = T)
    
    output$pdfview <- renderUI({
      tags$iframe(style = "height:600px; width:100%", src = "input.pdf")
      
    })
  })
  
}

ui <- fluidPage(
  titlePanel("PDF Viewer"),
  sidebarPanel(fileInput('file_input', 'upload pdf file', accept = c('.pdf'))),
  mainPanel(uiOutput("pdfview"))
  )

shinyApp(ui = ui, server = server)
