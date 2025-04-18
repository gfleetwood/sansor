library(shiny)
library(magick)
library(cowplot)
library(glue)

server <- shinyServer(function(input, output, session) {
  
  values = reactiveValues(imgs = NULL)
  active = reactiveVal(1)
  
  observeEvent(input$search, {      
    values$imgs = list.files(input$path, pattern = ".jpg", full.names = TRUE)
  })
  
  
  output$tracker = renderText({
      
      index = active()
      total = length(values$imgs)
      glue("{index}/{total}")
      
  }) 
  
  output$title = renderText({ 
    values$imgs[active()]
  }) 
  
  output$frame = renderPlot({

      im = image_read(values$imgs[active()])
      ggdraw() + draw_image(im)
      
  })
  
  observeEvent(input$left, {
      
      new_val = ifelse(active() - 1 < 1, length(values$imgs), active() - 1)
      active(new_val)
      
  }) 
  
  observeEvent(input$right, {
      
      new_val = ifelse(active() + 1 > length(values$imgs), 1, active() + 1)
      active(new_val)
      
  }) 
  
})

r1 = fluidRow(
  column(
    12, 
    h1("image slideshow"), 
    textInput("path", "enter images directory", ""), 
    actionButton("search", "Search"),
    align = "center"
    )
)

r2 = fluidRow(
  column(2, actionButton("left", "<|"), br(), align = "right"),
  column(8, verbatimTextOutput("tracker"), verbatimTextOutput("title"), plotOutput("frame"), align = "center"),
  column(2, actionButton("right", "|>"), align = "left")
)

ui = fluidPage(r1, r2)

shinyApp(ui = ui, server = server)

