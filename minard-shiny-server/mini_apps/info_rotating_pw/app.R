library(shiny)
library(tidyverse)
library(reticulate)

use_python("/usr/bin/python3")
source_python("server.py")

server <- function(input, output, session) {
  
  pw <- reactivePoll(
    30000, 
    session,
    checkFunc = function() {
      sample(1:1000000, 1)
    },
    valueFunc = function() {
      # system("cat /usr/share/dict/words", intern = TRUE) %>% 
      #   discard(~ nchar(.x) != 4) %>% 
      #   discard(~ str_detect(.x, "'")) %>% 
      #   tolower() %>% 
        c("boll", "whit", "oils", "lens", "toad", "feed", "bees", "side", "akin", "fern") %>% 
        sample(size = 4) %>% 
        paste(collapse = " ")
    }
  )
  
  observeEvent(input$request, {
    send_pw(pw())
    })
  
  observeEvent(input$submit, { 
    
    updateTextInput(session, "read_pw", "Enter Current Password","")
    
    pw_submitted <- input$read_pw
    
    if(pw_submitted == ""){
      
      output$msg <- renderUI(p("No password entered"))
      
    } else if(pw_submitted == pw()){
      
      output$msg <- renderUI(
        tagList(
          p("Name: John Doe"),
          p("Address: 123-34 ABC Drive, Timbuktu, Tim 00010"),
          p("Phone: 123-456-6874")
        )
      )
      
    } else{
      output$msg <- renderUI(p("Wrong password"))
    }

  })
  
  #output$pq <- renderText(pw())
  
}

ui <- fluidPage(
  fluidRow(column(12, h3("Info")), align = "center"),
  br(),
  fluidRow(
    column(12, textInput("read_pw", "Enter Current Password")),
    align = "center"
  ),
  fluidRow(
    column(2, actionButton("request", "Request Current Password"), offset = 4),
    column(2, actionButton("submit", "Submit Current Password"), offset = -4),
    align = 'center'
  ),
  fluidRow(
    #column(12, verbatimTextOutput("pq")),
    br(),
    uiOutput("msg"),
    align = "center"
    )
)

shinyApp(ui = ui, server = server)

