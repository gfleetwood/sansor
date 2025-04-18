server <- function(input, output, session) {
  
  r <- reactiveVal("Press: ")
  
  output$phone <- renderText({
      data %>% 
        filter(company == input$company) %>% 
        pull(phone_number) %>% 
        keep(~ !is.na(.x))
    })
  
  
  output$tr = renderUI({
      
      options = data %>% 
        filter(company == input$company) %>% 
        filter(from == input$company) %>% 
        select(to) %>% 
        rename(Choices = 1) %>% 
        distinct() %>% 
        pull(Choices)
      
      radioButtons('reply', 'Choices', options, selected = character(0))
      
    })
    
    output$seq <- renderText({r()})
    
    observeEvent(input$continue, {
      
      if(is.null(input$reply)){
        result = "Nothing"
      } else{
        result = input$reply
      }
      
      if(str_detect(result, ":")){
        
        r_new = result %>% 
          str_split("Press ") %>% 
          unlist() %>% 
          pluck(2) %>%
          str_split(":") %>% 
          unlist() %>% 
          pluck(1) %>% 
          paste0(r(), ., " ")
        
        r(r_new)
        
        options = data %>% 
          filter(company == input$company) %>% 
          filter(from == result) %>% 
          select(to) %>% 
          rename(Choices = 1) %>% 
          distinct() %>% 
          pull(Choices)
        
        updateRadioButtons(session, "reply", choices = options, selected = character(0))
        
      }
    
      
    })
    
    observeEvent(input$reset, {
      
      options = data %>% 
        filter(company == input$company) %>% 
        filter(from == input$company) %>% 
        select(to) %>% 
        rename(Choices = 1) %>% 
        distinct() %>% 
        pull(Choices)
      
      updateRadioButtons(session, "reply", choices = options, selected = character(0))
      r("Press: ")
      
      
    })
    

})

server <- function(input, output, session) {
  
  values = reactiveValues(df = NULL)
  active = reactiveVal(1)
  
  output$distPlot <- renderPlot({
    
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(
      x, 
      breaks = bins, 
      col = 'darkgray', 
      border = 'white',
      xlab = 'Waiting time to next eruption (in mins)',
      main = 'Histogram of waiting times'
      )
    
  })
  
  
  # observeEvent(input$search, {
  #   
  #   # values$df
  #   
  #   values$df = input$sr %>%
  #     read_sr_vids() %>% 
  #     do.call(rbind, .) %>% 
  #     data.frame()
  #   
  # })
  
}
