shinyServer(function(input, output, session) {
  
  values = reactiveValues(
    feed = sample(1:100, 10),
    saved = c(),
    removed = c()
    )
  
  observeEvent(input$save, {
    values$saved = c(values$saved, values$feed[1])
    values$feed = values$feed[-1]
    
  })
  
  observeEvent(input$remove, {
    values$removed = c(values$removed, values$feed[1])
    values$feed = values$feed[-1]
  })
  
  
  output$live_feed = renderUI({
    
    items = values$feed
    item_list = map(items, ~ wellPanel(p(as.character(.x))))
    tagList(item_list)
    
  })
  
  output$saved_tab = renderUI({
    
    items = values$saved
    item_list = map(items, ~ wellPanel(p(as.character(.x))))
    tagList(item_list)
    
  })
  
  output$removed_tab = renderUI({
    
    items = values$removed
    item_list = map(items, ~ wellPanel(p(as.character(.x))))
    tagList(item_list)
    
  })

})
