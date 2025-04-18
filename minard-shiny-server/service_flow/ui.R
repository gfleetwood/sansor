hook = "Don't waste your time listening to customer service scripts. Find the numbers to press to get what you need."

r1 = fluidRow(column(12, h1("Service Flow"), p(hook), br(), align = "center"))

r2 = fluidRow(
  column(
    4, 
    selectInput("company", "Choose Company", companies, selected = default),
    strong("Phone Number"),
    verbatimTextOutput("phone"), 
    strong("Track"),
    verbatimTextOutput("seq"), 
    br(),
    br(),
    align = "center"
  ),
  column(
    5,
    uiOutput('tr'),
    actionButton("reset", "Reset"), 
    actionButton("continue", "Continue"), 
    align = "left"
    ),
  column(
    3,
    includeHTML("adsense.html"),
    align = "left"
  )
)

ui = fluidPage(r1, r2)

r1 = fluidRow(
  column(
    12,
    sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
    plotOutput("distPlot"),
    align = "center"
  )
)

ui = fluidPage(r1)
