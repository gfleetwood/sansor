live = tabPanel(
  "live", 
  actionButton("save", "✔"),
  actionButton("remove", "x"),
  br(),
  br(),
  uiOutput("live_feed")
)

saved = tabPanel("saved", uiOutput("saved_tab"))
removed = tabPanel("removed", uiOutput("removed_tab"))

r1 = fluidRow(
  column(
    12,
    h3("unifeed"),
    p("one stop for all your updates"),
    tabsetPanel(type = "tabs", br(), live, saved, removed),
    align = "center"
    )
)

ui = fluidPage(theme = shinytheme("darkly"), r1)
