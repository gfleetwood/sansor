side_bar <- sidebarPanel(
  selectInput("report_choice", "Choose Report", select(reports, report_name)),
  verbatimTextOutput("auth_output")
)

main_panel <- mainPanel(uiOutput("display_report"))
ui <- fluidPage(titlePanel("Shiny BI"), sidebarLayout(side_bar, main_panel))
ui <- secure_app(ui)