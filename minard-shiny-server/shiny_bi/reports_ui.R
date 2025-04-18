r1 <- tagList(
  sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
  plotOutput("report_one")
)

r2 <- tagList(
  sliderInput("bins", "Number of bins:", min = 1, max = 10, value = 5),
  plotOutput("report_two")
)