users <- data.frame(
  user_id = c("1", "2"),
  user = c("shiny", "shinymanager"),
  password = c("12", "123"),
  admin = c(FALSE, TRUE),
  stringsAsFactors = F
)

user_groups <- data.frame(
  user_group_id = c("3", "4"),
  user_group_name = c("a", "b"),
  user_id = c("1", "2"),
  stringsAsFactors = F
)

reports <- data.frame(
  report_id = c("5", "6"),
  report_name = c("r1", "r2"),
  stringsAsFactors = F
)

user_groups_reports_access <- data.frame(
  user_group_id = c("3", "4", "4"),
  report_id = c("5", "5", "6"),
  stringsAsFactors = F
)