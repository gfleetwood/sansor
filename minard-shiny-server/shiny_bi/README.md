# Shiny BI

A prototype of a Business Intelligence tool built in Shiny. It has;

* Simple authentication through shinymanager
* User groups
* Permissions

The test_schema.R file has a structure for all the tables you would need to read from a database. There's a sample function in server.R for custom auth (taken from the shinymanager website).

Describe your UI for reports in reports_ui.R and the logic in the server function in server.R. You can see examples `output$report_one` and `output$report_two`.
