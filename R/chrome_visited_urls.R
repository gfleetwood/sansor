library(tidyverse)
library(RSQLite)
library(DBI)
library(odbc)

visited_urls_chrome <- \(x){
  
  system("cp $HOME/.config/google-chrome/Default/History ~")
  
  con <- dbConnect(RSQLite::SQLite(), "$HOME/History")
  df <- dbGetQuery(con, "select * from urls")
  
  system("rm $HOME/History")
  
  arrange(df, desc(last_visit_time))
  
}

