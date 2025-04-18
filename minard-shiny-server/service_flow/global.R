library(tidyverse)
library(shiny)
library(stringi)
library(odbc)
library(DBI)
library(pool)
library(DT)
library(glue)
library(reticulate)

use_python("/usr/bin/python3")
#source_python("server.py")

random_account_num_generator <- function(input, output) {
  
  account_num <- sample(c(letters, 0:9), 17, replace = T) %>% 
    paste(collapse = "")
  
}

con = dbPool(
  odbc(),
  Driver = "PostgreSQL ANSI", 
  Database = Sys.getenv("SF_DB"), 
  Port = "5432", 
  Server = Sys.getenv("SF_SERVER"), 
  UID = Sys.getenv("SF_UN"), 
  PWD = Sys.getenv("SF_PW"), 
  sslmode = "require"
)

data = con %>% 
  tbl("companies") %>% 
  select(from, to, company, phone_number) %>% 
  collect()

companies = data %>% 
  select(company) %>% 
  unique() %>% 
  map_chr(str_to_title) %>% 
  set_names() %>% 
  map(tolower)

default = companies[1]

