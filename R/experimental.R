library(rdrop2)
library(tidyverse)
library(glue)

md_tbl_to_df <- function(md_tbl){
  
  md_tbl_cleaned <- md_tbl %>% str_split("\n") %>% pluck(1) %>%
  discard(~ (nchar(.x) == 0) | str_detect(.x, "\\|---"))
  
  header <- md_tbl_cleaned[1]
  body <- md_tbl_cleaned[2:length(md_tbl_cleaned)]
  
  header_cleaned <- header %>% str_split("\\|") %>% pluck(1) %>% discard(~ nchar(.x) == 0) %>% 
    map_chr(~ trimws(.x)) %>% map_chr(~ janitor::make_clean_names(.x))
  
  body_cleaned <- body %>% str_split("\\|") %>% map(~ discard(.x, ~ nchar(.x) == 0)) %>% 
    map(~ map_chr(.x, ~ trimws(.x)))
  
  md_tbl_df <- body_cleaned %>% transpose() %>% map(~ unlist(.x)) %>% 
    set_names(header_cleaned) %>% data.frame()

  return(md_tbl_df)
  
}

ls_funcs <- function(pkg){

  result <- glue::glue("package:{pkg}") %>%
    lsf.str() %>%
    toString() %>%
    stringr::str_split(", ") %>%
    data.frame() %>%
    dplyr::rename("functions" = 1)

  return(result)

}

rename_file <- function(index, file_path){

  target_file_path <- str_replace(file_path, "", "")
  
  print(index)
  print(file_path)
  
  drop_move(file_path, target_file_path)
  
  return(TRUE)
  
}

rename_mv_file <- function(index, file_path, target_file_path){

  # target_file_path <- glue("{target_dir}/{basename(file_path)}")
  # target_file_path <- str_replace(file_path, "slot7", "slot4")
  
  print(index)
  print(file_path)
  print(target_file_path)
  
  drop_move(file_path, target_file_path)
  
  return(TRUE)
  
}

#source_dir <- ""
#target_dir <- ""

#files <- drop_dir(target_dir)

#foo <- files %>%
#  pull(path_display) %>%
#  map2(1:length(.), ., rename_file)
  
