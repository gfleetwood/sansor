write_tbl_to_db <- function(data, schema, tbl_name){

    tbl <- read_csv(tbl_path)

    dbWriteTable(
        con,
        DBI::Id(schema = schema, table = tbl_name),
        data
    )

    return(TRUE)

}

reddit_code_formatting <- function(code_as_str){

    result <- code_as_str %>%
        str_split("\n") %>%
        map(~ glue("    {.x}"))

    return(result)

}


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

library(usethis)

ls_funcs <- function(pkg){

  result <- glue::glue("package:{pkg}") %>%
    lsf.str() %>%
    toString() %>%
    stringr::str_split(", ") %>%
    data.frame() %>%
    dplyr::rename("functions" = 1)

  return(result)

}
