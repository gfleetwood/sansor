#' @title Markdown Table to Dataframe
#' @description Converts a markdown table to a dataframe.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

md_tbl_to_df <- \(md_tbl){
  
  md_tbl_cleaned <- md_tbl %>% str_split("\n") %>% pluck(1) %>%
  discard(~ (nchar(.x) == 0) | str_detect(.x, "\\|---"))
  
  header <- md_tbl_cleaned[1]
  body <- md_tbl_cleaned[2:length(md_tbl_cleaned)]
  
  header_cleaned <- header %>% str_split("\\|") %>% pluck(1) %>% discard(~ nchar(.x) == 0) %>% 
    map_chr(~ trimws(.x)) %>% map_chr(~ janitor::make_clean_names(.x))
  
  body_cleaned <- body %>% str_split("\\|") %>% map(~ discard(.x, ~ nchar(.x) == 0)) %>% 
    map(~ map_chr(.x, ~ trimws(.x)))
  
  md_tbl_df <- body_cleaned %>%
   transpose() %>% 
   map(~ unlist(.x)) %>% 
   set_names(header_cleaned) %>% 
   data.frame()
  
}