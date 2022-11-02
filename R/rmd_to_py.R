#' @title Convert RMD To Python Script
#' @description Converts an Rmd with comments and Python chunks to a Python script
#' @return TRUE
#' @export
rmd_to_py <- \(input_file_path, output_file_path){
  
  # Converts an rmd file with python chunks to a python script.
  
  py_file <- read_file(input_file_path)
  
  a <- py_file %>% 
    # remove yaml header
    str_remove_all("^---\n(.*\n)*---\n") %>%
    str_split("\n") %>% 
    unlist() %>% 
    # each row of the df is a remaining line from the file
    data.frame(x = .) %>%
    filter(x != "") %>% 
    # 1 is the start of an r chunk
    # 2 is the start of a python chunk
    # 3 is the end of any chunk
    mutate(
      identify_chunk_markers = case_when(
        str_detect(x, "```\\{r") ~ 1, 
        str_detect(x, "```\\{py") ~ 2, 
        x == "```" ~ 3,
        TRUE ~ 0
      )
    )
  
  # With the identify_chunk_markers column an r chunk is marked by
  # 1 followed by any number of 0s then 3.
  # So the simplest r chunk is 13, the next is 103, then 1003 and so on
  # That simplifies the regex to find r chunks
  
  r_chunk_rows <- pull(a, identify_chunk_markers) %>% 
    paste(collapse = "") %>% 
    str_locate_all("10*3") %>% 
    as.data.frame() %>% 
    # str_locate_all generates the start and end of each chunk
    # This generates all sequences start:end and then
    # unlist creates a vector with all the rows that are r chunks
    mutate(e = map2(start, end, ~ .x:.y)) %>% 
    pull(e) %>% 
    unlist()
  
  # remove r chunk rows from df
  a <- slice(a, -r_chunk_rows)
  
  # Similarly for python chunks the simplest is 23, then 203, then 2003
  py_chunk_rows = pull(a, identify_chunk_markers) %>% 
    paste(collapse = "") %>% 
    str_locate_all("20*3") %>%
    as.data.frame() %>% 
    mutate(e = map2(start, end, ~ .x:.y)) %>% 
    pull(e) %>% 
    unlist()
  
  a %>% 
    mutate(row_num = 1:n()) %>% 
    mutate(py_chunks = row_num %in% py_chunk_rows) %>% 
    # Prepends # to any comments,i.e non python chunk rows
    mutate(x = case_when(py_chunks == FALSE ~ paste("#", x), TRUE ~ x)) %>% 
    pull(x) %>% 
    paste(collapse = "\n") %>% 
    # Remove the last of the chunk markers
    str_remove_all("```\\{python\\}") %>% 
    str_remove_all("```") %>% 
    write_file(output_file_path)
  
  TRUE
  
}