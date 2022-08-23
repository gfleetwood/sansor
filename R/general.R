#' @title Mode
#' @description Calculates the statistical mode
#' @param vec A numeric/integer vector
#' @return The mode of the submitted vector
#' @export

read_mode = function(vec){

    vec %>% 
        tabyl() %>%
        arrange(desc(n)) %>%
        pull(1) %>%
        pluck(1)

}

#' @title Cohen's H
#' @description Calculates Cohen's H for two probabilities or proportions.
#' @param p1 The first probability or proportion
#' @param p2 The second probability or proportion
#' @return
#' @export

cohens_h = function(p1, p2) abs(2*(asin(sqrt(p1)) - asin(sqrt(p2))))

#' @title Outliers In Small Samples
#' @description A function to find outliers in small samples. Method seen here: https://bit.ly/2DFmsJr
#' @param vec A numeric/integer vector
#' @return A boolean vector of which values are and are not outliers
#' @export

outliers_iglewicz_hoaglin = function(vec){

    mi <- .6745*(vec - mean(vec))/mad(vec)
    vec[abs(mi) > 3.5]

}

#' @title Extract JSON
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

extract_json = function(df){

    check_key = function(dict, key) ifelse(key %in% names(fromJSON(dict)), fromJSON(dict)[key], NA)

    result <- df %>%
        mutate(
            col_new = unlist(future_map(col, ~ check_key(.x, "")))
        )

}

#' @title detect_outliers_mad
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

detect_outliers_mad = function(group, interval = 2){

    med = median(group)
    mad_ = mad(group)
    lower_bnd = med - interval*mad_
    upper_bnd = med + interval*mad_
    results <- between(group, lower_bnd, upper_bnd)

    # The inversion is necessary to have the outliers labeled at 1 instead of 0
    results_inverted <- as.integer(1 - results)

}

#' @title Markdown Table to Dataframe
#' @description Converts a markdown table to a dataframe.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

md_tbl_to_df = function(md_tbl){
  
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

#' @title List Package Functions
#' @description Lists a package's functions as a dataframe.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

ls_funcs = function(pkg){

  glue::glue("package:{pkg}") %>%
    lsf.str() %>%
    toString() %>%
    stringr::str_split(", ") %>%
    data.frame() %>%
    dplyr::rename("functions" = 1)

}

#' @title ISO-8601 Date Creation
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

create_iso8601_dates = function(vec){

  vec %>% map(
    ~ .x %>% as.POSIXct(format = "%d-%b-%Y %H:%M") %>% format_ISO8601()
  ) %>% unlist()

}

#' @title Change Range
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

change_range = function(x, new_min, new_max){

  zero_one_range = (x - min(x))/(max(x) - min(x))
  new_range = (new_max - new_min)*zero_one_range + new_min

}

#' @title Read Package Dependency Trees
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

read_pkg_dependency_tree = function(
  pack,
  dep_level = c("Depends", "Imports", "LinkingTo"), 
  available_packages = available.packages()
) {
  
  # source; https://gist.github.com/johnrc/faaa796e4b1ac53b7848
  # ex: read_pkg_dependency_tree("dplyr")
  
  packages = pack %>% 
    package_dependencies(available_packages, which = dep_level) %>% 
    unlist() %>% 
    unname()
  
  for(pkg in packages) {
    packages = c(packages, read_pkg_dependency_tree(pkg, dep_level, available_packages))
  }
  
  packages
  
}

#' @title Intuitive List Pkgs
#' @description Checks if two dataframes are equal.
#' @return Lists all loaded packages
#' @export

read_loaded_pkgs = \(x) { search() }

create_datetime = \(year = 0, month = 0, day = 0, hours = 0, minutes = 0, seconds = 0){
  
  datetime = list(
    year = year,
    month = month,
    day = day,
    hours = hours,
    minutes = minutes,
    seconds = seconds
  )
  
}

#' @title Requirements Script
#' @description 
#' @return Lists all loaded packages
#' @export
create_requirements_script <- \(){
  
  installed.packages() %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    select(Package, Version) %>% 
    mutate(
      install_cmd = glue('devtools::install_version("{Package}", version = "{Version}", dependencies = TRUE)')
    ) %>% 
    pull(install_cmd) %>% 
    readr::write_lines("./requirements.r")
  
}

pyrmd_to_py <- \(input_file_path, output_file_path){
  
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

create_sql_table_query <- \(con, df,  tbl_name){
  
  #con <- dbConnect(RSQLite::SQLite(), ":memory:")
  query <- sqlCreateTable(con, tbl_name, df, row.names = F)
  toString(query) %>% str_replace_all("`","")
  
}

create_sql_value_representation <- \(row){

  row_sql_temp <- unname(row) %>%
    map(as.character) %>%
    paste(collapse = ", ")

  glue::glue("({row_sql_temp})")

}

#g(iris)
create_sql_insert_query <- \(df){

  template = "
  INSERT INTO table_name
  VALUES
  "

  map_chr(1:nrow(df), ~ create_sql_value_representation(df[.x, ])) %>%
    paste(collapse = ",\n ") %>%
    paste(template, ., ";") %>%
    cat()

}
