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

read_timestamp_as_str_iso8601 <- \(x){
  
  as.POSIXlt(Sys.time(), "UTC") %>% 
    strftime("%Y-%m-%dT%H:%M:%S%z")
  
}

read_timestamp_as_date_iso8601 <- \(x){
  
  as.POSIXlt(Sys.time(), "UTC") %>% 
    strptime("%Y-%m-%dT%H:%M:%S%z")
  
}

create_venn_df <- \(
  vector_a, 
  vector_b, 
  a_label = "first_only",
  b_label = "second_only"
){
  
  a_cap_b_label = paste(
    str_remove(a_label, "_only"), 
    str_remove(b_label, "_only"),
    sep = "_intersect_"
  )
  
  intersection <- intersect(vector_a, vector_b)
  a_only <- setdiff(vector_a, vector_b)
  b_only <- setdiff(vector_b, vector_a)
  data <- c(intersection, a_only, b_only)
  
  label <- c(
    rep(a_cap_b_label, length(intersection)),
    rep(a_label, length(a_only)),
    rep(b_label, length(b_only))
  )
  
  result <- data.frame(data, label)
  
}

create_df_transpose <- \(df){
  
  df %>% 
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(cols = everything()) 
  
}


nested_json_to_df <- \(json_data_as_list){
  
  fromJSON(json_data_as_list, flatten = T) %>% 
    create_df_transpose()
  
}
