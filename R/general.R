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

outliers_ih <- function(vec){

    mi <- .6745*(vec - mean(vec))/mad(vec)
    result <- vec[abs(mi) > 3.5]

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

ls_funcs = function(pkg){

  glue::glue("package:{pkg}") %>%
    lsf.str() %>%
    toString() %>%
    stringr::str_split(", ") %>%
    data.frame() %>%
    dplyr::rename("functions" = 1)

}

create_iso8601_dates = function(vec){

  vec %>% map(
    ~ .x %>% as.POSIXct(format = "%d-%b-%Y %H:%M") %>% format_ISO8601()
  ) %>% unlist()

}

change_range = function(x, new_min, new_max){

  zero_one_range = (x - min(x))/(max(x) - min(x))
  new_range = (new_max - new_min)*zero_one_range + new_min

}

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

