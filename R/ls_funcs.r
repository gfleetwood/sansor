#' @title List Package Functions
#' @description Lists a package's functions as a dataframe.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

ls_funcs <- \(pkg){

  glue::glue("package:{pkg}") %>%
    lsf.str() %>%
    toString() %>%
    stringr::str_split(", ") %>%
    data.frame() %>%
    dplyr::rename("functions" = 1)

}