#' @title Remove Duplicated Columns
#' @description Generates unique columns for a dataframe.
#' @param df The dataframe to be modified.
#' @return The data with duplicated columns removed
#' @export

remove_duplicate_cols <- \(df) {

    df[!duplicated(names(df), fromLast = TRUE)]

} 
