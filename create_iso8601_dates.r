#' @title ISO-8601 Date Creation
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

create_iso8601_dates <- \(vec){

  vec %>% map(
    ~ .x %>% as.POSIXct(format = "%d-%b-%Y %H:%M") %>% format_ISO8601()
  ) %>% unlist()

}