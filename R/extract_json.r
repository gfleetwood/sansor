#' @title Extract JSON
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

extract_json <- \(df){

    check_key = \(dict, key) ifelse(key %in% names(fromJSON(dict)), fromJSON(dict)[key], NA)

    df %>%
        mutate(
            col_new = unlist(future_map(col, ~ check_key(.x, "")))
        )

}