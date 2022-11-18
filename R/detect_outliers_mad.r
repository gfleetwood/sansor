#' @title detect_outliers_mad
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

detect_outliers_mad <- \(group, interval = 2){

    med = median(group)
    mad_ = mad(group)
    lower_bnd = med - interval*mad_
    upper_bnd = med + interval*mad_
    results <- between(group, lower_bnd, upper_bnd)

    # The inversion is necessary to have the outliers labeled as 1 instead of 0
    as.integer(1 - results)

}