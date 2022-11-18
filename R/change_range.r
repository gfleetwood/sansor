#' @title Change Range
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

change_range <- \(x, new_min, new_max){

  zero_one_range = (x - min(x))/(max(x) - min(x))
  new_range = (new_max - new_min)*zero_one_range + new_min

}