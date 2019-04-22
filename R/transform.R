#' Remove Duplicated Columns
#'
#' Generates unique columns for a dataframe.
#'
#' @param df The dataframe to be modified.

remove_col_dups <- function(df){
    return(df[!duplicated(names(df), fromLast = TRUE)])
}


#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

standard_scaler_vec <- function(x){
    return((x - mean(x, na.rm = T))/sd(x, na.rm = T))
}

#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

standard_scaler_df <- function(x){
    return((x - mean(x, na.rm = T))/sd(x, na.rm = T))
}



#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

min_max_scaler_vec <- function(x, max_, min_){
    x_std = (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
    x_scaled = x_std * (max_ - min_) + min_
    return(X_scaled)
}

#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

min_max_scaler_df <- function(df, max_, min_){

    df_other <- df %>%
        select_if(
            function(y) !is.numeric(y) & !is.integer(y)
        )

    df_num <- df %>%
        select_if(
            function(y) is.numeric(y) | is.integer(y)
        )

    result <- map_df(df_num, function (x) min_max_scaler_vec(x, max_, min_))
    return(cbind(df_other, result))
}

#' Rounded Mean Function
#'
#' This function return the mean of vector to n decimal places while ignoring missing values.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords
#' @export
#' @examples
#' mean_dp()

get_dummies <- function(df, col){

    dmy <- dummyVars(paste("~", col), data = df)
    dummied_cols <- data.frame(predict(dmy, newdata = df))
    df2 <- df %>% select_(paste("-", col)) %>% cbind(., dummied_cols)

    return(df2)
}

#' Rounded Mean Function
#'
#' This function return the mean of vector to n decimal places while ignoring missing values.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords
#' @export
#' @examples
#' mean_dp()
#'
move_col <- function(df, col_name, pos){

    var <- enquo(col_name)

    df <- df %>%
        add_column(temp = 1, .before = pos) %>%
        mutate(temp = !!var) %>%
        select(-!!var) %>%
        rename(!!var := temp)

    return(df)

}
