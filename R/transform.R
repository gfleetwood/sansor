#' Remove Duplicated Columns
#'
#' Generates unique columns for a dataframe.
#' @param df The dataframe to be modified.

remove_col_dups <- function(df){

    result <- df[!duplicated(names(df), fromLast = TRUE)]

    return(result)

}

#' Replace With NA
#'
#' Replace a given value with NA
#' @param df A dataframe A numeric/integer vector
#' @param num_dp The value to replace by NA. The default is "".
#' @return

impute_with_na <- function(df, val = ""){

        df[df == val] <- NA

        return(df)

}

#' Min Max Scaler
#'
#' This function sets up all I need to start a new project.
#' @param vec The vector to scale
#' @param max_ The max of the scale
#' @param min_ THe min of the scale
#' @return The scaled vector

min_max_scaler <- function(vec, max_, min_){

    vec_std = (vec - min(vec, na.rm = T)) / (max(vec, na.rm = T) - min(vec, na.rm = T))
    vec_scaled = vec_std * (max_ - min_) + min_

    return(vec_scaled)

}

#' Get Dummies
#'
#' Dummy a given categorical variable
#' @param df A dataframe
#' @param col Column name to be dummied as string
#' @return The dummified dataframe

get_dummies <- function(df, col){

    dmy <- dummyVars(paste("~", col), data = df)
    dummied_cols <- data.frame(predict(dmy, newdata = df))
    result <- df %>% select_(paste("-", col)) %>% cbind(., dummied_cols)

    return(result)

}

#' Move Column
#'
#' Move a dataframe's column to a given position.
#' @param df A dataframe
#' @param col_name THe name of the column to move
#' @param pos The position to move the column to
#' @result The dataframe with the column in the new position

move_col <- function(df, col_name, pos){

    var <- enquo(col_name)

    result <- df %>%
        add_column(temp = 1, .before = pos) %>%
        mutate(temp = !!var) %>%
        select(-!!var) %>%
        rename(!!var := temp)

    return(result)

}
