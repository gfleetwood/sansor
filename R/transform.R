#' @title Remove Duplicated Columns
#' @description Generates unique columns for a dataframe.
#' @param df The dataframe to be modified.
#' @return The data with duplicated columns removed
#' @export

remove_duplicate_cols <- function(df){

    result <- df[!duplicated(names(df), fromLast = TRUE)]

    return(result)

}

#' @title Min Max Scaler
#' @description This function sets up all I need to start a new project.
#' @param vec The vector to scale
#' @param max_ The max of the scale
#' @param min_ THe min of the scale
#' @return The scaled vector
#' @export

min_max_scaler <- function(vec, max_, min_){

    vec_std = (vec - min(vec, na.rm = T)) / (max(vec, na.rm = T) - min(vec, na.rm = T))
    vec_scaled = vec_std * (max_ - min_) + min_

    return(vec_scaled)

}

#' @title Get Dummies
#' @description Dummy a given categorical variable
#' @param df A dataframe
#' @param col Column name to be dummied as string
#' @return The dummified dataframe
#' @export

get_dummies <- function(df, col){

    dmy <- dummyVars(paste("~", col), data = df)
    dummied_cols <- data.frame(predict(dmy, newdata = df))
    result <- df %>%
        select_(paste("-", col)) %>%
        cbind(., dummied_cols)

    return(result)

}

#' @title Unpack List Column As String
#' @description Converts a list column to a string column
#' @param df A dataframe
#' @param col The name of the list column
#' @return A dataframe with NAs instead of #NULLs and empty strings.
#' @export

list_col_to_str <- function(df, col){

    result <- mutate(
        df,
        !!sym(col) := map_chr(!!sym(col), ~ paste(.x, collapse = "-"))
    )

    return(result)

}

#' @title Identify Columns With NAs
#' @description Takes a dataframe and returns the columns which meet a threshold of NAs
#' @param df A dataframe
#' @return
#' @export

cols_missing_data <- function(df, frac){

    df_smry <- skimr::skim_to_wide(df)

    results <- df_smry %>%
        mutate(missing_ratio = as.integer(missing)/as.integer(n)) %>%
        filter(missing_ratio >= frac) %>%
        pull(variable)

    return(results)

}

#' @title Imputation Indicator Columns
#' @description For a given dataframe this functions adds a Boolean column for each column
#' with NAs that indicates the location of these NAs.
#' @param df A dataframe
#' @return A dataframe with indicator columns for columns with at least one NA
#' @export

update_label_nas <- function(df){

    update_label_na <- function(df, col_with_na){

        col_new <- paste(col_with_na, "is_na", sep = "_")

        result <- mutate(
            df,
            {{col_new}} := ifelse(is.na((!!sym(col_with_na))), 1, 0)
        )

        return(result)

    }

    cols_with_na <- df %>%
        keep(~ sum(is.na(.)) > 0) %>%
        names()

    result <- reduce(cols_with_na, ~ update_label_na(.x, .y), .init = df)

    return(result)

}

#' @title Drop NA Columns
#' @description Drops columns with NAs according to a specified criterion.
#' @param df A dataframe
#' @param method The drop criterion:
#' * all: Drop columns with all NAs
#' * frac: Drop columns with NAs above a threshold
#' * any: Drop any columns with at least one NA
#' @param frac The threshold (between 0 and 1 inclusive) to be used when method="frac".
#' @return A dataframe with some columns dropped according to the supplied specification
#' @export

drop_na_col <- function(df, method = "all", frac = 0.1){

    result <- switch(
        method,
        "all" = discard(df, ~ all(is.na(.x))),
        "frac" = discard(df, ~ mean(is.na(.x)) > frac),
        "any" = discard(df, ~ sum(is.na(.x)) > 0)
    )

    return(result)

}

#' @title Drop Anomalous Columns
#' @description Uses the xray library to identify anomalous columns and drops them.
#' @param df A dataframe to be examined
#' @return The df variable without the anomalous columns
#' @export

drop_anomalous_cols <- function(df){

    anomalous_cols <- df %>%
        xray::anomalies() %>%
        purrr::pluck("problem_variables", "Variable")

    result <- select(df, -one_of(anomalous_cols))

    return(result)

}


