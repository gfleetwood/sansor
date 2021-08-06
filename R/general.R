#' @title Seed Sampling
#' @description Produces a reproducible sample from a dataframe with
#' @param df A dataframe
#' @param frac The fraction
#' @param seed The seed for the sampling
#' @return The sample of the data
#' @export

ss_sampleseed <- function(df, frac, seed = 8){

    set.seed(seed)
    result <- df[sample.int(nrow(df), frac*nrow(df)), ]

    return(result)

}

#' @title Mode
#' @description Calculates the statistical mode
#' @param vec A numeric/integer vector
#' @return The mode of the submitted vector
#' @export

mode_stats <- function(vec){

    result <- tabyl(vec) %>%
        arrange(desc(n)) %>%
        pull(1) %>%
        pluck(1)

    return(result)

}

#' @title Cohen's H
#' @description Calculates Cohen's H for two probabilities or proportions.
#' @param p1 The first probability or proportion
#' @param p2 The second probability or proportion
#' @return
#' @export

cohens_h <- function(p1, p2){

    results <- abs(2*(asin(sqrt(p1)) - asin(sqrt(p2))))

    return(result)

}

#' @title Outliers In Small Samples
#' @description A function to find outliers in small samples. Method seen here: https://bit.ly/2DFmsJr
#' @param vec A numeric/integer vector
#' @return A boolean vector of which values are and are not outliers
#' @export

outliers_ih <- function(vec){

    mi <- .6745*(vec - mean(vec))/mad(vec)
    result <- vec[abs(mi) > 3.5]

    return(result)

}

#' @title Write Text File
#' @description Helper function to write an object to txt
#' @param obj The object to write
#' @param fname The name of the file to write to
#' @export

write_txt <- function(obj, fname){

    con <- file(fname)
    writeLines(c(obj),  con)
    close(con)

}
#' @title Dataframe Equality
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

dfs_equal <- function(df1, df2){

    # Removes NAs so the equality check isn't thrown off

    vec_compare <- function(df, col_loc) {

        result <- df %>%
            pull(col_loc) %>%
            keep(~ !is.na(.x))

        return(result)

    }

    # Check the base case where the dataframes should have the same shape
    if(all(dim(df1) !=  dim(df2))) return(0)

    n_cols <- seq(1, ncol(df1))

    result <- n_cols %>%
        map_lgl(
            ~ all(vec_compare(df1, .x) == vec_compare(df2, .x))
        ) %>%
        all()

    return(result)

}

#' @title Check Key
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

check_key <- function(dict, key){

    result <- ifelse(key %in% names(fromJSON(dict)), fromJSON(dict)[key], NA)

    return(result)

}

#' @title Extract JSON
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

extract_json <- function(df){

    result <- df %>%
        mutate(
            col_new = unlist(future_map(col, ~ check_key(.x, "")))
        )

    return(result)

}

#' @title detect_outliers_mad
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

detect_outliers_mad <- function(group, interval = 2){

    med = median(group)
    mad_ = mad(group)
    lower_bnd = med - interval*mad_
    upper_bnd = med + interval*mad_
    results <- between(group, lower_bnd, upper_bnd)

    # The inversion is necessary to have the outliers labeled at 1 instead of 0
    results_inverted <- as.integer(1 - results)

    return(results_inverted)

}
