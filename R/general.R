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

#' @title Dynamic Formula Construction
#' @description A simple dynamic formula constructor
#' @param target The LHS (left hand side) of the formula
#' @param additions The columns to add
#' @param substractions The columns to subtract
#' @return A formula
#' @export

make_formula <- function(target, additions, subtractions = NULL){

    vars_sum <- paste0(additions, collapse = ' + ')
    vars_subtract <- ifelse(
        is.null(subtractions), NULL, paste0(subtractions, collapse = ' - ')
        )
    rhs <- ifelse(
        is.null(vars_subtract),
        vars_sum,
        paste(vars_sum, vars_subtract, sep = " + ")
        )

    result <- as.formula(paste(target, rhs, sep = ' ~ '))

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

#' @title Recursive Function Builder
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param func Name/path of the file
#' @param args
#' @return A hierarchical partial function
#' @export

partial_recursive <- function(func, args){

    if(length(args) == 1) return(func(as.formula(args[1])))

    func <- partial_recursive(
        partial(func, as.formula(head(args, 1))),
        tail(args, -1)
    )

    return(func)

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





