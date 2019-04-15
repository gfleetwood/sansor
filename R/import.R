#' Read CSVs Function
#'
#' This function takes a path and reads all the csvs in it into a single dataframe.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

read_csvs <- function(path){
    dfs <- dir(path, pattern ='\\.csv', full.names = T) %>%
        map_df(read.csv, sep =',', header = T, stringAsFactors = F)
    return(dfs)
}

#' Read CSVs Function
#'
#' This function takes a path and reads all the csvs in it into a single dataframe.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

read_csv_sample <- function(fpath, nrows, seed = 8, header = "-r"){

    sample <- system(glue("subsample {header} -s {seed} -n {nrows} {fpath}"), intern = T)

    # Convert the character vector into a string
    sample_cleaned <- paste(sample, collapse = '\n')

    df <- read_csv(sample_cleaned)

    return(df)

}
