#' @title Read CSVs
#' @description This function takes a path and reads all the csvs in it into a single dataframe.
#' @param path The path to the csvs to read in
#' @return All the data in a single dataframe
#' @export

read_csvs <- function(path){

    dfs <- dir(path, pattern ='\\.csv', full.names = T) %>%
        map_df(read.csv, sep =',', header = T, stringAsFactors = F)

    return(dfs)

}

#' @title Read CSV Sample
#' @description This function takes a path and reads all the csvs in it into a single dataframe.
#' @param fpath The pth and name of the file
#' @param nrows The number of rows to sample
#' @param seed The seed to use for the random selection
#' @param header An indicator of whether the file has a header row. The default is yes ("=r"). Use "" for no.
#' @return df A sample of the file specified
#' @export

read_csv_sample <- function(fname, nrows, seed = 8, header = "-r"){

    sample <- system(glue("subsample {header} -s {seed} -n {nrows} {fname}"), intern = T)
    # Convert the character vector into a string
    sample_cleaned <- paste(sample, collapse = '\n')
    result <- read_csv(sample_cleaned)

    return(result)

}
