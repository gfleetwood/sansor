#' @title Counts Rows Of On Disk File
#' @description A command line version of nrow
#' @param f1 Name/path of the file
#' @export

bash_nrows <- function(f1){
    system(glue("wc -l {f1}"))
}

#' @title Column Counter
#' @description Returns the number of columns in a file
#' @param f1 Name/path of the file.
#' @param sep File separator. Defaults to csv.
#' @return The number of columns
#' @export

bash_ncols <- function(f1, sep = ','){
    return(unique(count.fields(f1, sep = sep)))
}

#' @title Show Head Of File
#' @description Show the top n rows of a file.
#' @param f1 The name/path of the file.
#' @param nrows The number of rows to show.
#' @export

bash_head <- function(f1, nrows){
    system(glue("head -n {nrows} {f1}"))
}

#' @title Column Sum
#' @description Finds the sum of a given column.
#' @param f1 The name/path of the file.
#' @param col The name of the column to find the sum of.
#' @export

bash_col_sum <- function(f1, col){
    col_num <-  which(names(f1) == col)
    system(glue("awk -F"," '{x+=${col_num}}END{print x}' {f1}"))
}

#' @title  Mean
#' @description Finds the mean of a given column.
#' @param f1 The name/path of the file.
#' @param col The name of the column to find the mean of.
#' @export

bash_col_mean <- function(f1, col){
    col_num <-  which(names(f1) == col)
    system(glue("awk '{ sum += ${col_num} } END { if (NR > 0) print sum / NR }' {f1}"))
}

#' @title Column Sort
#' @description Sorting by a column
#' @param f1 The name/path of the file.
#' @param col_sort The name of the column to find the mean of.
#' @export

bash_arrange <- function(f1, col_sort){

    col_num <-  which(names(f1) == col_sort)
    system(glue("sort -t, -k{col_num} {f1}"))

}

#' @title Select Column Bash
#' @description Selects a column from a file on disk
#' @param f1 The name/path of the file.
#' @param cols_to_remove Names of the columns to removes.
#' @param f_new Optional name for new file if the results should be saved.
#' @export

bash_select <- function(f1, cols_to_remove, f2 = NA){

    # which means "select columns 2 through 5 and columns 8, using comma as the separator".
    #cut -f 2-5,8 -d , values.csv

    #Remove columns

    col_nums <-  which(names(f1) == cols_to_remove)
    cols_to_remove_nums <- paste0(col_nums, collapse = ',')


    if(is.na(f2)){
        system(glue("cut -d, -f {cols_to_remove_nums} {f1}"))
    } else{
        system(glue("cut -d, -f {cols_to_remove_nums} {f1} > {f2}"))
    }

}

#' @title Bash Join Function
#' @description Join two files stored on disk.
#' @param f1 The name/path of the first file.
#' @param col1 Column to use for joining from the first file.
#' @param f2 The name/path of the second file.
#' @param col2 Column to use for joining from the second file.
#' @param f_new Nme for new file.
#' @export

bash_join <- function(f1, col1, f2, col2, f_new){

    # Join the first file (-1) by the second column and the second file (-2) by the first

    col_num1 <-  which(names(f1) == col1)
    col_num2 <-  which(names(f2) == col2)

    system(glue("join -t, -1 2 -2 1 {f1} {f2} > {f_new}"))
}
