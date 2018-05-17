#' A Function
#'
#' A print out of some base R functions which replicate command line operations.
#' @export
#' @examples
#' bash_baseR()

bash_baseR <- function(){

  print('
A list of useful base R bash functions:

* dir
* list.files
* dir.exists
* dir.create
* untar
* file.info
* file.append: Like rbind
* count.fields: Like ncol
* file.rename
        ')
}


#' A Function
#'
#' A command line version of rbind for two files.
#' @param f1 The name/path of the first file.
#' @param f2 The name/path of the second file.
#' @export
#' @examples
#' bash_rbind()

bash_rbind <- function(f1, f2){
  return(file.append(f1, f2))
}

#' A Function
#'
#' A command line version of cbind for two files.
#' @param f1 The name/path of the first file.
#' @param f2 The name/path of the second file.
#' @param f_product Name of the new file produced by the binding
#' @export
#' @examples
#' bash_cbind()

bash_cbind <- function(f1, f2, f_product){
  system(
    glue("paste -d ',' {f1} {f2} > {f_product}")
  )

  return(
    glue("{f1} and {f2} joined column-wise into {f_product}."
    )
}

#' A Function
#'
#' A command line version of nrow
#' @param f1 Name/path of the file
#' @export
#' @examples
#' bash_nrows()

bash_nrows <- function(f1){
  system(
    glue("wc -l {f1}")
  )
}

#' A Function
#'
#' Returns the number of columns in a file
#' @param f1 Name/path of the file.
#' @param sep File separator. Defaults to csv.
#' @export
#' @examples
#' cat_function()

bash_ncols <- function(f1, sep = ','){
  return(unique(count.fields(f1, sep = sep)))
}

#' A Function
#'
#' Show the top n rows of a file.
#' @param f1 The name/path of the file.
#' @param nrows The number of rows to show.
#' @export
#' @examples
#' bash_head()

bash_head <- function(f1, nrows){
  system(
    glue("head -n {nrows} {f1}")
  )
}

#' A Function
#'
#' Finds the sum of a given column.
#' @param f1 The name/path of the file.
#' @param col The name of the column to find the sum of.
#' @export
#' @examples
#' bash_col_sum()

bash_col_sum <- function(f1, col){
  col_num <-  which(names(f1) == col)
  system(
    glue("awk -F"," '{x+=${col_num}}END{print x}' {f1}")
  )
}

#' A Function
#'
#' Finds the sum of a given column.
#' @param f1 The name/path of the file.
#' @param col The name of the column to find the mean of.
#' @export
#' @examples
#' bash_col_mean()

bash_col_mean <- function(f1, col){
  col_num <-  which(names(f1) == col)
  system(
    glue("awk '{ sum += ${col_num} } END { if (NR > 0) print sum / NR }' {f1}")
  )
}

#' A Function
#'
#' Sorting
#' @param f1 The name/path of the file.
#' @param col_sort The name of the column to find the mean of.
#' @export
#' @examples
#' bash_arrange()

bash_arrange <- function(f1, col_sort){

  #Sorting a CSV file by the second column alphabetically

  col_num <-  which(names(f1) == col_sort)

  system(
    glue("sort -t, -k{col_num} {f1}")
  )

  # Numerically
  #sort -t, -k2n filename.csv
  # Reverse order
  #sort -t, -k2nr filename.csv
}

#' A Function
#'
#' This function allows you to express your love of cats.
#' @param f1 The name/path of the file.
#' @param cols_to_remove Names of the columns to removes.
#' @param f_new Optional name for new file if the results should be saved.
#' @export
#' @examples
#' bash_select()

bash_select <- function(f1, cols_to_remove, f2 = NA){

  # which means "select columns 2 through 5 and columns 8, using comma as the separator".
  #cut -f 2-5,8 -d , values.csv

  #Remove columns

  col_nums <-  which(names(f1) == cols_to_remove)
  cols_to_remove_nums <- paste0(col_nums, collapse = ',')


  if(is.na(f2)){
    system(
      glue("cut -d, -f {cols_to_remove_nums} {f1}")
    )

  } else{
    system(
      glue("cut -d, -f {cols_to_remove_nums} {f1} > {f2}")
    )
  }

}

#' A Function
#'
#' Join two files.
#' @param f1 The name/path of the first file.
#' @param col1 Column to use for joining from the first file.
#' @param f2 The name/path of the second file.
#' @param col2 Column to use for joining from the second file.
#' @param f_new Nme for new file.
#' @export
#' @examples
#' bash_join()

bash_join <- function(f1, col1, f2, col2, f_new){

  # Join the first file (-1) by the second column and the second file (-2) by the first

  col_num1 <-  which(names(f1) == col1)
  col_num2 <-  which(names(f2) == col2)

  system(
    glue("join -t, -1 2 -2 1 {f1} {f2} > {f_new}")
  )
}






