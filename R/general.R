#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

ss_sampleseed <- function(x, frac){
    set.seed(8)
    x[sample.int(nrow(x), frac*nrow(x)), ]
    return(x)
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

mode_stats <- function(vec){
    return((tabyl(vec) %>% arrange(desc(n)) %>% pull(1))[1])
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

optimal_hist_bins <- function(vec){
    return(diff(range(vec)) / (2 * IQR(vec) / length(vec)^(1/3)))
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


cohens_h <- function(p1, p2){
    return(abs(2*(asin(sqrt(p1)) - asin(sqrt(p2)))))
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

make_formula <- function(target, additions, subtractions = NULL, powers = NULL){

    add_sub_sep = ifelse(is.null(subtractions), '', '-')
    add <- paste0(additions, collapse = ' + ')
    subtract <- paste0(subtractions, collapse = ' - ')
    add_subtract <- paste(add, subtract, sep = add_sub_sep)

    return(as.formula(paste(target, add_subtract, sep = '~')))

}

#' Rounded Mean Function
#'
#' https://stats.stackexchange.com/questions/78609/outlier-detection-in-very-small-sets/78617#78617
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords
#' @export
#' @examples
#' mean_dp()

small_sample_outliers <- function(vec){
    mi <- .6745*(vec - mean(vec))/mad(vec)
    return(vec[abs(mi) > 3.5])
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

write_txt <- function(fobj, fname){
    con <- file(fname)
    writeLines(c(fobj),  con)
    close(con)
}

