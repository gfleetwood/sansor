#' @title Outliers In Small Samples
#' @description A function to find outliers in small samples. Method seen here: https://bit.ly/2DFmsJr
#' @param vec A numeric/integer vector
#' @return A boolean vector of which values are and are not outliers
#' @export

outliers_iglewicz_hoaglin = \(vec){

    mi <- .6745*(vec - mean(vec))/mad(vec)
    vec[abs(mi) > 3.5]

}