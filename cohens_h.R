#' @title Cohen's H
#' @description Calculates Cohen's H for two probabilities or proportions.
#' @param p1 The first probability or proportion
#' @param p2 The second probability or proportion
#' @return
#' @export

cohens_h = \(p1, p2) {

    abs(2*(asin(sqrt(p1)) - asin(sqrt(p2))))

}
