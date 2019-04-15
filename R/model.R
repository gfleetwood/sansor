library(TSclust)
library(parallelDist)
library(rsample)

#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

get_model_diagnostics <- function(mdl){
    return(inner_join(broom::tidy(mdl), tidy(confint(mdl)), by = c("term" = ".rownames")))
}

calc_accuracy <- function(labels, preds){

    results <- table(labels, preds) %>%
        data.frame %>%
        mutate(correct = ifelse(labels==preds, 1, 0)) %>%
        group_by(correct) %>%
        summarise(freq = sum(Freq)) %>%
        ungroup() %>%
        mutate(prop = freq/sum(freq)) %>%
        filter(correct == 1)

    return(results)
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

model_glm <- function(df) {
    glm(y ~ ., data = df, family = binomial(link="logit"))
}

impute_with_na <- function(df, val){

    df[df==""] <- NA
    return(df)

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


train_test_split <- function(df, ratio){

    split <- initial_split(data, prop = ratio)
    train <- training(data_split)
    test  <- testing(data_split)

    return(list(train, test))
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

ts_clustering <- function(mat, dist = "dtw", linkage = "average"){
    mat_dist <- parDist(x = stories_temp, method = dist)
    stories_clust <- hclust(mat_dist,  method = linkage)
    return(stories_clust)
}






