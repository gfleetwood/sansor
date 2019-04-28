library(TSclust)
library(parallelDist)
library(rsample)

#' Combined Model Diagnostics
#'
#' Combines tidy model diagnostics with the confidence intervals of the estimates
#' @mdl A model object
#' @return Tidy model diagnostics with the confidence intervals of the estimates

model_diagnostics <- function(mdl){

    result <- inner_join(broom::tidy(mdl), tidy(confint(mdl)), by = c("term" = ".rownames"))

    return(result)

}

#' Train Test Split
#'
#' This function sets up all I need to start a new project.
#' @param df The dataframe to splitr
#' @param ratio The fraction of data for training
#' @return A list of the training and testing dataframes


train_test_split <- function(df, ratio){

    split <- initial_split(data, prop = ratio)
    train <- training(data_split)
    test  <- testing(data_split)

    return(list(train, test))
}

#' Time Series Clustering
#'
#' This function sets up all I need to start a new project.
#' @param mat A matrix
#' @param dist A distance measure. See the method arg in parallelDist::parDist for the supported parameters.
#' @param linkage The type of linkage to use. See the method arg in TSclust::hclust for the supported parameters.
#' @return

ts_clustering <- function(mat, dist = "dtw", linkage = "average"){

    mat_dist <- parDist(x = stories_temp, method = dist)
    stories_clust <- hclust(mat_dist,  method = linkage)

    return(stories_clust)

}






