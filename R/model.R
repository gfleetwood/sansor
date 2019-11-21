library(TSclust)
library(parallelDist)
library(rsample)

#' @title Combined Model Diagnostics
#' @description Combines tidy model diagnostics with the confidence intervals of the estimates
#' @mdl A model object
#' @return Tidy model diagnostics with the confidence intervals of the estimates
#' @export

create_train_test_split <- function(df, prob = .8){

    split <- caret::createDataPartition(
        y = df$target, p = prob, list = F)

    train <- df %>% slice(split) %>% mutate(label = "train")
    test <- df %>% slice(-split) %>% mutate(label = "test")

    result <- rbind(train, test) %>% group_by(label) %>% nest()

    return(result)

}

#' @title Combined Model Diagnostics
#' @description Combines tidy model diagnostics with the confidence intervals of the estimates
#' @mdl A model object
#' @return Tidy model diagnostics with the confidence intervals of the estimates
#' @export

read_train_test_split <- function(df){

    result <- df %>%
        filter(label == "train") %>%
        pull(data) %>%
        magrittr::extract2(1)

    return(result)

}

#' @title Combined Model Diagnostics
#' @description Combines tidy model diagnostics with the confidence intervals of the estimates
#' @mdl A model object
#' @return Tidy model diagnostics with the confidence intervals of the estimates
#' @export

model_diagnostics <- function(mdl){

    result <- inner_join(broom::tidy(mdl), tidy(confint(mdl)), by = c("term" = ".rownames"))

    return(result)

}

#' @title Train Test Split
#' @description This function sets up all I need to start a new project.
#' @param df The dataframe to splitr
#' @param ratio The fraction of data for training
#' @return A list of the training and testing dataframes
#' @export

train_test_split <- function(df, ratio){

    split <- initial_split(data, prop = ratio)
    train <- training(data_split)
    test  <- testing(data_split)

    return(list(train, test))
}

#' @title Time Series Clustering
#' @description This function sets up all I need to start a new project.
#' @param mat A matrix
#' @param dist A distance measure. See the method arg in parallelDist::parDist for the supported parameters.
#' @param linkage The type of linkage to use. See the method arg in TSclust::hclust for the supported parameters.
#' @return The clustering results
#' @export

ts_clustering <- function(mat, dist = "dtw", linkage = "average"){

    mat_dist <- parDist(x = stories_temp, method = dist)
    stories_clust <- hclust(mat_dist,  method = linkage)

    return(stories_clust)

}






