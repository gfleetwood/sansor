#' @title Combined Model Diagnostics
#' @description Combines tidy model diagnostics with the confidence intervals of the estimates
#' @mdl A model object
#' @return Tidy model diagnostics with the confidence intervals of the estimates
#' @export

create_train_test_split <- function(df, prob = .8){

    # split <- initial_split(data, prop = ratio)
    # train <- training(data_split)
    # test  <- testing(data_split)

    split <- caret::createDataPartition(
        y = df$target, p = prob, list = F)

    train <- df %>%
        slice(split) %>%
        mutate(label = "train")
    test <- df %>%
        slice(-split) %>%
        mutate(label = "test")

    result <- rbind(train, test)

    return(result)

}

#' @title Combined Model Diagnostics
#' @description Combines tidy model diagnostics with the confidence intervals of the estimates
#' @mdl A model object
#' @return Tidy model diagnostics with the confidence intervals of the estimates
#' @export

model_diagnostics <- function(mdl){

    result <- inner_join(tidy(mdl), tidy(confint(mdl)), by = c("term" = ".rownames"))

    return(result)

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

