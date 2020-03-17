#' @title Linear Model Evaluation
#' @description Calculates several metrics for a trained linear regression model
#' @param mdl A trained model
#' @return A list of linear model evaluation metrics.
#' @export

lm_check <- function(mdl){

    result <- list(
        car::avPlots(mdl),
        car::influencePlot(mdl), # Outlier Analysis
        car::vif(mdl), # multi-colinearity
        car::durbinWatsonTest(mdl), # Autocorrelated Errors
        lmtest::bptest(mdl) # heteroskedasticity of errors
    )

    return(result)

}

#' @title Model Calibration
#' @description
#' @param df A dataframe
#' @param preds_col A string with the name of the column containing the probabilistic
#' predictions
#' @param binary_class_col A string with the name of the column with the labels
#' @export

model_calibration <- function(df, preds_col, binary_class_col){

    result <- df %>%
        ggplot(aes(!!sym(preds_col), !!sym(binary_class_col))) +
        geom_smooth()

    return(result)

}

#' @title Train Test Split
#' @description Splits a dataframe into training and testing sets
#' @param df A dataframe
#' @param frac The proportion of the data to be used for training
#' @return A dataframe with an extra column called train where a 1 means the data point
#' is in the training set.
#' @export

train_test_split <- function(df, frac){

    # index <- caret::createDataPartition(df$target, p = frac, list = FALSE)
    # train <- slice(df, index)
    # test <- slice(df, -index)

    df_split <- initial_split(df, prop = frac)
    train <- training(df_split) %>% mutate(train = 1)
    test <- testing(df_split) %>% mutate(train = 0)

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
#' @description Hierarchical clustering for time series with dynamic time warping.
#' @param mat A matrix
#' @param dist A distance measure. See the method arg in parallelDist::parDist for the supported parameters.
#' @param linkage The type of linkage to use. See the method arg in TSclust::hclust for the supported parameters.
#' @return A hierarchical cluster object
#' @export

ts_clustering <- function(mat, dist = "dtw", linkage = "average"){

    mat_dist <- parDist(x = mat, method = dist)
    result <- hclust(mat_dist, method = linkage)

    return(result)

}
