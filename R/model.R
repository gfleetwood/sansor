#' @title Linear Model Evaluation
#' @description Calculates several metrics for a trained linear regression model
#' @param mdl A trained model
#' @return A list of linear model evaluation metrics.
#' @export

lm_check = function(mdl){

    list(
        car::avPlots(mdl),
        car::influencePlot(mdl), # Outlier Analysis
        car::vif(mdl), # multi-colinearity
        car::durbinWatsonTest(mdl), # Autocorrelated Errors
        lmtest::bptest(mdl) # heteroskedasticity of errors
    )

}

#' @title Model Calibration
#' @description
#' @param df A dataframe
#' @param preds_col A string with the name of the column containing the probabilistic
#' predictions
#' @param binary_class_col A string with the name of the column with the labels
#' @export

model_calibration = function(df, preds_col, binary_class_col){

    df %>%
        ggplot(aes(!!sym(preds_col), !!sym(binary_class_col))) +
        geom_smooth()

}

#' @title Time Series Clustering
#' @description Hierarchical clustering for time series with dynamic time warping.
#' @param mat A matrix
#' @param dist A distance measure. See the method arg in parallelDist::parDist for the supported parameters.
#' @param linkage The type of linkage to use. See the method arg in TSclust::hclust for the supported parameters.
#' @return A hierarchical cluster object
#' @export

time_series_clustering = function(mat, dist = "dtw", linkage = "average"){

    mat_dist <- parDist(x = mat, method = dist)
    result <- hclust(mat_dist, method = linkage)

}



