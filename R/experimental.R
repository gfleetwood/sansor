model_calibration <- function(df){

    result <- ggplot(df, aes(fitted_probability, binary_class_indicator)) + geom_smooth()

    return(result)

}

heatmap <- function(df){

    result <- select_if(df, is.numeric) %>%
        cor() %>%
        corrplot(method = "circle", is.corr = FALSE)

    return(result)

}

lm_check <- function(mdl){

    result <- list(car::avPlots(mdl),
                   car::influencePlot(mdl), # Outlier Analysis
                   car::vif(mdl), # multi-colinearity
                   car::durbinWatsonTest(mdl), # Autocorrelated Errors
                   lmtest::bptest(mdl)) # heteroskedasticity of errors

    return(result)

}
