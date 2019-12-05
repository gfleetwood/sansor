drop_anomalous_cols <- function(df){

    anomalous_cols <- df %>%
        xray::anomalies() %>%
        purrr::pluck("problem_variables", "Variable")

    result <- select(df, -one_of(anomalous_cols))

    return(result)

}

train_test_split <- function(df){

    index <- caret::createDataPartition(df$target, p = 0.8, list = FALSE)
    train <- slice(df, index)
    test <- slice(df, -index)

    return(list(train = train, test = test))

}

drop_na_col_some <- function(df, frac = 0.1){

    result <- df %>%
        discard(~ mean(is.na(.x)) > 0.1)

    return(result)

}

drop_na_col_any <- function(df){

    result <- df %>%
        discard(~ sum(is.na(.x)) > 0)

    return(result)

}

drop_na_col_all <- function(df){

    result <- df %>%
        discard(~ all(is.na(.x)))

    return(result)

}

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
