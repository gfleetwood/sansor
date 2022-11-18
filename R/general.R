read_timestamp_as_str_iso8601 <- \(x){
  
  as.POSIXlt(Sys.time(), "UTC") %>% 
    strftime("%Y-%m-%dT%H:%M:%S%z")
  
}


#' @title Unpack List Column As String
#' @description Converts a list column to a string column
#' @param df A dataframe
#' @param col The name of the list column
#' @return A dataframe with NAs instead of #NULLs and empty strings.
#' @export

list_col_to_str <- function(df, col){

    mutate(
        df,
        !!sym(col) := map_chr(!!sym(col), ~ paste(.x, collapse = "-"))
    )

}

#' @title Identify Columns With NAs
#' @description Takes a dataframe and returns the columns which meet a threshold of NAs
#' @param df A dataframe
#' @return
#' @export

cols_missing_data <- function(df, frac){

    df %>%
        skim_to_wide() %>%
        mutate(missing_ratio = as.integer(missing)/as.integer(n)) %>%
        filter(missing_ratio >= frac) %>%
        pull(variable)

}

#' @title Imputation Indicator Columns
#' @description For a given dataframe this functions adds a Boolean column for each column
#' with NAs that indicates the location of these NAs.
#' @param df A dataframe
#' @return A dataframe with indicator columns for columns with at least one NA
#' @export

update_label_nas <- function(df){

    update_label_na <- function(df, col_with_na){

        col_new <- paste(col_with_na, "is_na", sep = "_")

        mutate(
            df,
            {{col_new}} := ifelse(is.na((!!sym(col_with_na))), 1, 0)
        )

    }

    cols_with_na <- df %>%
        keep(~ sum(is.na(.)) > 0) %>%
        names()

    reduce(cols_with_na, ~ update_label_na(.x, .y), .init = df)

}

#' @title Drop NA Columns
#' @description Drops columns with NAs according to a specified criterion.
#' @param df A dataframe
#' @param method The drop criterion:
#' * all: Drop columns with all NAs
#' * frac: Drop columns with NAs above a threshold
#' * any: Drop any columns with at least one NA
#' @param frac The threshold (between 0 and 1 inclusive) to be used when method="frac".
#' @return A dataframe with some columns dropped according to the supplied specification
#' @export

drop_na_col <- function(df, method = "all", frac = 0.1){

    switch(
        method,
        "all" = discard(df, ~ all(is.na(.x))),
        "frac" = discard(df, ~ mean(is.na(.x)) > frac),
        "any" = discard(df, ~ sum(is.na(.x)) > 0)
    )

}

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

find_mode <- function(x) {
u <- unique(x)
tab <- tabulate(match(x, u))
u[tab == max(tab)]
}





#' @title Drop Anomalous Columns
#' @description Uses the xray library to identify anomalous columns and drops them.
#' @param df A dataframe to be examined
#' @return The df variable without the anomalous columns
#' @export

drop_anomalous_cols <- function(df){

    df %>%
        xray::anomalies() %>%
        purrr::pluck("problem_variables", "Variable") %>%
        select(df, -one_of(.))
        
}




#' @title Freedman–Diaconis Histogram Binning
#' @description Uses the Freedman–Diaconis rule for optimal histogram binning.
#' Also see: https://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram/862#862
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @return The Freedman-Diaconis bins
#' @export

freedman_diaconis_hist_bins = function(vec) diff(range(vec)) / (2 * IQR(vec) / length(vec)^(1/3))

read_timestamp_as_date_iso8601 <- \(x){
  
  as.POSIXlt(Sys.time(), "UTC") %>% 
    strptime("%Y-%m-%dT%H:%M:%S%z")
  
}

create_venn_df <- \(
  vector_a, 
  vector_b, 
  a_label = "first_only",
  b_label = "second_only"
){
  
  a_cap_b_label = paste(
    str_remove(a_label, "_only"), 
    str_remove(b_label, "_only"),
    sep = "_intersect_"
  )
  
  intersection <- intersect(vector_a, vector_b)
  a_only <- setdiff(vector_a, vector_b)
  b_only <- setdiff(vector_b, vector_a)
  data <- c(intersection, a_only, b_only)
  
  label <- c(
    rep(a_cap_b_label, length(intersection)),
    rep(a_label, length(a_only)),
    rep(b_label, length(b_only))
  )
  
  result <- data.frame(data, label)
  
}

create_df_transpose <- \(df){
  
  df %>% 
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(cols = everything()) 
  
}


nested_json_to_df <- \(json_data_as_list){
  
  fromJSON(json_data_as_list, flatten = T) %>% 
    create_df_transpose()
  
}
