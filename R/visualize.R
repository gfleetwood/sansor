#' @title Correlation Heatmap
#' @description Generate a statis or interactive heatmap.
#' @param df A dataframe
#' @return A correlation heatmap of the numeric columns of the dataframe.
#' @export

heat_map = function(df, interactive = FALSE){

    mat = df %>%
        select_if(is.numeric) %>%
        cor() 
        
    interactive_flag = ifelse(interactive, "T", "F")
    
    switch(
    interactive_flag,
    "T" = corrplot(mat, method = "circle", is.corr = FALSE),
    "F" = plot_ly(
            x = rownames(mat), y = rownames(mat), z = mat,
            colorscale = "Greys", type = "heatmap"
            )  
    )
              
}

#' @title Freedman–Diaconis Histogram Binning
#' @description Uses the Freedman–Diaconis rule for optimal histogram binning.
#' Also see: https://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram/862#862
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @return The Freedman-Diaconis bins
#' @export

freedman_diaconis_hist_bins = function(vec) diff(range(vec)) / (2 * IQR(vec) / length(vec)^(1/3))


