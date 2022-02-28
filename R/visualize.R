#' @title Correlation Heatmap
#' @description
#' @param df A dataframe
#' @return A correlation heatmap of the numeric columns of the dataframe.
#' @export

heat_map <- function(df){

    result <- df %>%
        select_if(is.numeric) %>%
        cor() %>%
        corrplot(method = "circle", is.corr = FALSE)
        
}

#' @title Interactive Correlation Map
#' @description Produces an interactive correlation map of the numeric/integer variables
#' @param df A dataframe
#' @return An interactive correlation map of the numeric/integer variables
#' @export

cor_interactive <- function(df){

    result <- df %>%
        select_if(function(x) is.numeric(x) | is.integer(x)) %>%
        cor() %>%
        plot_ly(
            x = rownames(.), y = rownames(.), z = .,
            colorscale = "Greys", type = "heatmap"
            )

}

#' @title Freedman–Diaconis Histogram Binning
#' @description Uses the Freedman–Diaconis rule for optimal histogram binning.
#' Also see: https://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram/862#862
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @return The Freedman-Diaconis bins
#' @export

fd_binning <- function(vec){

    diff(range(vec)) / (2 * IQR(vec) / length(vec)^(1/3))

}


