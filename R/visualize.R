#' @title Interactive Correlation Map
#' @description Produces an interactive correlation map of the numeric/integer variables
#' @param df A dataframe
#' @return An interactive correlation map of the numeric/integer variables
#' @export

cor_interactive <- function(df){

    result <- df %>%
        select_if( function(x) is.numeric(x) | is.integer(x)) %>%
        cor() %>%
        plot_ly(x = rownames(.), y = rownames(.), z = .,
            colorscale = "Greys", type = "heatmap")

    return(result)
}

