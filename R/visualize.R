#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

cor_interactive <- function(df){

    plot <- df %>%
        select_if( function(x) is.numeric(x) | is.integer(x)) %>%
        cor() %>%
        plot_ly(
            x = rownames(.),
            y = rownames(.),
            colorscale = "Greys",
            z = .,
            type = "heatmap"
        )
    return(plot)
}

