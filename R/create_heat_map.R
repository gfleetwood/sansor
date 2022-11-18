#' @title Correlation Heatmap
#' @description Generate a statis or interactive heatmap.
#' @param df A dataframe
#' @return A correlation heatmap of the numeric columns of the dataframe.
#' @export

create_heat_map <- function(df, interactive = FALSE){

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


