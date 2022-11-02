#' @title Mode
#' @description Calculates the statistical mode
#' @param vec A numeric/integer vector
#' @return The mode of the submitted vector
#' @export

read_mode <- function(vec){
  
  vec %>% 
    tabyl() %>%
    arrange(desc(n)) %>%
    pull(1) %>%
    pluck(1)
  
}