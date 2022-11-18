#' @title Requirements Script
#' @description hold
#' @return Lists all loaded packages
#' @export
create_requirements_script <- \(){

  installed.packages() %>%
    as.data.frame(stringsAsFactors = F) %>%
    select(Package, Version) %>%
    mutate(
      install_cmd = glue('devtools::install_version("{Package}", version = "{Version}", dependencies = TRUE)')
    ) %>%
    pull(install_cmd) %>%
    readr::write_lines("./requirements.r")

}
