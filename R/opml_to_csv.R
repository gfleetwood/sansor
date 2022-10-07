library(tidyverse)
library(xml2)

create_df_repr <- \(x){
  
  tibble(
    type = attributes(x)$type,
    text = attributes(x)$text,
    xmlUrl = attributes(x)$xmlUrl
  )
  
}

process_opml <- \(opml_path){
  
  podcasts_df <- read_xml(opml_path) %>% 
    xml2::as_list() %>% 
    purrr::transpose() %>% 
    as_tibble() %>% 
    pull(body) %>% 
    pluck("opml", "outline") %>%
    map_df(create_df_repr)
  
}


