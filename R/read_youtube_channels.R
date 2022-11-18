# Go to this page: https://www.youtube.com/feed/channels
# Download the html with right-click and then Save As.
# Feed the path to the file into the read_html function below.

read_channel_name_and_link <- \(html_section){

  name <- html_section %>%
    html_element(".style-scope ytd-channel-name") %>%
    html_text2() %>%
    map_chr(~ unlist(str_split(.x, "\n"))[1])

  link <-  html_section %>%
    html_element(".channel-link") %>%
    html_attr("href")

  tibble(name = name, link = link)

}

read_yt_html <- \(path){

  read_html(path) %>%
    html_elements("#content-section") %>%
    map_df(read_channel_name_and_link) %>%
    write_csv("~/yt_channels.csv")

}


