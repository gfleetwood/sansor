transform_url_to_rss <- \(youtube_channel_url){
  
  str_replace(
    youtube_channel_url, 
    "https://www.youtube.com/channel/", 
    "https://invidious.namazso.eu/feed/channel/"
    )
  
}
