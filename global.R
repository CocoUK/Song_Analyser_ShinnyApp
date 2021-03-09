
library(musicbrainz)
library(dplyr)
library(stringr)
library(R.utils)
library(rstatix)
library(wordcloud2)
library(tm)
library(memoise)
library(shinycssloaders)


# List of artists
artists <<- list ("Kylie Minogue",  "Queen", "Blondie", "Madonna")

getSongLyrics <- function(artist, nsongs){
  # malicious user could manipulate this value.
  if (!(artist %in% artists))
    stop("Unknown artist")
  
  songs <- browse_works_by('artist', artist_mbid(artist))
  titles <- head(Song_titles(songs),nsongs)
  lyrics_count <- song_lyrics(artist,titles)
  return(lyrics_count) 
}


###Functions
## Returns artist mbid number
artist_mbid <- function(x){
  artist <- gsub(" ", "%20", x)
  artist_df <- search_artists(artist)
  artist_id <- artist_df %>%
    select(mbid) %>%
    slice(1) %>%
    pull()
  
  return(artist_id)
}



## Function extract song titles
Song_titles <- function (artist_data){
  title <- artist_data %>%
    select(title) %>%
    slice(1:1000) %>%
    pull()
  
  title <- title %>% tolower() %>% unique()
  
  return(title)
  
}


##Extract lyrics of songs and count words

song_lyrics <- function (artist, title){
  
  root_url <- "https://api.lyrics.ovh/v1/"
  artist <- gsub(" ", "%20", artist)
  ## replace space for %20 in song title
  title <- gsub(" ", "%20", title)
  all_urls <- paste0(root_url,artist,"/",title)
  ##  Get all song lyrics and count words
  cbind(title, purrr::map_df(all_urls, function(my_url) {
    my_content_from_json <- withTimeout(jsonlite::fromJSON(my_url)$lyrics, timeout = 5, onTimeout = "silent")
    if(is.null(my_content_from_json)) return(data.frame(lyrics = NA, word_count = NA))
    lyrics <- gsub("[\r\n]", " ", my_content_from_json)
    #Remove words in '[]' in the lyrics.
    lyrics <- gsub('\\[.*?]', '', lyrics)
    lyrics <- str_squish(lyrics)
    word_count <- str_count(lyrics, '\\s+') + 1
    #If lyrics has word 'Instrumental' turn word count to NA.
    if(grepl('Instrumental', lyrics, ignore.case = TRUE)) word_count <- NA
    data.frame(  lyrics, word_count)
  })) -> result
  
}



###  wordcloud function
artist_wordcloud <- function (lyric_count){
  ## create vector containing lyrics
  all_lyrics <- paste(unlist(lyric_count$lyrics), collapse =" ")
  
  # create corpus
  docs <- Corpus(VectorSource(all_lyrics))
  
  #clean data
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  ##document term matrix
  dtm <- TermDocumentMatrix(docs)
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix),decreasing=TRUE)
  df <- data.frame(word = names(words),freq=words)
  
  ## Generate the world cloud
  #wordcloud2(data=df, size=1.6, color='random-dark')
  wordcloud::wordcloud(df$word, df$freq, min.freq = 1, colors=brewer.pal(8, "Dark2"))
}
