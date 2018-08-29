######################################
#### Twitter Football Text Mining ####
######################################

### Load Packages

require(rtweet) 
require(wordcloud)
require(stringr)

# Variablen zuweisen

hashtags <- c("#m05vfb")
app <- "University of Regensburg"
ApiKey <- "tbMk9DqZkVnfdcJXG9ao0iLxn"
AccessToken <- "1032283823537631234-Ky2tqY77ViKSkyS5E44gs4jLlFYvAl"
AccessTokenSecret <- "YY2Mb6CFAFPEcJQCjMgjJrSA41zHjUFTebp3alsTSG0Yk"


### Funktionen definieren

# API Connect

connectTweetApi <- function(){
  create_token(app = app, consumer_key = ApiKey, consumer_secret = ApiSecretKey,access_token = AccessToken, access_secret = AccessTokenSecret)
}


# Tweets holen

getTweets <- function(hashtgs, nTweets = 25, lang = "de", retweets = F){
  hashtags <- paste(hashtgs,collapse = " OR ")
  connectTweetApi()
  statusupdates = search_tweets(hashtags,nTweets,include_rts = retweets,lang = lang)
  return(statusupdates)
}


# Funktion zum cleanen

cleanTweets <- function(text){
  text <- iconv(text,"latin1", "ASCII", sub = "")
  text <- gsub("&amp", "", text) 
  text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text) 
  text <- gsub("@\\w+", "", text) 
  text <- gsub("[[:punct:]]", "", text) 
  #text <- gsub("[[:digit:]]", "", text) 
  text <- gsub("http\\w+", "", text) 
  text <- gsub("[ \t]{2,}", "", text) 
  text <- gsub("^\\s+|\\s+$", "",text) 
  text <- gsub("\\n", " ", text)
  return(text)
}


# Funktion zu Hashtag Extrahieren

extractHashtags <- function(tweetdf, wordcloud = TRUE){
  library(stringr)
  tweetdf <- tweetdf$text
  hashvector <- unlist(str_extract_all(tweetdf, "#\\S+"))
  if(wordcloud){
    library(wordcloud)
    wordcloud(names(table(hashvector)),table(hashvector),scale = c(3,.5))
  }
  return(hashvector)
}

# Text extrahieren

extractText <- function(tweetdf, cleanText = TRUE, exportCSV = FALSE){
  tweetsTxt <- tweetdf$text
  if(cleanText){
    tweetsTxt <- cleanTweets(tweetsTxt)
  }
  if(exportCSV){
    write.csv2(tweetsTxt,file = "Tweets.csv", row.names = F)
  }
  return(tweetsTxt)
}

# Funktionen nutzen

connectTweetApi()
tweets <- getTweets(hashtags)
text <- extractText(tweets, exportCSV = T)
