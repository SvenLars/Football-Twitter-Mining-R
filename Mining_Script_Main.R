######################################
#### Twitter Football Text Mining ####
######################################

### Packages

require(rtweet)
require(stringr)

## Optional packages
#require(wordcloud)

### Global variables

##Setting up keys for accesing twitter

AccessToken <- "1032283823537631234-Ky2tqY77ViKSkyS5E44gs4jLlFYvAl"
AccessTokenSecret <- "YY2Mb6CFAFPEcJQCjMgjJrSA41zHjUFTebp3alsTSG0Yk"
ApiKey <- "tbMk9DqZkVnfdcJXG9ao0iLxn"
app <- "University of Regensburg"

## Vector of Hashtags matching teams
hashtagFirstTeam <- c("#regensburg","#wetter")
hashtagSecondTeam <-c("#rich","#poor")

## to store the tweets elsewhere on your pc file path is required
## otherwise CSV Files will be stored in your working directory
csvFilePath <- ""

##Specifying the input parameters of getTweets Function
nTweets = 10000          #Number of tweets returned by the function
lang = "de"              #Language of the tweets e.g. "en" = english, "de" = german
retweets = FALSE         #Enabling retweets or not
retryonratelimit = FALSE   #Every 15 minutes the Api automatically reconnects to twitter after exceeding the tweet limit



## Functions

# Twitter rate limits cap the number of search results returned to 18,000 every 15 minutes. 
# To request more than that, simply set retryonratelimit = TRUE and rtweet will wait for rate limit resets for you.

connectingTweetApi <- function(){
  if(require(rtweet)){
  create_token(app = app, consumer_key = ApiKey, consumer_secret = ApiSecretKey,access_token = AccessToken, access_secret = AccessTokenSecret)
  }else{library("rtweet")}
}

# getTweets returns a DataFrame Object called "containerTweets" containing all tweets matching the input hashtags
# once can go back only for 6 to 9 days

getTweets <- function(){
  hashtags <- paste(hashtagFirstTeam,hashtagSecondTeam, sep = " OR ",collapse = " OR ")
  connectingTweetApi()
  containerTweets <- search_tweets(q=hashtags,n=nTweets, lang = lang,include_rts =  retweets,retryonratelimit =  retryonratelimit)
  return(containerTweets)
}


### Sorting And Writing tweets to file with respect to hashtag vector

# Helper function
# Evaluating whether vector of hashtags containing specific hashtags. Returns boolean
evaluating<-function(string, searchkey){
  library(stringr)
  searchkey <- tolower(searchkey)
  string <- tolower(string)
  searchkeyConcatenated <- paste(searchkey, collapse = "|")
  return(any(str_detect(searchkeyConcatenated,string)))
}

#Sorting tweets with respect to the hashtags
SortingTweets <-function(tweetdataframe, hashtagTeamOne, hashtagTeamTwo){
  dataframeTeamOne <- tweetdataframe[0, ]             #setting up 3 empty dataframes with same structure 
  dataframeTeamTwo <- tweetdataframe[0, ]             # as tweetdataframe but without any observations
  dataframeLeftovers <- tweetdataframe[0, ]           # This on should catch those without any match
  lengthOfInputDf <- length(tweetdataframe$hashtags)  #lenght of input dataframe
  
  for(i in 1:lengthOfInputDf){

       if(evaluating(tweetdataframe$hashtags[[i]],hashtagTeamOne)){
         dataframeTeamOne <- rbind(dataframeTeamOne,as.vector(tweetdataframe[i,]))
       }
       else if(evaluating(tweetdataframe$hashtags[[i]],hashtagTeamTwo)){
         dataframeTeamTwo <-  rbind(dataframeTeamTwo,as.vector(tweetdataframe[i,]))
       }
      #else{dataframeLeftovers <- rbind(dataframeLeftovers,as.vector(tweetdataframe[i,]))
      #}
    
  }
  return(list(dataframeTeamOne, dataframeTeamTwo) )
}
  
  #Writing extracted text of tweets to csv file
writingCSV <- function(ListOfDataFrames){
  firstTeam <- paste(hashtagFirstTeam,collapse = " ")
  secondTeam <- paste(hashtagSecondTeam,collapse = " ")
  firstTeam <- paste(firstTeam,".csv",sep = "")
  secondTeam <- paste(secondTeam,".csv",sep = "")
  teamNames <- c(firstTeam,secondTeam)
  j <-1
  for(i in ListOfDataFrames){
    pureText <- extractText(i)
    cleanPureText <- cleanTweets(pureText)
    
    write.csv(cleanPureText, file = teamNames[j],row.names=FALSE, na="")
    
    j<-j+1
  }
  
}

# Extracting pure text from collected tweets. Input: tweetdf -> Data Frame object containig tweets

extractText<-function(tweetdf){
  tweetsTxt <- tweetdf$text
  return(tweetsTxt)
}

# Cleaning text. Input: text ->  Vector of Strings to be cleaned

cleanTweets <- function(text){
  text <- iconv(text,"latin1", "ASCII", sub = "")       # Converting Strings to ASCII
  text <- gsub("&amp", "", text)                        # removing expression &amp
  text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text) # removing expression like RT or via
  text <- gsub("@\\w+", "", text)                       # removing expressions starting with @
  text <- gsub("[[:punct:]]", "", text)                 # removing punctuation
  #text <- gsub("[[:digit:]]", "", text)                # removing digits
  text <- gsub("http\\w+", "", text)                    # removing hyperlinks
  text <- gsub("[ \t]{2,}", "", text)                   # removing tabs
  text <- gsub("^\\s+|\\s+$", "",text)                  # removing backslashes 
  text <- gsub("\\n", " ", text)                        # removing newline
  return(text)
}

########## Merging functions to automize getting and writting tweets ######################
### Before executing the master function you must have done the following
### 1. Adding all global variables and functions to your Environment!

grabbingTweets<-function(){

#Downloading Tweets from Twitter
  DataFrameTweets <- getTweets()
  print("Step 1 executed: Tweets have been downlaoded successfully")

#Sorting Tweets according to hashtag vectors
  ListOfSortedDataFrames <- SortingTweets(DataFrameTweets,hashtagFirstTeam,hashtagSecondTeam)
  print("Step 2 executed: Tweets have been sorted successfully")
  
#Extracting, Cleaning and Writing text to csv Files accordingly
  writingCSV(ListOfSortedDataFrames)
  print("Step 3 executed: Tweets have been written down successfully")

}
