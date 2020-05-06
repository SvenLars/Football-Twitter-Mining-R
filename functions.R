################################################## Functions ###################################################################################
#packages needed by functions
# install.packages("glue")
# install.packages("httr")
# install.packages("stringr")
# install.packages("jsonlite")
# install.packages("openxlsx")
# install.packages("rtweet")
# install.packages("readr") # for getting better performance when reading large twitter .json files
# install.packages("taskscheduleR")
# /packages
require(glue)
require(httr)
require(stringr)
require(jsonlite)
require(openxlsx)
require(rtweet)
require(readr)
#require(taskscheduleR)

# Twitter

# Twitter rate limits cap the number of search results returned to 18,000 every 15 minutes. 
# To request more than that, simply set retryonratelimit = TRUE and rtweet will wait for rate limit resets for you.

# Getting conncected with twitter
connectingTweetApi <- function(){
  if(require(rtweet)){
    ##Setting up keys for accesing twitter
    AccessToken <- "1032283823537631234-Ky2tqY77ViKSkyS5E44gs4jLlFYvAl"
    AccessTokenSecret <- "YY2Mb6CFAFPEcJQCjMgjJrSA41zHjUFTebp3alsTSG0Yk"
    ApiSecretKey <- "lLiODXJm5kiZfheZvbiDQhAfhmBQ5307QvlwviPuZiTFq5E9nI"
    ApiKey <- "tbMk9DqZkVnfdcJXG9ao0iLxn"
    app <- "University of Regensburg"
    create_token(app = app, consumer_key = ApiKey, consumer_secret = ApiSecretKey,access_token = AccessToken, access_secret = AccessTokenSecret)
  }else{library("rtweet")}
}

# All functions needed for using past tweets data (RESTful API)

# getTweets returns a DataFrame Object called "containerTweets" containing all tweets matching the input hashtags
# once can go back only for 6 to 9 days

getTweets <- function(hashtagFirstTeam,hashtagSecondTeam,nTweets = 18000, lang = "de" ,retweets = FALSE,retryonratelimit = TRUE){
  # hashtags <- paste(hashtagFirstTeam,hashtagSecondTeam, sep = " OR ",collapse = " OR ") doesn´t work with string of hashtags
  connectingTweetApi()
  if(hashtagSecondTeam == ""){
    hashtags = hashtagFirstTeam
    containerTweets <- search_tweets(q=hashtags,n=nTweets, lang = lang,include_rts =  retweets,retryonratelimit =  retryonratelimit)
    return(containerTweets)
  }else{
  hashVec1 = unlist(strsplit(trimws(hashtagFirstTeam)," "))
  hashVec2 = unlist(strsplit(trimws(hashtagSecondTeam)," "))
  hashtags = paste(hashVec1,hashVec2,sep = " OR ",collapse = " OR ")
  # if one hashtag is empty
  #hashtags = gsub("^\\ OR|\\OR $", "", hashtags)
  #hashtags = trimws(hashtags)
  containerTweets <- search_tweets(q=hashtags,n=nTweets, lang = lang,include_rts =  retweets,retryonratelimit =  retryonratelimit)
  #save(containerTweets,file = "containerTweets.Rda") #######################
  return(containerTweets)
  }
}

### Sorting And Writing tweets to file with respect to hashtag vector

sortingTweets2 = function(dataframe,...){
  #Initialising empy list to store the output
  store = list()
  index = 1
  for(i in list(...)){
    #Transorming elements from #BSCWOB #WOB to "BSCWOB|WOB"
    searchkeys = str_sub(i,start = 2)
    searchkeys = str_replace_all(searchkeys," ","")
    searchkeys = str_replace_all(searchkeys,"#","|")
    
    #subsetting df regarding searchkey respectively
    sortedTeam = testDF[str_detect(tolower(dataframe$hashtags),tolower(searchkeys)),]
    
    
    store[[index]] = sortedTeam
    index = index + 1
  }
  return(store)
}

# Helper function
# Evaluating whether vector of hashtags containing specific hashtags. Returns boolean
# evaluating<-function(string, searchkey){
#   library(stringr)
#   searchkey <- tolower(searchkey)
#   string <- tolower(string)
#   searchkeyConcatenated <- paste(searchkey, collapse = "|")
#   return(any(str_detect(searchkeyConcatenated,string)))
# }
# 
# #Sorting tweets with respect to the hashtags
# SortingTweets <-function(tweetdataframe, hashtagTeamOne, hashtagTeamTwo){
#   dataframeTeamOne <- tweetdataframe[0, ]             #setting up 3 empty dataframes with same structure 
#   dataframeTeamTwo <- tweetdataframe[0, ]             # as tweetdataframe but without any observations
#   dataframeLeftovers <- tweetdataframe[0, ]           # This on should catch those without any match
#   lengthOfInputDf <- length(tweetdataframe$hashtags)  #lenght of input dataframe
#   
#   for(i in 1:lengthOfInputDf){
#     
#     if(evaluating(tweetdataframe$hashtags[[i]],hashtagTeamOne)){
#       dataframeTeamOne <- rbind(dataframeTeamOne,as.vector(tweetdataframe[i,]))
#     }
#     else if(evaluating(tweetdataframe$hashtags[[i]],hashtagTeamTwo)){
#       dataframeTeamTwo <-  rbind(dataframeTeamTwo,as.vector(tweetdataframe[i,]))
#     }
#     #else{dataframeLeftovers <- rbind(dataframeLeftovers,as.vector(tweetdataframe[i,]))
#     #}
#     
#   }
#   return(list(dataframeTeamOne, dataframeTeamTwo) )
# }
#############################################################
# evaluating<-function(string, searchkey){
#   library(stringr)
#   searchkey <- tolower(searchkey)
#   string <- tolower(string)
#   # split string of keywords in chr vector of keywords
#   searchkey = unlist(strsplit(trimws(searchkey)," "))
#   searchkeyConcatenated <- paste(searchkey, collapse = "|")
#   return(any(str_detect(searchkeyConcatenated,string)))
#   # return(any(str_detect(string,searchkeyConcatenated)))
# }

# searchkey = "#FCB #FCBayern #MiaSanMia"
# searchkey = unlist(strsplit(trimws(searchkey)," "))
# searchkeyConcatenated <- paste(searchkey, collapse = "|")
# searchkeyConcatenated

#Sorting tweets with respect to the hashtags

# SortingTweets <-function(tweetdataframe, hashtagTeamOne, hashtagTeamTwo){
#   dataframeTeamOne <- tweetdataframe[0, ]             #setting up 3 empty dataframes with same structure 
#   dataframeTeamTwo <- tweetdataframe[0, ]             # as tweetdataframe but without any observations
#   dataframeLeftovers <- tweetdataframe[0, ]           # This on should catch those without any match
#   lengthOfInputDf <- length(tweetdataframe$hashtags)  #lenght of input dataframe
#   
#   for(i in 1:lengthOfInputDf){
#     
#     if(evaluating(tweetdataframe$hashtags[[i]],hashtagTeamOne)){
#       dataframeTeamOne <- rbind(dataframeTeamOne,as.vector(tweetdataframe[i,]))
#     }
#     else if(evaluating(tweetdataframe$hashtags[[i]],hashtagTeamTwo)){
#       dataframeTeamTwo <-  rbind(dataframeTeamTwo,as.vector(tweetdataframe[i,]))
#     }
#     #else{dataframeLeftovers <- rbind(dataframeLeftovers,as.vector(tweetdataframe[i,]))
#           
#     #}
#     
#   }
#   #write.xlsx(dataframeLeftovers,file = "Lefties.xlsx")
#   # adding result 
#   DataFrame <<- list(dataframeTeamOne, dataframeTeamTwo)
#   return(list(dataframeTeamOne, dataframeTeamTwo) )
# }

#Writing extracted text of tweets to csv file



writingxlsx <- function(ListOfDataFrames,FirstTeam, SecondTeam,zeit){
  require(openxlsx)
  ### naming ###
  currentTime = format(zeit, format = "%Y-%m-%d %H.%M.%S")
  # concatenating chr vector to one string
  FirstTeam = paste(FirstTeam,sep = " ", collapse = " ")
  SecondTeam = paste(SecondTeam,sep = " ", collapse = " ")
  firstTeam <- paste(currentTime, FirstTeam,collapse = " ")
  secondTeam <- paste(currentTime, SecondTeam,collapse = " ")
  firstTeamxlsx <- paste(firstTeam,".xlsx",sep = "")
  secondTeamxlsx <- paste(secondTeam,".xlsx",sep = "")
  firstTeamRDA = paste(firstTeam,".Rda",sep = "")
  secondTeamRDA = paste(secondTeam,".Rda",sep = "")

  teamNamesxlsx = c(firstTeamxlsx,secondTeamxlsx)
  teamNamesRDA = c(firstTeamRDA,secondTeamRDA)
  ### /naming ###
  
  j <-1
  for(i in ListOfDataFrames){
    write.xlsx(i,file = teamNamesxlsx[j])
    save(i,file = teamNamesRDA[j])
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

grabbingTweets<-function(hashtagFirstTeam,hashtagSecondTeam,nTweets = 10000, lang = "de" ,retweets = FALSE,retryonratelimit = TRUE,zeit){
  
  #Downloading Tweets from Twitter
  DataFrameTweets <- getTweets(hashtagFirstTeam,hashtagSecondTeam,nTweets = nTweets, lang = lang ,retweets = retweets,retryonratelimit = retryonratelimit)
  print("Step 1 executed: Tweets have been downlaoded successfully")
  
  #Sorting Tweets according to hashtag vectors
  ListOfSortedDataFrames <- sortingTweets2(DataFrameTweets,hashtagFirstTeam,hashtagSecondTeam)
  print("Step 2 executed: Tweets have been sorted successfully")
  
  #Extracting, Cleaning and Writing text to csv Files accordingly
  writingxlsx(ListOfSortedDataFrames,hashtagFirstTeam,hashtagSecondTeam,zeit = zeit)
  print("Step 3 executed: Tweets have been written down successfully")
  
}

automatedGrabbingTweets <- function(startDate = "2019-08-17", endDate = "2019-08-17",numberofTweets = 18000, language = "de"){
  Df1 = getSubsettedDf(startDate = startDate,endDate = endDate)
  rounds = nrow(Df1)
  for(i in 1:rounds){
  Hashtags.HomeTeam = Df1$Hashtags.HomeTeam[i]
  Hashtags.AwayTeam = Df1$Hashtags.AwayTeam[i]
  grabbingTweets(Hashtags.HomeTeam, Hashtags.AwayTeam,nTweets = numberofTweets,lang = language)
  }

}


######################## start Input: df to sort and DfFootballData from GlobalEnvironment##############
sortingStreamTweets <- function(DfFootballdata = DfFootballData,tweetdf){
  bigvector = c(DfFootballdata$Hashtags.HomeTeam,DfFootballdata$Hashtags.AwayTeam)
  bigvector = trimws(bigvector) #removing leading and trailing whitespaces
  searchkeys = gsub("#", "",bigvector) #getting rid of leading #
  searchkeys = tolower(searchkeys)
  currTime = format(Sys.time(), "%Y-%M-%d %H.%M.%S")
  ### start loop ### 1 to length(searchkey)
  for(i in 1:length(searchkeys)){
    testText1 = tweetdf$text
    testText1 = tolower(testText1)
    searchkeys[i] #10
    c =  strsplit(searchkeys[i]," ")
    c = unlist(c)
    pattern = paste(c, collapse = "|")
    
    boolvec =  grepl(pattern,testText1 )
    
    #checking if boolvec has at least one True element or DataFrame is not empty
    if(any(boolvec) && nrow(tweetdf)!=0){
      #subsetting df mit boolvec
      dfnn = tweetdf[boolvec,]
      #naming pattern
      
      pattern = paste(currTime, pattern, ".xlsx", sep = " ")
      #writing to xlsx file
      write.xlsx(dfnn,file = pattern)
      #getting rid of already written rows -> reducing nmb rows of data frame
      tweetdf = tweetdf[!boolvec,]
    }
    else{
      # do nothing
    }
  }
  # end loop
  lefties = paste(currTime, "no Matchings",".xlsx",sep = " ")
  write.xlsx(tweetdf,file = lefties )
  
  # end function
  # am Anfang evtl alle tweets aussortieren, die mehrere hashtags enthalten
}



# e.g. time = "15:30" startDate = "2019-08-17" matchday ??? [1;lastMatchday] duration in seconds for stream connection
# return data frame containing all tweets as well as creating xlsx file with all tweets

getStreamTweets <- function(duration = 60 ,keywords = "",writeToXlsx = TRUE){
  # some formats regarding file name
  currentTime = format(Sys.time(), format = "%Y-%m-%d %H.%M.%S")
  fileNameJson = paste(currentTime,keywords,".json")
  fileNameXls = paste(currentTime,keywords,".xlsx")
  fileNameRDA = paste(currentTime,keywords,".Rda")
  # /some formats regarding file name
  connectingTweetApi()

  stream_tweets2(
    keywords,
    timeout = duration ,
    file_name = fileNameJson,
    parse = FALSE,
    language = "de"
  )
  # parsing Json output to DataFrame usig parse_stream
  DfTweets <- parse_stream(fileNameJson)
  save(DfTweets,file = fileNameRDA)
  # writing data frame to xlxs file in home directory
  if(writeToXlsx){
  write.xlsx(DfTweets,file = fileNameXls)
  }
  return(DfTweets)
}

# e.g. time = "15:30" startDate = "2019-08-17" matchday ??? [1;lastMatchday] duration in seconds for stream connection
# returns string such as e.g. "BVB,FCA,Werder,f95,SCF,Mainz05,Werkself,Bayer04,paderborn,VfLWolfsburg,Koeln"

getKeywords <- function(DfGamesSub){
  # subsetting data frame DfGamesSub
  text = substring(paste(DfGamesSub$Hashtags.HomeTeam,DfGamesSub$Hashtags.AwayTeam,sep = " ",collapse = " "),2)
  keywords = gsub("#", ",",gsub("[[:space:]]","",text))
  # /subsetting data frame ...
  return(keywords)
}

getSubsettedDf <- function(time = "", startDate = "", endDate = "", competitionID ="2002",matchday =""){
  DfGames = GetDataFootballDataOrg(startDate,endDate,competitionID,matchday) # getting all matches on certain day
  if(time == ""){return(DfGames)}
  else{
    dateTime = as.POSIXct(paste(startDate,time),format="%Y-%M-%d %H:%M") # converting startDate into Posixct format (x sec past 1970)
    # adding DfGames to current environment referred to as DfFootballData
    #assign("DfFootballData",DfGames, envir = .GlobalEnv)
    # subsetting data frame regarding time and concatenating hashtag strings to specific format, 
    DfGamesSub = subset(DfGames,Match_DateTime == dateTime) 
    # /subsetting data frame ...
    return(DfGamesSub)
  }
}

# football data org

####### Getting Data from football-data.org using uri as request url ################################
# start function
GetDataFootballDataOrg <-function(startDate = "", endDate = "", competitionID ="2002",matchday ="",earlyReturn = FALSE){
  ### football-data.org: Only 1. Bundesliga is included in Free Trier ####
  ##### required libraries ######
  # require(glue)
  # require(httr)
  # require(stringr)
  # require(jsonlite)
  # require(openxlsx)
  ##### \required libraries ######
  
  auth_token = "76ab70019471409eac06a5671021cfaf"
  
  ### there is no error handler if only endDate has been set without startDate ###
  if(startDate != ""){
    dateFrom = as.Date(startDate) #you must provide the standard ISO format for date and time! 
    if(endDate != ""){
      dateTo = as.Date(endDate)
    }else{
      dateTo = dateFrom
    }
    # matches within a certein period of time
    uri = glue("http://api.football-data.org/v2/matches?competitions={competitionID}&dateFrom={dateFrom}&dateTo={dateTo}")
    action = paste(c("startDate ", "endDate "),c(dateFrom, dateTo),collapse = " ")
  }  
  else if(matchday != ""){
    # matches on certain matchday 
    uri = glue("http://api.football-data.org/v2/competitions/{competitionID}/matches/?matchday={matchday}") 
    action = paste("matchday ",matchday,collapse = "")
  }
  else{
    # If user hasn´t specified any wishes he will get everything
    # all matches of current season so far
    uri = glue("http://api.football-data.org/v2/competitions/{competitionID}/matches/" )
    action = "get_All_matches"
  }
  
  matches = GET(uri,add_headers("X-Auth-Token"=auth_token))
  warn_for_status(matches)
  matches = content(matches, "raw")
  writeBin(matches,"currentRequest.json")
  
  ######## Extracting and Transforming JSON Input to DataFrame Output #################################
  
  result <- fromJSON(txt = "currentRequest.json", flatten = TRUE, simplifyDataFrame = TRUE)
  n = length(result) #utcDates, hometeams, awayteams always stored at the list entry which returns a data frame
  count = result[[1]] #access to count
  ID = 1 : count
  Hashtags.HomeTeam = vector(mode="character", length=count)
  Hashtags.AwayTeam = vector(mode="character", length=count)
  Match_DateTime = result[[n]]$utcDate #access to utcDates
  Home_Team = result[[n]]$homeTeam.name #access to hometeams
  Away_Team = result[[n]]$awayTeam.name #access to awayteams
  ###### Transforming Match_DateTime into PosixCA Datatype and changing timezone from GMT to CET/CEST ####
  Match_DateTime = as.POSIXct(format(as.POSIXct(Match_DateTime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "GMT") , tz="Europe/Berlin",usetz=TRUE))
  ##### Getting current time from Sys.time() as vector of the same length as data frame #######
  now = rep(Sys.time(), count)
  ##### Transforming it in minutes ##############
  minutesTillStart = as.numeric((Match_DateTime - now), units = "mins")
  ##### Finally, creating data frame with all relevant datas ###########################
  DF = data.frame(ID, Home_Team,Hashtags.HomeTeam, Away_Team,Hashtags.AwayTeam,Match_DateTime,now,minutesTillStart, stringsAsFactors = FALSE)
  ### earlyReturn is used by fct getTeamsofCurrentSeason
  if(earlyReturn){
    return(DF)
  }
  ##### naming the excel files ######
  #currentDate = format(Sys.time(),format = "%Y-%m-%d")
  #name = paste(action,currentDate, collapse = "")
  name = paste(action,".xlsx",collapse = "")
  ############################## ADDING HASHTAGS TO TEAMS ##########################################
  
  x <- read.csv("hashtagTeams.csv", header = TRUE, stringsAsFactors = FALSE, encoding = "ANSI")
  
  
  ###### Adding Hashtags to each of the HomeTeams from Excel csv #######################
  index.homeTeam <- (match(DF$Home_Team,x$Team)) #looking up for hashtags in the hashtag overview called teams_hashtags.csv
  DF$Hashtags.HomeTeam <- x$Hashtags[index.homeTeam] #using the indexvector (and recycling) for getting all hashtags which corresponds to one hometeam
  
  ###### Adding Hashtags to each of the AwayTeams from Excel csv #######################
  index.AwayTeam <- (match(DF$Away_Team,x$Team)) #looking up for hashtags in the hashtag overview called teams_hashtags.csv
  DF$Hashtags.AwayTeam <- x$Hashtags[index.AwayTeam] #using the indexvector (and recycling) for getting all hashtags which corresponds to one Awayteam
  
  #write.xlsx(DF,file = name)
  
  return(DF)
}

getTeamsofCurrentSeason = function(){
  ############################## Identifying all teams taking part in Bundesliga 19/20 #############
  ############################## in order to append hashtags to each team respectively #############
  DF = GetDataFootballDataOrg(earlyReturn = TRUE)
  DF.HomeTeam = DF[2] #extracting the hometeam coloumn to a new dataframe DF.HomeTeam
  DF.HomeTeam = unique(DF.HomeTeam) #getting rid of duplicates
  
  write.xlsx(DF.HomeTeam,file = "UniqueTeams.xlsx")
  
}

todayGames = function(today = format(Sys.time(),format = "%Y-%m-%d")){
  todaysGames = GetDataFootballDataOrg(startDate = today)
  return(todaysGames)
}

gettingStreamTag = function(startDateTime = ""){
  d = format(startDateTime,"%Y-%m-%d") #transforming "2019-08-25 15:30:00 CEST" into "2019-08-25"
  t = startDateTime
  todaysGames = GetDataFootballDataOrg(startDate = d)
  #startTimes = unique(todaysGames$Match_DateTime)
  #length(startTimes)
  sub.Games = subset(todaysGames,todaysGames$Match_DateTime == t,select = c(Hashtags.HomeTeam,Hashtags.AwayTeam))
  v = as.vector(apply(sub.Games,1,paste)) # Vector of Strings
  p = paste(v,sep = " ", collapse = " ") # One big string
  q = str_sub(p,start = 2) # ersten # entfernen
  r = str_squish(q) # doppelte Leerzeichen entfernen
  s = str_replace_all(r," ", "") #removing all whitespaces
  streamTags = str_replace_all(s,"#","," ) #swapping # with ,
  u = sub(" .*","",sub.Games$Hashtags.HomeTeam)
  v = sub(" .*","",sub.Games$Hashtags.AwayTeam)
  w = paste(u,v,sep = " ")
  y = str_replace_all(w,"#","" )
  #matchTags = str_replace_all(paste("#",y)," ","")
  streamMatchTags = str_replace_all(y," ","") #removing all whitespaces
  streamMatchTags = paste(streamMatchTags,collapse = ",") #making one big string seperated by ","
  # Adding matchTags to rest of tags
  finalString = paste(streamMatchTags,streamTags,sep = ",")
  finalString = str_replace_all(finalString," ","")
  return(c(finalString,streamMatchTags))
}

# end function

# # e.g. time = "15:30" startDate = "2019-08-17" matchday ??? [1;lastMatchday] duration in seconds
# getStreamTweets <- function(duration = 60 ,time = "", startDate = "", endDate = "", competitionID ="2002",matchday =""){
#   dateTime = as.POSIXct(paste(startDate,time),format="%Y-%M-%d %H:%M")
#   DfGames = GetDataFootballDataOrg(startDate,endDate,competitionID,matchday)
#   DfGamesSub = subset(DfGames,Match_DateTime == dateTime, select = c(Hashtags.HomeTeam, Hashtags.AwayTeam))
#   text = substring(paste(DfGamesSub$Hashtags.HomeTeam,DfGamesSub$Hashtags.AwayTeam,sep = " ",collapse = " "),2)
#   keywords = gsub("#", ",",gsub("[[:space:]]","",text))
#   currentTime = format(Sys.time(), format = "%Y-%m-%d %H.%M.%S")
#   fileNameJson = paste(currentTime,keywords,".json")
#   fileNameXls = paste(currentTime,keywords,".xlsx")
#   connectingTweetApi()
#   stream_tweets(
#     keywords,
#     timeout = duration ,
#     file_name = fileNameJson,
#     parse = FALSE,
#     language = "de"
#   )
#   # parsing Json output to DataFrame usig parse_stream
#   DfTweets <- parse_stream(fileNameJson)
#   write.xlsx(djt,file = fileNameXls)
#   return(DfTweets)
# }
grabbingStream = function(){
  #source("functions.R")
  
  today = format(Sys.Date(),"%Y-%m-%d")
  
  lang = "de"
  duration = 60*60*2
  
  connectingTweetApi()
  
  df = todayGames(today = today)
  
  startTimes = unique(df$Match_DateTime) #getting match date
  #startTimes[1]
  #######for each d in startTimes
  for(d in 1:length(startTimes)){
    
    streamTagsFilenames = gettingStreamTag(startDate = startTimes[d]) #vector of streamNames[1] and Filenames[2]
    #streamTagsFilenames
    
    #filenames
    #streamTagsFilenames[2]
    t = format(startTimes[d], format = "%Y-%m-%d %H.%M.%S")
    filenNameJson = paste(t,streamTagsFilenames[2],".json")
    filenNameRDA = paste(t,streamTagsFilenames[2],".Rda")
    
    #warten bis 60 minuten vor spielbeginn
    #sys.sleep() in seconds
    secondsTowait = 60*60
    waitingTime = difftime(startTimes[d],Sys.time(),units = "sec") - secondsTowait
    if(waitingTime > 0){
        while(waitingTime > 60){
          waitingTime = difftime(startTimes[d],Sys.time(),units = "sec") - secondsTowait
          print(paste("Wait for minutes: ",waitingTime/60))
          Sys.sleep(30)
        }
    
        print(paste("Wating Time: ",waitingTime))
        Sys.sleep(waitingTime)
    
        #start streaming tweets
        stream_tweets(
          q = streamTagsFilenames[1],
          parse = FALSE,
          timeout = duration,
          language = lang,
          file_name = filenNameJson
        )
        ## Parse from json file
        rt <- parse_stream(filenNameJson)
        # save tweets as Rda
        save(rt,file = filenNameRDA)
    }else{
          #do Nothing
    }
    
  }
  
}

######################### Twitter Grabber for looking backwards in time ##############
getFootballTweets = function(startdate = "2019-09-01"){
  df = todayGames(today = startdate)
  startTimes = unique(df$Match_DateTime) #getting match dates
  
  for (d in startTimes){
    
    sub.df = subset(df,df$Match_DateTime == d,select = c(Hashtags.HomeTeam,Hashtags.AwayTeam))
    #adding combined hashtags like #s04fcb...
    u = sub(" .*","",sub.df$Hashtags.HomeTeam) #extracting the first hashtag of hometeam vector
    v = sub(" .*","",sub.df$Hashtags.AwayTeam) #extracting the first hashtag of awayteam vector
    w = paste(u,v,sep = " ")
    y = str_replace_all(w,"#","" )
    matchTags = str_replace_all(paste("#",y)," ","")
    a = data.frame(matchTags,matchTags)
    colnames(a) = c("Hashtags.HomeTeam","Hashtags.AwayTeam")
    sub.df = rbind(sub.df,a)
    
    for(i in 1:length(sub.df$Hashtags.HomeTeam)){
      
      grabbingTweets(hashtagFirstTeam = sub.df[i,1],hashtagSecondTeam = sub.df[i,2],zeit = as.POSIXct(d, origin = "1970-01-01"))
      
    }
    
    
  }
  
  
}
