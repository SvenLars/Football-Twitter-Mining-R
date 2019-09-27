########################################
#### Twitter Football Data Retrieval ###
########################################

### Required Packages

# install.packages("glue")
# install.packages("httr")
# install.packages("stringr")
# install.packages("jsonlite")
# install.packages("openxlsx")
# install.packages("rtweet")
# install.packages("readr") # for getting better performance when reading large twitter .json files
# install.packages("taskscheduleR")

### Checking if required packages installed

require(glue)
require(httr)
require(stringr)
require(jsonlite)
require(openxlsx)
require(rtweet)
require(readr)
require(taskscheduleR)

#########################################            PART 1: Functions used for streaming tweets            #########################################

#0# Helper Functions

#1# Connecting to Twitter Restful API and getting oauth token

connectingTweetApiS <- function(){
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

#2# todayGames() : refer to section "Global helper Functions": Providing a date like "2019-08-17" you will get all matches at this date

#3# Creating Team Hashtags like "FCBBVB" when FCB plays against BVB on this day

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

#0# Main Function

grabbingStream = function(){
  
  today = format(Sys.Date(),"%Y-%m-%d")
  
  lang = "de"
  
  # setting stream duration in seconds
  duration = 60*60*2
  
  connectingTweetApiS()
  
  df = todayGames(today = today)
  
  # getting match dates
  startTimes = unique(df$Match_DateTime) 
  
  # For each d in startTimes
  
  for(d in 1:length(startTimes)){
    
	# Vector of streamNames[1] and Filenames[2]
    streamTagsFilenames = gettingStreamTag(startDate = startTimes[d]) 
    
    
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

#########################################            PART 2: Functions used for getting past tweets            #########################################
### Annotations

#1# Twitter rate limits cap the number of search results returned to 18,000 every 15 minutes. 
#2# To request more than that, simply set retryonratelimit = TRUE and rtweet will wait for rate limit resets for you.
#3# Once can go back only for 6 to 9 days

#0# Helper Functions

#1# Connecting to Twitter Restful API and getting oauth token using a different account called Peter2nd

connectingTweetApi <- function(){
  if(require(rtweet)){
    ##Setting up keys for accesing twitter
    AccessToken <- "1167337836955222016-p3YL1FIssTuBSl0Lgv0nE5AJmuoTTu"
    AccessTokenSecret <- "oMJbNBjCQXPZDD4Q4eNVytaIndDgrBE3Xf9oFZ9bjGD7z"
    ApiSecretKey <- "yRt1jmw13rcjeqmfJHd5hm8CJcn47gsglGIQreSW2kfxx3ehYe"
    ApiKey <- "KSUEsvIi5Vgj6MwkQmQrWpppl"
    app <- "Peter2nd"
    create_token(app = app, consumer_key = ApiKey, consumer_secret = ApiSecretKey,access_token = AccessToken, access_secret = AccessTokenSecret)
  }else{library("rtweet")}
}

#2# Returning a DataFrame Object called "containerTweets" containing all tweets matching the input hashtags

getTweets <- function(hashtagFirstTeam,hashtagSecondTeam,nTweets = 18000, lang = "de" ,retweets = FALSE,retryonratelimit = TRUE){
  # hashtags <- paste(hashtagFirstTeam,hashtagSecondTeam, sep = " OR ",collapse = " OR ") doesnÂ´t work with string of hashtags
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

#3# Sorting DataFrame with respect to hashtag vector

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
    #sortedTeam = testDF[str_detect(tolower(dataframe$hashtags),tolower(searchkeys)),]
    sortedTeam = dataframe[str_detect(tolower(dataframe$hashtags),tolower(searchkeys)),]
    
    
    store[[index]] = sortedTeam
    index = index + 1
  }
  return(store)
}

#4# Writing sorted Tweets to file as .RDA as well as .XLSX

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

#5# Merging functions to automize getting and writting tweets

grabbingTweets<-function(hashtagFirstTeam,hashtagSecondTeam,nTweets = 100, lang = "de" ,retweets = FALSE,retryonratelimit = TRUE,zeit){
  
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

#0# Main Function

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

### Global Helper Functions

#1# Getting Data from football-data.org using uri as request url
### Football-data.org: Only 1. Bundesliga is included in Free Trier

GetDataFootballDataOrg <-function(startDate = "", endDate = "", competitionID ="2002",matchday ="",earlyReturn = FALSE){

  auth_token = "76ab70019471409eac06a5671021cfaf"
  
  ### There is no error handler if only endDate has been set without startDate ###
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
  #print(paste("matches:",matches))
  warn_for_status(matches)
  matches = content(matches, "raw")
  writeBin(matches,"currentRequest.json")
  
  ######## Extracting and Transforming JSON Input to DataFrame Output #################################
  
  result <- fromJSON(txt = "currentRequest.json", flatten = TRUE, simplifyDataFrame = TRUE)
  n = length(result) #utcDates, hometeams, awayteams always stored at the list entry which returns a data frame
  count = result[[1]] #access to count
  
  # if the requested matchday is empty - nobody is playing - return TRUE
  if(count == 0){
    return(NULL)
  }
  # print(paste("count: ", count))
  # print(paste("result: ", result))
  
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

#2# IF the new season has started you can find out which teams are participating by using following functions
### which returns a excel file with all teams
  # Identifying all teams taking part in Bundesliga 19/20
  # in order to append hashtags to each team respectively 
  
getTeamsofCurrentSeason = function(){

  DF = GetDataFootballDataOrg(earlyReturn = TRUE)
  DF.HomeTeam = DF[2] #extracting the hometeam coloumn to a new dataframe DF.HomeTeam
  DF.HomeTeam = unique(DF.HomeTeam) #getting rid of duplicates
  
  write.xlsx(DF.HomeTeam,file = "UniqueTeams.xlsx")
  
}

#3# Providing a date like "2019-08-17" you will get all matches at this date

todayGames = function(today = format(Sys.time(),format = "%Y-%m-%d")){
  todaysGames = GetDataFootballDataOrg(startDate = today)
  n = 1
  while(is.null(todaysGames)){
  
	# if there aren´t any matches today (is.null(todaygames)) then try the next day 
	today = format(Sys.time()+24*60*60*n,format = "%Y-%m-%d")
    Sys.sleep(10)
    todaysGames = GetDataFootballDataOrg(startDate = today)
    n = n + 1
  }
  return(todaysGames)
}
