#1# Set your working directory where the output should be stored as well as where the function can find the .csv file
### hashtagTeams.csv containing all teams and all corresponding hashtags

setwd("C:/Users/Michael Hechtbauer/Desktop/Data Mining Github")

#2# Load all Packages/functions needed for mining Twitter

source("Functions.r")

#3# If you want to harvest Tweets for past games played on a specific date: getFootballTweets()

getFootballTweets("2019-09-14")

#4# If you want to stream Tweets for 2 hours starting 60 minutes before the match starts: grabbingStream()
### grabbingStream() once started it will automatically wait until 60 minutes before the next scheduled match is gonna start.

grabbingStream()


