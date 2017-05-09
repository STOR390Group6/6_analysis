library(tidyverse)
library(rtweet)
library(readr)
library(tm)
library(lubridate)
library(sentiment)

#Read CSV
players <- read_csv("~/R Projects/NBA Project/players.csv")

#Filter players without twitter
players <- filter(players, !is.na(twitter))

#Filter duplicate twitter accounts
players <- players[!duplicated(players$twitter),] 

#Setting Twitter Token
consumer_key <- '##########'
consumer_secret <- '##########'
appname <- '390project'

access_token <- '##########'
access_secret <-  '##########'

twitter_token <- create_token(app = appname,
                            consumer_key = consumer_key,
                            consumer_secret = consumer_secret)


####messing around####
qacy <- get_timeline(players$twitter[1],n = 2000)

qacy <- select(qacy, screen_name, created_at, text)

total <- filter(qacy, (created_at >= as.POSIXct('2016-10-24 00:00')) &
                   (created_at <= as.POSIXct('2017-04-13 00:00')))


###Get Tweets of all players###
for(i in 364:dim(players)[1]){
    current <- get_timeline(players$twitter[i],n = 3000) %>%
        select(screen_name, created_at, text) %>%
        filter((created_at >= as.POSIXct('2016-10-24 00:00')) &
                       (created_at <= as.POSIXct('2017-04-13 00:00')))
    
    if(dim(current)[1] >0){
    total <- rbind(total, current)
    }
}
    

    
current <- get_timeline(players$twitter[364],n = 3000)
current <- get_timeline('Teague0', n = 2000)
holder <- cbind.data.frame(current[1], current[3], current[5])
holder <- filter(holder, (created_at >= as.POSIXct('2016-10-24 00:00')) &
                       (created_at <= as.POSIXct('2017-04-13 00:00')))
total <- rbind(total, holder)


save(total, file='total_tweets.Rda')