library(tidyverse)
library(stringr)
library(rvest)
library(readr)

#Read CSV
players <- read_csv("~/R Projects/NBA Project/players.csv")

#Filter players without twitter
players <- filter(players, !is.na(twitter))

#Filter duplicate twitter accounts
players <- players[!duplicated(players$twitter),] 


#Create empty game log dataframe
gamelogs <- tibble(
    name = character(),
    position = character(),
    date = character(),
    current = character(),
    opponent = character(),
    mp = character(),
    gsc = numeric(),
    pm = numeric()
)

#Scrape each player's game log
for(j in 49:length(players$URLs)){
    #Get game log URLS
    url <- paste0(str_replace(players$URLs[j], '.html', ''), '/gamelog/2017')
    
    #Read in html
    html <- read_html(url)
    
    #Read in info
    info <- html %>%
        html_nodes('.right:nth-child(29) , #pgl_basic_playoffs\\.1 a , th , #pgl_basic a , .right:nth-child(10) , .right:nth-child(30)') %>%
        html_text()
    
    #Creat empty vectors to fill in
    name <- character() 
    position <- character()
    date <- character()
    current <- character()
    opponent <- character()
    mp <- character()
    gsc <- numeric()
    pm <- numeric()
    
    #Parse info into various numbers
    for(i in 1:length(info)){
        
        #Look for the date as an indicator of the start of a game
        if(str_detect(info[i], "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}")){
            name = c(name, players$Player[j])
            position = c(position, players$Pos[j])
            date = c(date, info[i])
            current = c(current, info[i+1])
            opponent = c(opponent, info[i+2])
            
            #If the third item after the date is an indicator for minutes, then the player played, if not, he didn't
            if(is.na(info[i+3])){
                mp = c(mp, NA)
                gsc = c(gsc, NA)
                pm = c(pm, NA)                
            } else if((str_detect(info[i+3], '[0-9]+\\:[0-9]+'))){
                mp = c(mp, info[i+3])
                gsc = c(gsc, info[i+4])
                pm = c(pm, info[i+5])
            } else {
                mp = c(mp, NA)
                gsc = c(gsc, NA)
                pm = c(pm, NA)
            }
        }
    }
    gamelogs <- rbind(gamelogs, data.frame(name, position, date, current, opponent, mp, gsc, pm))
    Sys.sleep(30)
}

save(gamelogs, file='gamelogs.Rda')