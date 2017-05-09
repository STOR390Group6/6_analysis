library(tidyverse)
library(rvest)
library(readxl)
library(stringr)
library(xlsx)
players <- read_excel("~/R Projects/NBA Project/NBA Dataset.xlsx")
players$twitter <- NA

for(i in 1:nrow(players)){
    #retrieve html from dataframe
    html <- read_html(players$URLs[i])
    
    #retrieve twitter handle if there is one and place it in dataframe
    twitter <- html %>%
        html_nodes("#meta a+ a") %>%
        html_text
    
    players$twitter[i] <- twitter[1]
}

#Get rid of values in a dataframe that only contain numbers
players$twitter[!str_detect(players$twitter, '\\@')] <- NA

write.table(players, file = 'players.csv', row.names=FALSE)
