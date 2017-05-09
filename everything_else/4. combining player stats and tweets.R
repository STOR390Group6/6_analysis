library(tidyverse)
library(readr)

load('total_tweets.Rda')
players <- read_csv("~/R Projects/NBA Project/players.csv")

players <- select(players, Player, Pos, Tm, G, GS, MP, MP/G, WS, twitter)

total <- left_join(total, players, c('screen_name' = 'twitter' ))

save(total, file='total_tweets2.Rda')