library(tidyverse)
library(stringr)
library(tidytext)
library(wordcloud)
library(readr)

load('total_tweets4.Rda')

#Load twitter lexicon
lexicon <- read_csv("~/R Projects/NBA Project/twitter_lexicon1.csv")

#Remove all handles
total$text <- str_replace_all(total$text, "@\\w+", "")

#Remove 'RT'
total$text <- str_replace_all(total$text, "RT", "")

#Removing Emojis
#total$text <- str_replace_all(total$text, '\\<[A-z 0-9 \\+]+\\>', '')

#Remove URLs
total$text <- str_replace(total$text, 'https\\S+\\s*', '') 

#Remove Ampers
total$text <- str_replace(total$text, '&amp;', '')

word <- unnest_tokens(total, word, text) %>%
    anti_join(stop_words)

word %>%
    count(word, sort = TRUE) %>%
    with(wordcloud(word, n, max.words = 100))

sentiment <- word %>%
    inner_join(lexicon)

twitter_sentiment <- sentiment %>% group_by(screen_name, created_at, 
                                            Player, Pos, Tm, G, GS, MP, WS) %>%
    summarise(sent = mean(sentiment))

save(twitter_sentiment, file='twitter_sentiment.Rda')

twitter_sentiment %>% group_by(Player) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

twitter_sentiment %>% filter(Player == 'Spencer Dinwiddie') %>%
    ggplot(aes(x=created_at, y = sent)) +
        geom_point()