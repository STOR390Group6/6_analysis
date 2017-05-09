library(tidyverse)
library(readxl)
library(stringr)
teams <- read_excel("~/Documents/STOR Classes/STOR 390/stor390_nba-master/stor390_nba-master/teams.xlsx")
load("~/Documents/STOR Classes/STOR 390/stor390_nba-master/stor390_nba-master/gamelogs.Rda")
load("~/Documents/STOR Classes/STOR 390/stor390_nba-master/stor390_nba-master/twitter_sentiment.Rda")

gamelogs <- gamelogs %>%
  rename(player = name, team = current, minutes= mp, game_score = gsc, plus_minus = pm) %>%
  na.omit()
gamelogs$date <- as.POSIXct(gamelogs$date)

twitter_sentiment <- twitter_sentiment %>%
  rename(player = Player, position = Pos, team = Tm)

teams$date <- as.Date(teams$date, format = "%a, %b %d, %Y") 

#twitter_sentiment <- separate(twitter_sentiment, col = created_at, into = c('date', 'time'), sep = " ") 

#twitter_sentiment <- filter(twitter_sentiment, G > 41) # played half of the season

teams <- separate(teams, col = streak, into = c('streak_type', 'streak_amt'), sep = " ") 
teams <- teams %>%
  mutate(result_bin = if_else(result == 'W', 1, -1)) %>%
  mutate(win_pct = current_wins/(current_wins + current_losses))
#normalizing game score
gamelogs$game_score <- as.numeric(gamelogs$game_score)
gamelogs <- gamelogs %>%
  mutate(normalized_gamescore = (game_score-mean(gamelogs$game_score))/sd(gamelogs$game_score))

#normalizing sentiment
twitter_sentiment <- twitter_sentiment %>%
  mutate(normalized_sent = (sent-mean(twitter_sentiment$sent))/sd(twitter_sentiment$sent))

tweet_counts <- twitter_sentiment %>% separate(twitter_sentiment, col = created_at, into = c('date', 'time'), sep = " ") %>%
  group_by(date) %>%
  summarise(total = n(), mean_sent = mean(sent)) %>%
  arrange(desc(total))  #besides 3/14,pi day (also steph curry's birthday so maybe players reached out?), the days with the most amount of tweets are before March. This makes sense because March is when teams lock in and really push for the playoffs more than usual.


#total tweets throughout the season
ggplot(tweet_counts) +
  geom_point(aes(x=date, y=total))+
  geom_smooth(aes(x=date, y=total))

#mean sentiment throughout the season
ggplot(tweet_counts) +
  geom_point(aes(x=date, y=mean_sent)) +
  geom_smooth(aes(x=date, y=mean_sent), method = lm)

#who tweets out the most positivity
player_tweet_count <-twitter_sentiment %>% group_by(player) %>%
  summarise(count =n(), mean = mean(sent)) %>%
  arrange(desc(mean))

#who tweets out the least positivity
twitter_sentiment %>% group_by(player) %>%
  summarise(count =n(), mean = mean(sent)) %>%
  arrange(mean)

#who tweets the most
twitter_sentiment %>% group_by(player) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#player tweet sentiment vs gamelogs sorted by team
twitter_sentiment %>% filter(team == 'OKC') %>%
  ggplot() +
  geom_point(aes(x=created_at, y = normalized_sent, color='blue')) +
  geom_smooth(aes(x=created_at, y = normalized_sent, color='blue'))+
  geom_point(data=filter(gamelogs, team=='OKC'), (aes(x=date, y=normalized_gamescore, color='red')))+
  geom_smooth(data=filter(gamelogs, team=='OKC'), (aes(x=date, y=normalized_gamescore, color='red')))+
scale_color_manual(labels = c("Player Sent", "Player Game Score"), values = c("blue", "red"))


twitter_sentiment %>% filter(Player=='Stephen Curry') %>%
  ggplot(aes(x=created_at, y = sent)) +
  geom_point() 


#top players sentiment over time
top_player_sent <- twitter_sentiment %>%
  filter(WS > 10)
ggplot(top_player_sent)+
  geom_point(aes(x=created_at, y= sent))+
  geom_smooth(aes(x=created_at, y= sent), method=lm)


#team average sentiment over time
team_sent <- twitter_sentiment %>% 
  separate(col = created_at, into = c('date', 'time'), sep = " ") %>%
  group_by(team, date) %>%
  summarise(avg= mean(sent))

#team sentiment and win % over time
team_sent %>% filter(team=="CLE") %>%
  ggplot()+
  geom_point(aes(x=as.POSIXct(date), y=avg, color='blue'))+
  geom_point(data=filter(teams,team=="CLE"), aes(x=as.POSIXct(date), y=win_pct, color='red'))+
  geom_smooth(data=team_sent, aes(x=as.POSIXct(date), y=avg,  color='blue')) +
  scale_color_manual(labels = c("Team Sent", "Team Win%"), values = c("blue", "red"))


#player sent vs team avg sent
twitter_sentiment %>% filter(team == 'OKC') %>%
  ggplot() +
  geom_point(aes(x=created_at, y = sent, color='blue')) +
  geom_smooth(aes(x=created_at, y = sent, color='blue'))+
  geom_point(data=filter(team_sent, team=='OKC'), (aes(x=as.POSIXct(date), y=avg, color='red')))+
  geom_smooth(data=filter(team_sent, team=='OKC'), (aes(x=as.POSIXct(date), y=avg, color='red'))) +
  scale_color_manual(labels = c("Player Sent", "Team Sent"), values = c("blue", "red"))



