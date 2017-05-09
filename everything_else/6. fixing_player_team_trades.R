library(tidyverse)
library(stringr)
library(tidytext)
library(wordcloud)
library(readr)
source('helper.R')

load('total_tweets3.Rda')

test <- filter(total, Tm == 'TOT')
names <- unique(test$screen_name)

total<- total[!((total$screen_name %in% names) & !(total$Tm == 'TOT')),]

total$Tm[(total$screen_name == 'QuincyAcy') & (total$created_at < as.Date('2017-01-09'))] = 'DAL'
total$Tm[(total$screen_name == 'QuincyAcy') & (total$created_at > as.Date('2017-01-09'))] = 'BRK'

total$Tm[(total$screen_name == 'JusAnderson1') & (total$created_at < as.Date('2017-02-23'))] = 'DAL'
total$Tm[(total$screen_name == 'JusAnderson1') & (total$created_at > as.Date('2017-02-23'))] = 'PHI'

total$Tm[(total$screen_name == 'Matt_Barnes22') & (total$created_at < as.Date('2017-03-01'))] = 'SAC'
total$Tm[(total$screen_name == 'Matt_Barnes22') & (total$created_at > as.Date('2017-03-01'))] = 'GSW'

total$Tm[(total$screen_name == '44Bojan') & (total$created_at < as.Date('2017-02-23'))] = 'BRK'
total$Tm[(total$screen_name == '44Bojan') & (total$created_at > as.Date('2017-02-23'))] = 'WAS'

total$Tm[(total$screen_name == 'andrewbogut') & (total$created_at < as.Date('2017-02-23'))] = 'DAL'
total$Tm[(total$screen_name == 'andrewbogut') & ((total$created_at > as.Date('2017-02-23')) & 
                                                     (total$created_at < as.Date('2017-03-02')))] = 'PHI'
total$Tm[(total$screen_name == 'andrewbogut') & (total$created_at > as.Date('2017-03-02'))] = 'CLE'

total$Tm[(total$screen_name == 'TheCoreyBrewer') & (total$created_at < as.Date('2017-02-23'))] = 'HOU'
total$Tm[(total$screen_name == 'TheCoreyBrewer') & (total$created_at > as.Date('2017-02-23'))] = 'LAL'

total$Tm[(total$screen_name == 'TwoShotOne') & (total$created_at < as.Date('2017-01-21'))] = 'NOP'
total$Tm[(total$screen_name == 'TwoShotOne') & (total$created_at > as.Date('2017-01-21'))] = 'ORL'

total$Tm[(total$screen_name == 'JmCalderon') & (total$created_at < as.Date('2017-03-04'))] = 'LAL'
total$Tm[(total$screen_name == 'JmCalderon') & (total$created_at > as.Date('2017-03-04'))] = 'ATL'

total$Tm[(total$screen_name == 'Casspi18') & (total$created_at < as.Date('2017-02-23'))] = 'SAC'
total$Tm[(total$screen_name == 'Casspi18') & ((total$created_at > as.Date('2017-02-23')) & 
                                                     (total$created_at < as.Date('2017-03-23')))] = 'NOP'
total$Tm[(total$screen_name == 'Casspi18') & (total$created_at > as.Date('2017-03-23'))] = 'MIN'

total$Tm[(total$screen_name == 'QCook323') & (total$created_at < as.Date('2017-03-18'))] = 'DAL'
total$Tm[(total$screen_name == 'QCook323') & (total$created_at > as.Date('2017-03-18'))] = 'NOP'

total$Tm[(total$screen_name == 'boogiecousins') & (total$created_at < as.Date('2017-02-23'))] = 'SAC'
total$Tm[(total$screen_name == 'boogiecousins') & (total$created_at > as.Date('2017-02-23'))] = 'NOP'

total$Tm[(total$screen_name == 'TyrekeEvans') & (total$created_at < as.Date('2017-02-22'))] = 'NOP'
total$Tm[(total$screen_name == 'TyrekeEvans') & (total$created_at > as.Date('2017-02-22'))] = 'SAC'

total$Tm[(total$screen_name == 'YogiFerrell11') & (total$created_at < as.Date('2017-01-28'))] = 'BRK'
total$Tm[(total$screen_name == 'YogiFerrell11') & (total$created_at > as.Date('2017-01-28'))] = 'DAL'

total$Tm[(total$screen_name == 'LangGalloway10') & (total$created_at < as.Date('2017-02-22'))] = 'NOP'
total$Tm[(total$screen_name == 'LangGalloway10') & (total$created_at > as.Date('2017-02-22'))] = 'SAC'

total$Tm[(total$screen_name == 'TajGibson22') & (total$created_at < as.Date('2017-02-22'))] = 'CHI'
total$Tm[(total$screen_name == 'TajGibson22') & (total$created_at > as.Date('2017-02-22'))] = 'OKC'

total$Tm[(total$screen_name == 'A1Laflare10') & (total$created_at < as.Date('2017-03-15'))] = 'NOP'
total$Tm[(total$screen_name == 'A1Laflare10') & (total$created_at > as.Date('2017-03-15'))] = 'BRK'

total$Tm[(total$screen_name == 'JeramiGrant') & (total$created_at < as.Date('2016-11-01'))] = 'PHI'
total$Tm[(total$screen_name == 'JeramiGrant') & (total$created_at > as.Date('2016-11-01'))] = 'OKC'

total$Tm[(total$screen_name == 'spencerhawes00') & (total$created_at < as.Date('2017-02-02'))] = 'CHO'
total$Tm[(total$screen_name == 'spencerhawes00') & (total$created_at > as.Date('2017-02-02'))] = 'MIL'

total$Tm[(total$screen_name == 'buddyhield') & (total$created_at < as.Date('2017-02-22'))] = 'NOP'
total$Tm[(total$screen_name == 'buddyhield') & (total$created_at > as.Date('2017-02-22'))] = 'SAC'

total$Tm[(total$screen_name == 'sergeibaka_7') & (total$created_at < as.Date('2017-02-14'))] = 'ORL'
total$Tm[(total$screen_name == 'sergeibaka_7') & (total$created_at > as.Date('2017-02-14'))] = 'TOR'

total$Tm[(total$screen_name == 'brandonjennings') & (total$created_at < as.Date('2017-02-26'))] = 'NYK'
total$Tm[(total$screen_name == 'brandonjennings') & (total$created_at > as.Date('2017-02-26'))] = 'WAS'

total$Tm[(total$screen_name == 'TerrenceJones1') & (total$created_at < as.Date('2017-03-03'))] = 'NOP'
total$Tm[(total$screen_name == 'TerrenceJones1') & (total$created_at > as.Date('2017-03-03'))] = 'MIL'

total$Tm[(total$screen_name == 'KyleKorver') & (total$created_at < as.Date('2017-01-07'))] = 'ATL'
total$Tm[(total$screen_name == 'KyleKorver') & (total$created_at > as.Date('2017-01-07'))] = 'CLE'

total$Tm[(total$screen_name == '1JOLOLO') & (total$created_at < as.Date('2017-02-23'))] = 'OKC'
total$Tm[(total$screen_name == '1JOLOLO') & (total$created_at > as.Date('2017-02-23'))] = 'CHI'

total$Tm[(total$screen_name == 'briskuno') & (total$created_at < as.Date('2017-02-23'))] = 'BRK'
total$Tm[(total$screen_name == 'briskuno') & (total$created_at > as.Date('2017-02-23'))] = 'WAS'

total$Tm[(total$screen_name == 'KJMcDaniels') & (total$created_at < as.Date('2017-02-24'))] = 'HOU'
total$Tm[(total$screen_name == 'KJMcDaniels') & (total$created_at > as.Date('2017-02-24'))] = 'BRK'

total$Tm[(total$screen_name == 'dougmcdermott') & (total$created_at < as.Date('2017-02-23'))] = 'CHI'
total$Tm[(total$screen_name == 'dougmcdermott') & (total$created_at > as.Date('2017-02-23'))] = 'OKC'

total$Tm[(total$screen_name == 'MrAnthonyMorrow') & (total$created_at < as.Date('2017-02-23'))] = 'OKC'
total$Tm[(total$screen_name == 'MrAnthonyMorrow') & (total$created_at > as.Date('2017-02-23'))] = 'CHI'

total$Tm[(total$screen_name == 'nicholaf44') & (total$created_at < as.Date('2017-02-23'))] = 'WAS'
total$Tm[(total$screen_name == 'nicholaf44') & (total$created_at > as.Date('2017-02-23'))] = 'BRK'

total$Tm[(total$screen_name == 'NerlensNoel3') & (total$created_at < as.Date('2017-02-23'))] = 'PHI'
total$Tm[(total$screen_name == 'NerlensNoel3') & (total$created_at > as.Date('2017-02-23'))] = 'DAL'

total$Tm[(total$screen_name == 'bosnianbeast27') & (total$created_at < as.Date('2017-02-12'))] = 'DEN'
total$Tm[(total$screen_name == 'bosnianbeast27') & (total$created_at > as.Date('2017-02-12'))] = 'POR'

total$Tm[(total$screen_name == 'campayne') & (total$created_at < as.Date('2017-02-23'))] = 'OKC'
total$Tm[(total$screen_name == 'campayne') & (total$created_at > as.Date('2017-02-23'))] = 'CHI'

total$Tm[(total$screen_name == 'masonplumlee') & (total$created_at < as.Date('2017-02-12'))] = 'POR'
total$Tm[(total$screen_name == 'masonplumlee') & (total$created_at > as.Date('2017-02-12'))] = 'DEN'

total$Tm[(total$screen_name == 'masonplumlee') & (total$created_at < as.Date('2017-02-23'))] = 'PHI'
total$Tm[(total$screen_name == 'masonplumlee') & (total$created_at > as.Date('2017-02-23'))] = 'NYK'

total$Tm[(total$screen_name == 'ChassonRandle') & (total$created_at < as.Date('2017-02-23'))] = 'PHI'
total$Tm[(total$screen_name == 'ChassonRandle') & (total$created_at > as.Date('2017-02-23'))] = 'NYK'

total$Tm[(total$screen_name == 'T_DotFlight31') & (total$created_at < as.Date('2017-02-14'))] = 'TOR'
total$Tm[(total$screen_name == 'T_DotFlight31') & (total$created_at > as.Date('2017-02-14'))] = 'ORL'

total$Tm[(total$screen_name == 'WayneSeldenJr') & (total$created_at < as.Date('2017-03-18'))] = 'NOP'
total$Tm[(total$screen_name == 'WayneSeldenJr') & (total$created_at > as.Date('2017-03-18'))] = 'MEM'

total$Tm[(total$screen_name == 'StephensonLance') & (total$created_at < as.Date('2017-02-07'))] = 'NOP'
total$Tm[(total$screen_name == 'StephensonLance') & ((total$created_at > as.Date('2017-02-07')) & 
                                                     (total$created_at < as.Date('2017-03-18')))] = 'MIN'
total$Tm[(total$screen_name == 'StephensonLance') & (total$created_at > as.Date('2017-03-18'))] = 'IND'

total$Tm[(total$screen_name == 'DeronWilliams') & (total$created_at < as.Date('2017-02-16'))] = 'DAL'
total$Tm[(total$screen_name == 'DeronWilliams') & (total$created_at > as.Date('2017-02-16'))] = 'CLE'

total$Tm[(total$screen_name == 'DWXXIII') & (total$created_at < as.Date('2017-02-08'))] = 'MIA'
total$Tm[(total$screen_name == 'DWXXIII') & (total$created_at > as.Date('2017-02-08'))] = 'CLE'

total$Tm[(total$screen_name == 'TeamLou23') & (total$created_at < as.Date('2017-02-22'))] = 'LAL'
total$Tm[(total$screen_name == 'TeamLou23') & (total$created_at > as.Date('2017-02-22'))] = 'HOU'

total$Tm[(total$screen_name == 'troywilliams_') & (total$created_at < as.Date('2017-03-09'))] = 'MEM'
total$Tm[(total$screen_name == 'troywilliams_') & (total$created_at > as.Date('2017-03-09'))] = 'HOU'

save(total, file='totaltweets4.Rda')