library(rvest)
library(tidyverse)
library(xlsx)

#List of teams
team_list <- c("TOR", "NYK", "PHI", "BRK", "WAS", "ATL", "MIA", "CHO", 
               "ORL", "CLE", "MIL", "IND", "CHI", "DET", "UTA", "OKC", 
               "POR", "DEN", "MIN", "SAS", "HOU", "MEM", "NOP", "DAL", 
               "GSW", "LAC", "SAC", "LAL", "PHO")

###################
#Set up first team#
###################


bos <- "http://www.basketball-reference.com/teams/BOS/2017_games.html" #Get URL
bos_html <- read_html(bos) #Read HTML

#Parse HTML into List
bos_schedule <- bos_html %>%
    html_nodes('#games td:nth-child(13) , #games td:nth-child(12) , 
               #games .center~ .right+ .center , #games .left a , 
               #games td:nth-child(6) , #games .center+ td.left , 
               #games .left+ td.center , #games .right') %>%
    html_text()

#Group List into info per game
bos_split <- split(bos_schedule, ceiling(seq_along(bos_schedule)/13))

current <- matrix(unlist(bos_split),ncol=13,byrow=TRUE) #Turn list into a matrix
current <- cbind(current, rep('BOS', 82))

#Create a overall matrix
total <- current

###################
###################
###################


########################
#For Loop for each team#
########################

for(i in 1:length(team_list)){
    team <- team_list[i] #Set Current Team
    url <- paste0('http://www.basketball-reference.com/teams/', team, '/2017_games.html') #Get URL 
    html <- read_html(url) #Read HTML
    
    #Parse HTML into List
    schedule <- html %>%
        html_nodes('#games td:nth-child(13), #games td:nth-child(12), 
                   #games .center~ .right+ .center , #games .left a, 
                   #games td:nth-child(6) , #games .center+ td.left, 
                   #games .left+ td.center , #games .right') %>%
        html_text()
    
    #Group List into info per game
    split <- split(schedule, ceiling(seq_along(bos_schedule)/13))
    
    
    current <- matrix(unlist(split),ncol=13,byrow=TRUE) #Turn list into a matrix
    current <- cbind(current, rep(team, 82)) #Add Team Name into matrix
    
    total <- rbind(total, current) #Add it onto total number of matrices
    
    Sys.sleep(10) #pls don't ban us sportsreference
}

########################
########################
########################

#Turn Matrix into a dataframe
nba_team <- tibble(
    team = total[,14],
    game = total[,1],
    date = total[,2],
    time = total[,3],
    away = total[,5],
    opponent = total[,6],
    result = total[,8],
    team_pts = total[,9],
    opp_pts = total[,10],
    current_wins = total[,11],
    current_losses = total[,12],
    streak = total[,13]
)

#convert some stuff to numbers
nba_team$game <- as.numeric(nba_team$game)
nba_team$team_pts <- as.numeric(nba_team$team_pts)
nba_team$opp_pts <- as.numeric(nba_team$opp_pts)
nba_team$current_wins <- as.numeric(nba_team$current_wins)
nba_team$current_losses <- as.numeric(nba_team$current_losses)

#Export dataframe
write.xlsx(nba_team, file = 'teams.xlsx')
