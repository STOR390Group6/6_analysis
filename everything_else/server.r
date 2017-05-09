library(shiny)
library(ggplot2)
library(readr)
library(tidyverse)
twitter_sentiment <- read_csv("twitter_sentiment.csv")



shinyServer(function(input, output){     
  
  
  #output$choice = unique(filter(twitter_sentiment, input$input_team)$Player)    
  
  
  output$plot_1 <- renderPlot({ 

    twitter_sentiment %>% filter(Player == input$input_player) %>%
      ggplot(aes(x=created_at, y = sent)) +
      geom_point(shape = 21, colour = "brown", fill = "white", size = 3, stroke = 3) +
      labs(x="Month", y= "Sentiment Level") +
      geom_smooth(se = FALSE) +
      theme_bw()
      #geom_smooth(data=filter(gamelogs, player==input$input_player), aes(x=as.POSIXct(date), y=normalized_gamescore))
    
    })
  
  # output$plot_2 <- renderPlot({ 
  #   
  #   twitter_sentiment %>% filter(Player == input$input_player) %>%
  #     ggplot(aes(x=created_at, y = sent)) +
  #     geom_jitter(shape = 21, colour = "brown", fill = "white", size = 3, stroke = 3) +
  #     labs(x="Month", y= "Sentiment Level") +
  #     geom_smooth(se = FALSE) +
  #     theme_bw()
    
  # })
  
  
  
  output$ui <- renderUI({
    
    
    selectInput(inputId = "input_player",    
                
                label = "Player",  
                
                choices = unique(filter(twitter_sentiment, Tm == input$input_team)$Player))
  })
    

  })
  







    
    
    



