# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library(shiny)
library(caret)
library(tidyverse)
library(DT)

genshin <- read.csv("genshin.csv")

gFLT <- genshin %>% 
  select(-starts_with("voice"), -constellation, -special_dish) %>%
  mutate(birthday = as.Date(birthday, '%m/%d')) %>%
  mutate_if(is.character, as.factor)

shinyServer(function(input, output) {
  
  #create output of observations    
  output$fullTable <- renderDataTable({
    
    gTbl <- gFLT %>%
      select(input$vars)
    
    datatable(gTbl,
              options = list(
                lengthMenu = list(c(10, 20, 50, -1), c('10', '20','50', 'All')),
                pageLength = 10),
              filter = list(
                position = "top",
                clear = FALSE))
   
   })
    
    
  })

