# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library(shiny)
library(caret)
library(tidyverse)
library(DT)

attrition <- read.csv("Attrition.csv") %>%
  select(-X, -starts_with("Date_of_")) %>%
  mutate_if(is.character, as.factor)


shinyServer(function(input, output) {
  
  #create output of observations    
  output$fullTable <- renderDataTable({
    
    Tbl <- attrition %>%
      select(input$vars)
    
    datatable(Tbl,
              options = list(
                lengthMenu = list(c(10, 20, 50, -1), c('10', '20','50', 'All')),
                pageLength = 10),
              filter = list(
                position = "top",
                clear = FALSE))
   
   })
  
  # output$employData <- downloadHandler(
  #   filname = function() {
  #     paste("attrition_", format(Sys.time(), '%d-%m-%Y_%H:%M:%S'), ".csv", sep = "")
  #   },
  #   
  #   content = function(file) {
  #     write.csv(output$fullTable, file)
  #   }
  # )
    
    
  })

