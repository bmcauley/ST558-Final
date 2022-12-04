# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library(shiny)
library(caret)
library(tidyverse)
library(DT)

attrition <- read_csv("HR Employee Attrition.csv") %>%
  select(-EmployeeCount, -EmployeeNumber, -Over18, -PerformanceRating, -RelationshipSatisfaction,
         -StandardHours) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(BusinessTravel = factor(BusinessTravel,
                                 levels = c("Non-Travel", "Travel_Frequently", "Travel_Rarely"),
                                 labels = c("None", "Frequent", "Rare")),
         Education = factor(Education,
                            levels = c(1, 2, 3, 4, 5),
                            labels = c("Below College", "Some College", "Bachelor's", "Master's", "PhD")),
         EnvironmentSatisfaction = factor(EnvironmentSatisfaction,
                                          levels = c(1, 2, 3, 4),
                                          labels = c("Low", "Medium", "High", "Very High")),
         JobInvolvement = factor(JobInvolvement,
                                 levels = c(1, 2, 3, 4),
                                 labels = c("Low", "Medium", "High", "Very High")),
         JobSatisfaction = factor(JobSatisfaction,
                                  levels = c(1, 2, 3, 4),
                                  labels = c("Low", "Medium", "High", "Very High")),
         WorkLifeBalance = factor(WorkLifeBalance,
                                  levels = c(1, 2, 3, 4),
                                  labels = c("Bad", "Good", "Better", "Best")))

shinyServer(function(input, output) {
  
  output$ggplot <- renderPlot({
    
    G <- ggplot(data = attrition, aes(x = get(input$plotVar)))
    Gb <- ggplot(data = attrition, aes(x = get(input$barPlotVar)))
    
    if (input$plotType == 'density' && input$fillOpt == 'No') {
      G + geom_histogram(aes(y=..density..), color = "black", fill = "lightblue") +
        geom_density(linetype = 2, color = "red") +
        xlab(input$plotVar)
    }
    
    else if (input$plotType == 'density' && input$fillOpt == 'Yes') {
      G + geom_density(aes(fill = get(input$fillVar)), alpha = 0.5) +
        labs(x = input$plotVar, fill = input$fillVar)
    }
    
    else if (input$plotType == 'bar' && input$barfillOpt == FALSE && input$barfacetOpt == FALSE) {
      Gb + geom_bar(aes(fill = get(input$barPlotVar)), show.legend = FALSE) +
        xlab(input$barPlotVar)
    }
    
    else if (input$plotType == 'bar' && input$barfillOpt == TRUE && input$barfacetOpt == FALSE) {
      GbF <- ggplot(data = attrition, aes(get(input$barPlotVar), fill = get(input$barfillVar)))
      
      GbF + geom_bar(position = 'dodge') +
        labs(x = input$barPlotVar, fill = input$barfillVar)
    }
    
    else if (input$plotType == 'bar' && input$barfillOpt == FALSE && input$barfacetOpt == TRUE) {
      ggplot(data = attrition, aes(get(input$barPlotVar), fill = get(input$barPlotVar))) + 
        geom_bar() +
        facet_wrap(vars(get(input$barfacetVar))) +
        labs(x = input$barfacetVar, fill = input$barPlotVar)
    }
    
    else if (input$plotType == 'bar' && input$barfillOpt == TRUE && input$barfacetOpt == TRUE) {
      ggplot(data = attrition, aes(get(input$barPlotVar), fill = get(input$barfillVar))) + 
        geom_bar(position = 'dodge') +
        facet_wrap(vars(get(input$barfacetVar))) +
        labs(x = input$barplotVar, fill = input$barfillVar)
    }
    
    else if (input$plotType == 'point') {
      ggplot(attrition, aes(x = get(input$scatterX), y = get(input$scatterY))) +
        geom_point() +
        labs(x = input$scatterX, y = input$scatterY)
    }
    
  })
  
  output$summTable <- renderDataTable({
    
    if (input$tblType == 'corr') {
      
      corrTbl <- attrition %>%
        select(all_of(input$corrvars))
      
      cnt_tbl <- cor(corrTbl)
    }
    
    else if (input$tblType == 'fivesum') {
    if (input$tableGroup == 'Yes') {
      cnt_tbl <- attrition %>%
        group_by(get(input$tblGroupVar)) %>%
        summarize(n = n(),
                  min = min(get(input$sumVar)),
                  mean = round(mean(get(input$sumVar)), 2),
                  max = max(get(input$sumVar)),
                  stddev = round(sd(get(input$sumVar)), 2)
        )
                  
    } else if (input$tableGroup == 'No') {
      
      cnt_tbl <- attrition %>%
        summarize(min = min(get(input$sumVar)),
                  mean = round(mean(get(input$sumVar)), 2),
                  max = max(get(input$sumVar)),
                  stddev = round(sd(get(input$sumVar)), 2))
                  }
      }
    
    datatable(cnt_tbl)
  })
  
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

