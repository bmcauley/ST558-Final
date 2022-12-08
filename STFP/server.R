# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library(shiny)
library(caret)
library(tidyverse)
library(DT)

#Data cleaning step
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
    
    if (input$plotType == 'density') {
      
      g <- ggplot(data = attrition, aes(x = get(input$densityX)))
      
      gOut <- g + geom_histogram(aes(y=..density..), fill = "lightblue") +
        geom_density(linetype = 2, color = "red") +
        labs(x= input$densityX)

      if ('fill' %in% input$plotOpt) {
        gOut <- g + geom_histogram(aes(y=..density.., fill = get(input$groupVar)), alpha = 0.5) +
          geom_density(linetype = 2, aes(color = get(input$groupVar))) +
          labs(x = input$densityX, fill = input$groupVar, color = input$groupVar)
      } 
      
      if ('facet' %in% input$plotOpt) {
        gOut <- gOut + facet_wrap(input$facetVar)
      }
      
      gOut
      
    }
    
    
    else if (input$plotType == 'bar') {
      
      g <- ggplot(data = attrition, aes(x = get(input$barX)))
      
      gOut <- g + geom_bar(aes(fill = get(input$barX))) +
        xlab(input$barX) +
        guides(fill = "none")
      
      if ('fill' %in% input$plotOpt) {
        gOut <- g + geom_bar(aes(fill = get(input$groupVar)), position = "dodge") +
          labs(x = input$barX, fill = input$groupVar)
          
      }
      
      if ('facet' %in% input$plotOpt) {
        gOut <- gOut + facet_wrap(input$facetVar)
      }
      
      gOut
      
    }
    
    else if (input$plotType == 'point') {
      g <- ggplot(data = attrition, aes(x = get(input$pointX), y = get(input$pointY)))
      
      gOut <- g + geom_point() + geom_smooth(method = "lm") + 
        labs(x = input$pointX, y = input$pointY)
      
      if ('fill' %in% input$plotOpt) {
        gOut <- g + geom_point() + geom_smooth(method = "lm", aes(fill = get(input$groupVar))) +
          labs(x = input$pointX, y = input$pointY, fill = input$groupVar)
      }
      
      if ('facet' %in% input$plotOpt) {
        gOut <- gOut + facet_wrap(input$facetVar)
      }
      
      gOut
    }
    
    else if (input$plotType == 'box') {
      g <- ggplot(data = attrition, aes(x = get(input$boxX), y = get(input$boxY)))
      
      gOut <- g + geom_boxplot(aes(fill = get(input$boxX))) +
        labs(x = input$boxX, y = input$boxY) +
        guides(fill = "none")
      
      if ('fill' %in% input$plotOpt) {
        gOut <- g + geom_boxplot(aes(fill = get(input$groupVar))) +
          labs(x = input$boxX, y = input$boxY, fill = input$groupVar)
      }
      
      if ('facet' %in% input$plotOpt) {
        gOut <- gOut + facet_wrap(input$facetVar)
      }
      
      gOut
    }
    
  })
  
  output$summTable <- renderDataTable({
    
    grplist <- syms(input$tblGroupVar)
    varlist <- syms(input$sumVar)
    
    if (input$tblType == 'corr') {
      
      corrTbl <- attrition %>%
        select(all_of(input$corrvars))
      
      cnt_tbl <- round(cor(corrTbl), 4)
    }
    
    else if (input$tblType == 'fivenum') {
    if (input$tableGroup == 'Yes') {
      
      cnt_tbl <- attrition %>%
        select(!!!varlist, !!!grplist) %>%
        group_by(!!!grplist) %>%
        dplyr::summarize_all(
          list(Min = min,
            Q1 = ~quantile(., 0.25),
            Median = median,
            Q3 = ~quantile(., 0.75),
            Max = max)
        )
      
      if(length(varlist) > 1) {
        cnt_tbl <- cnt_tbl %>%
          pivot_longer(cols = ends_with(c("_Min", "_Q1", "_Median", "_Q3", "_Max")),
                     names_sep = "_",
                     names_to = c("SummVar", ".value"))
      }
                  
    } else if (input$tableGroup == 'No') {
      
      cnt_tbl <- attrition %>%
        select(!!!varlist) %>%
        summarize_all(list(
                  Min = min,
                  Q1 = ~quantile(., 0.25),
                  Median = median,
                  Q3 = ~quantile(., 0.75),
                  Max = max)
        )
      
      if (length(varlist) > 1) {
        cnt_tbl <- cnt_tbl %>%
          pivot_longer(cols = everything(),
                     names_sep = "_",
                     names_to = c("SummVar", ".value"))
      }
        
        
                  }
    }
    
    else {
      if (input$tableGroup == 'Yes') {
        cnt_tbl <- attrition %>%
          select(!!!varlist, !!!grplist) %>%
          group_by(!!!grplist) %>%
          summarize_all(list(
              Mean = mean,
              Stddev = sd,
              Var = var,
              IQR = IQR)
              )
        
        if (length(varlist) > 1) {
          cnt_tbl <- cnt_tbl %>%
            pivot_longer(cols = ends_with(c("_Mean", "_Stddev", "_Var", "_IQR")),
                       names_sep = "_",
                       names_to = c("SummVar", ".value"))
        }
        
        cnt_tbl <- cnt_tbl %>%
          mutate_if(is.numeric, ~round(., 2))
          
      }
      
      else if (input$tableGroup == 'No') {
        cnt_tbl <- attrition %>%
          select(!!!varlist) %>%
        summarize_all(list(
          Mean = mean,
          Stddev = sd,
          Var = var,
          IQR = IQR)
        )
        
        if(length(varlist) > 1) {
          cnt_tbl <- cnt_tbl %>%
            pivot_longer(cols = everything(),
                       names_sep = "_",
                       names_to = c("SummVar", ".value"))
        }
          
      }
      
      cnt_tbl <- cnt_tbl %>%
        mutate_if(is.numeric, ~round(., 2))
    }
    
    if (input$tblType != 'corr' && input$tableGroup == 'Yes') {
      cnt_tbl <- cnt_tbl %>% 
        rename_with(.cols = 1, ~paste0(input$tblGroupVar))
    }

    
    datatable(cnt_tbl)
  })
  
  #create output of observations    
  output$fullTable <- renderDataTable(

    datatable(attrition,
              options = list(
                lengthMenu = list(c(10, 20, 50, -1), c('10', '20','50', 'All')),
                pageLength = 10),
              filter = list(
                position = "top",
                clear = FALSE)
              
    )
    
  )
  
 # Tbl <- reactive(attrition %>% select(all_of(input$tblVars)))
  
  
  
  # output$employData <- downloadHandler(
  #   filname = function() {
  #     
  #     #paste("attrition_", format(Sys.time(), '%d-%m-%Y_%H:%M:%S'), ".csv", sep = "")
  #   },
  # 
  #   content = function(file) {
  #     write.csv(attrition[input[["dt_rows_all"]]], file)
  #   },
  # 
  #   contentType = "text/csv"
  # )

    
  })

