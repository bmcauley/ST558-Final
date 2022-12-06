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
        select(input$sumVar, !!!grplist) %>%
        group_by(!!!grplist) %>%
        summarize(VarName = input$sumVar,
                  N = n(),
                  Min = min(get(input$sumVar)),
                  Q1 = quantile(get(input$sumVar), 0.25),
                  Median = median(get(input$sumVar)),
                  Q3 = quantile(get(input$sumVar), 0.75),
                  Max = max(get(input$sumVar))
        )
                  
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
                  }
    }
    
    else {
      if (input$tableGroup == 'Yes') {
        cnt_tbl <- attrition %>%
          select(input$sumVar, !!!grplist) %>%
          group_by(!!!grplist) %>%
          summarize(VarName = input$sumVar,
                    Mean = round(mean(get(input$sumVar)), 2),
                    Stddev = round(sd(get(input$sumVar)), 2),
                    Var = round(var(get(input$sumVar)), 2),
                    IQR = IQR(get(input$sumVar))
          )
      }
      
      else if (input$tableGroup == 'No') {
        cnt_tbl <- attrition %>%
        summarize(VarName = input$sumVar,
                  Mean = round(mean(get(input$sumVar)), 2),
                  Stddev = round(sd(get(input$sumVar)), 2),
                  Var = round(var(get(input$sumVar)), 2),
                  IQR = IQR(get(input$sumVar))
        )
      }
    }
    
    if (input$tblType != 'corr' && input$tableGroup == 'Yes') {
      cnt_tbl <- cnt_tbl %>% 
        rename_with(.cols = 1, ~paste(input$tblGroupVar))
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

