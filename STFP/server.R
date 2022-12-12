# THIS R FILE CONTAINS THE SERVER LOGIC FOR THE STFP APPLICATION


shinyServer(function(input, output) {
  
#-----PLOT RENDERING FOR EDA PAGE-----#
  
# TYPE: DENSITY PLOT
  output$ggplot <- renderPlot({
    if (input$plotType == 'density') {
      g <- ggplot(data = attrition, aes(x = get(input$densityX)))
      
      gOut <- g + geom_histogram(aes(y = ..density..), fill = "lightblue") +
        geom_density(linetype = 2, color = "red") +
        labs(x = input$densityX)
      
      if ('fill' %in% input$plotOpt) {
        gOut <- g + geom_histogram(aes(y = ..density.., fill = get(input$groupVar)), alpha = 0.5) +
          geom_density(linetype = 2, aes(color = get(input$groupVar))) +
          labs(x = input$densityX, fill = input$groupVar, color = input$groupVar)
      }
      
      if ('facet' %in% input$plotOpt) {
        gOut <- gOut + facet_wrap(input$facetVar)
      }
      
      gOut
    }
    
# TYPE: BAR PLOT
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
    
# TYPE: SCATTER PLOT
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
    
# TYPE: BOXPLOT
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
  
#-----SUMMARY TABLE RENDERING FOR EDA PAGE-----#
  
  output$summTable <- renderDataTable({
    
    grplist <- syms(input$tblGroupVar)
    varlist <- syms(input$sumVar)
    
# CORRELATION MATRIX
    
    if (input$tblType == 'corr') {
      corrTbl <- attrition %>%
        select(all_of(input$corrvars))
      
      cnt_tbl <- round(cor(corrTbl), 4)
    }
    
# FIVE-NUMBER SUMMARY
    
    else if (input$tblType == 'fivenum') {
      
      if (input$tableGroup == 'Yes') {
        cnt_tbl <- attrition %>%
          select(!!!varlist,!!!grplist) %>%
          group_by(!!!grplist) %>%
          dplyr::summarize_all(list(
            Min = min,
            Q1 = ~ quantile(., 0.25),
            Median = median,
            Q3 = ~ quantile(., 0.75),
            Max = max
          ))
        
        if (length(varlist) > 1) {
          cnt_tbl <- cnt_tbl %>%
            pivot_longer(
              cols = ends_with(c("_Min", "_Q1", "_Median", "_Q3", "_Max")),
              names_sep = "_",
              names_to = c("SummVar", ".value")
            )
        }
        
      } else if (input$tableGroup == 'No') {
        cnt_tbl <- attrition %>%
          select(!!!varlist) %>%
          summarize_all(list(
            Min = min,
            Q1 = ~ quantile(., 0.25),
            Median = median,
            Q3 = ~ quantile(., 0.75),
            Max = max
          ))
        
        if (length(varlist) > 1) {
          cnt_tbl <- cnt_tbl %>%
            pivot_longer(
              cols = everything(),
              names_sep = "_",
              names_to = c("SummVar", ".value")
            )
        }
        
      }
    }
    
    
# MEASURES OF CENTER & SPREAD SUMMARY
    else {
      
      if (input$tableGroup == 'Yes') {
        cnt_tbl <- attrition %>%
          select(!!!varlist,!!!grplist) %>%
          group_by(!!!grplist) %>%
          summarize_all(list(
            Mean = mean,
            Stddev = sd,
            Var = var,
            IQR = IQR
          ))
        
        if (length(varlist) > 1) {
          cnt_tbl <- cnt_tbl %>%
            pivot_longer(
              cols = ends_with(c("_Mean", "_Stddev", "_Var", "_IQR")),
              names_sep = "_",
              names_to = c("SummVar", ".value")
            )
        }
        
        cnt_tbl <- cnt_tbl %>%
          mutate_if(is.numeric, ~ round(., 2))
        
      }
      
      else if (input$tableGroup == 'No') {
        cnt_tbl <- attrition %>%
          select(!!!varlist) %>%
          summarize_all(list(
            Mean = mean,
            Stddev = sd,
            Var = var,
            IQR = IQR
          ))
        
        if (length(varlist) > 1) {
          cnt_tbl <- cnt_tbl %>%
            pivot_longer(
              cols = everything(),
              names_sep = "_",
              names_to = c("SummVar", ".value")
            )
        }
        
      }
      
      cnt_tbl <- cnt_tbl %>%
        mutate_if(is.numeric, ~ round(., 2))
    }
    
# RENAME COLUMNS TO VARIABLE NAMES IF THERE IS GROUPING;
# MAKES THE TABLE LOOK CONSISTENT
    
    if (input$tblType != 'corr' && input$tableGroup == 'Yes') {
      cnt_tbl <- cnt_tbl %>%
        rename_with(.cols = 1, ~ paste0(input$tblGroupVar))
    }
    

# OUTPUT DATATABLE
    datatable(cnt_tbl,
              options = list(dom = 'ltipr'))
  })
  
#-----LOGIC FOR MODEL BUILDING PAGE-----#
  
# DATA PARTITIONING
  i <- reactive({
    createDataPartition(attrition[[1]], p = input$prop, list = FALSE)
  })
  
  trainData <- reactive({
    attrition[i(),]
  })
  
  testData <- reactive({
    attrition[-i(),]
  })
  
  
# LOGISTIC REGRESSION MODEL OBJECT
  log <- eventReactive(input$modelBuild,
                       {
                         prd <- input$logVars
                         
                         summOut <- train(
                           Attrition ~ .,
                           data = trainData()[, c("Attrition", prd)],
                           method = "glm",
                           metric = "Accuracy"
                         )
                         
# CROSS-VALIDATION OPTION ENABLED
                         
                         if ('cv' %in% input$logSettings) {
                           t <- trainControl(method = "cv", number = input$logCV)
                           
                           summOut <- train(
                             Attrition ~ .,
                             data = trainData()[, c("Attrition", prd)],
                             method = "glm",
                             metric = "Accuracy",
                             trControl = t
                           )
                           
                         }
                         
# VARIABLE STANDARDIZATION OPTION ENABLED
                         
                         if ('std' %in% input$logSettings) {
                           p <- c("scale", "center")
                           
                           summOut <- train(
                             Attrition ~ .,
                             data = trainData()[, c("Attrition", prd)],
                             method = "glm",
                             metric = "Accuracy",
                             preProcess = p
                           )
                         }
                         
# CV & VARIABLE STANDARIZATION ENABLED
                         
                         if ('cv' %in% input$logSettings && 'std' %in% input$logSettings) {
                           summOut <- train(
                             Attrition ~ .,
                             data = trainData()[, c("Attrition", prd)],
                             method = "glm",
                             metric = "Accuracy",
                             preProcess = p,
                             trControl = t
                           )
                         }
                         
                         summOut
                       })
  
# FIT SUMMARY FOR LOGISTIC REGRESSION MODEL
  
  output$logModel <- renderPrint({
    req(input$modelBuild)
    
    isolate({
      test <- testData()[, c("Attrition", input$logVars)]
      
      preds <- predict.train(log(), newdata = test)
      predResults <- postResample(preds, obs = test$Attrition)
      
      cat(
        "Train Accuracy: ", round(log()$results$Accuracy, 4),
        "\nTest Accuracy: ", round(predResults[[1]], 4),
        "\n\nModel:\n"
      )
      
      summary(log())
      
    })
  })

# CLASSIFICATION TREE OBJECT
  
  tree <- eventReactive(input$modelBuild,
                        {
                          prd <- input$treeVars
                          tune <- data.frame(cp = input$cp)
                          
                          treeObj <- train(
                            Attrition ~ .,
                            data = trainData()[, c("Attrition", prd)],
                            method = "rpart",
                            metric = "Accuracy",
                            tuneGrid = tune
                          )
                          
# CROSS VALIDATION ENABLED
                          
                          if ('cv' %in% input$treeSettings) {
                            t <- trainControl(method = "cv", number = input$treeCV)
                            
                            treeObj <- train(
                              Attrition ~ .,
                              data = trainData()[, c("Attrition", prd)],
                              method = "rpart",
                              metric = "Accuracy",
                              trControl = t,
                              tuneGrid = tune
                            )
                          }
                          
                          treeObj
                        })
  
  
# FIT SUMMARY FOR CLASSIFICATION TREE
  
  output$treeModel <- renderPrint({
    req(input$modelBuild)
    
    isolate({
      test <- testData()[, c("Attrition", input$treeVars)]
      
      preds <- predict.train(tree(), newdata = test)
      predResults <- postResample(preds, obs = test$Attrition)
      
      cat(
        "Train Accuracy: ", round(tree()$results$Accuracy, 4),
        "\nTest Accuracy: ", round(predResults[[1]], 4),
        "\n\nVariable Importance:\n"
      )
      
      varImp(tree()$finalModel)  %>% arrange(desc(Overall))
      
    })
  })
  
# RANDOM FOREST OBJECT
  
  rf <- eventReactive(input$modelBuild,
                      
                      {
                        prd <- input$rfVars
                        tune <- data.frame(mtry = input$mtry)
                        
                        rfObj <- train(
                          Attrition ~ .,
                          data = trainData()[, c("Attrition", prd)],
                          method = "rf",
                          metric = "Accuracy",
                          tuneGrid = tune
                        )

# CROSS VALIDATION ENABLED
                        
                        if ('cv' %in% input$rfSettings) {
                          t <- trainControl(method = "cv", number = input$rfCV)
                          
                          rfObj <- train(
                            Attrition ~ .,
                            data = trainData()[, c("Attrition", prd)],
                            method = "rf",
                            metric = "Accuracy",
                            trControl = t,
                            tuneGrid = tune
                          )
                        }
                        
                        rfObj
                      })
  
  
# FIT SUMMARY FOR RANDOM FOREST
  
  output$rfModel <- renderPrint({
    req(input$modelBuild)
    
    isolate({
      test <- testData()[, c("Attrition", input$rfVars)]
      
      preds <- predict.train(rf(), newdata = test)
      predResults <- postResample(preds, obs = test$Attrition)
      
      cat(
        "Train Accuracy: ", round(rf()$results$Accuracy, 4),
        "\nTest Accuracy: ", round(predResults[[1]], 4),
        "\n\nVariable Importance:\n"
      )
      
      varImp(rf()$finalModel)  %>% arrange(desc(Overall))
      
    })
  })
  
#-----LOGIC FOR MODEL PREDICTION PAGE-----#
  
# GET VARIABLE NAMES FROM EACH MODEL
  
  logNames <- reactive(input$logVars)
  treeNames <- reactive(input$treeVars)
  rfNames <- reactive(input$rfVars)
  
# RENDER UI FOR EACH MODEL BASED ON VARIABLE INPUTS
  
  output$logUI <- renderUI({
    map(logNames(), ~ make_ui(attrition[[.x]], .x))
  })
  
  output$treeUI <- renderUI({
    map(treeNames(), ~ make_ui(attrition[[.x]], .x))
  })
  
  output$rfUI <- renderUI({
    map(rfNames(), ~ make_ui(attrition[[.x]], .x))
  })
  
# GET USER-INPUT VALUES FROM UI
  
  logVals <- reactive(a <- map(logNames(), ~ input[[.x]]))
  treeVals <- reactive(b <- map(treeNames(), ~ input[[.x]]))
  rfVals <- reactive(c <- map(rfNames(), ~ input[[.x]]))
  
# MODEL PREDICTION
  
  logGuess <- reactive({
    new <- tibble(name = unlist(logNames()), x = unlist(logVals()))
    
    new <- new %>% pivot_wider(names_from = name, values_from = x) %>%
      mutate_if(logNames() %in% numVar, as.numeric)
    
    predict.train(log(), newdata = as.data.frame(new))
  })
  
  treeGuess <- reactive({
    new <- tibble(name = unlist(treeNames()), x = unlist(treeVals()))
    
    new <- new %>% pivot_wider(names_from = name, values_from = x) %>%
      mutate_if(treeNames() %in% numVar, as.numeric)
    
    predict.train(tree(), newdata = as.data.frame(new))
  })
  
  rfGuess <- reactive({
    new <- tibble(name = unlist(rfNames()), x = unlist(rfVals()))
    
    new <- new %>% pivot_wider(names_from = name, values_from = x) %>%
      mutate_if(rfNames() %in% numVar, as.numeric)
    
    predict.train(rf(), newdata = as.data.frame(new))
  })

# OUTPUT USER-INPUT VALUES
  
  output$logEmploy <- renderPrint({
    print("ABOUT THIS EMPLOYEE")
    
    for (i in 1:length(logNames())) {
      print(paste0(logNames()[i], ": ", logVals()[i]))
    }
    
  })
  
  output$treeEmploy <- renderPrint({
    print("ABOUT THIS EMPLOYEE")
    
    for (i in 1:length(treeNames())) {
      print(paste0(treeNames()[i], ": ", treeVals()[i]))
    }
    
  })
  
  output$rfEmploy <- renderPrint({
    print("ABOUT THIS EMPLOYEE")
    
    for (i in 1:length(rfNames())) {
      print(paste0(rfNames()[i], ": ", rfVals()[i]))
    }
    
  })
  
# OUTPUT PREDICTION
  
  output$logResult <- renderText({
    req(input$predict)
    
    isolate({
    x <- if_else(logGuess() == 'No', 'DOES NOT ATTRIT', 'ATTRITS')
    
    print(paste0("Based on the given values, Logistic Regression predicts this employee...", x, "."))
    })
  })
  
  output$treeResult <- renderText({
    req(input$predict)
    
    isolate({
    x <- if_else(treeGuess() == 'No', 'DOES NOT ATTRIT', 'ATTRITS')
    
    print(paste0("Based on the given values, Classification Tree predicts this employee...", x, "."))
    })
  })
  
  output$rfResult <- renderText({
    req(input$predict)
    
    isolate({
    x <- if_else(rfGuess() == 'No', 'DOES NOT ATTRIT', 'ATTRITS')
    
    print(paste0("Based on the given values, Random Forest predicts this employee...", x, "."))
    })
  })
  
#-----LOGIC FOR DATA EXPLORATION PAGE-----#
  
# FILTER DATA BY USER INPUTS
  Tbl <- reactive({
    
    #CHECK IF FILTER BOX IS EMPTY OR NOT
    if (isTruthy(input$tblRows)) {
      attrition %>% select(all_of(input$tblVars)) %>%
        filter(eval(parse(text = input$tblRows)))
    } else {
      attrition %>% select(all_of(input$tblVars))
    }
    
  })
  
# OUTPUT OF FILTERED DATA
  
  output$fullTable <- renderDataTable({
    req(input$updateTbl)
    
    isolate({
      datatable(Tbl(),
                options = list(
                  lengthMenu = list(c(10, 20, 50,-1), c('10', '20', '50', 'All')),
                  pageLength = 10,
                  dom = 'ltipr'
                ))
    })
    
  })
  
  
# SAVE FILTERED DATA
  
  output$employData <- downloadHandler(
    filename = function() {
      paste("attrition_", format(Sys.time(), '%d-%m-%Y_%H-%M-%S'), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(Tbl(), file)
    }
  )
  
  
})
