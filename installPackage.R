# Run this script to install and/or load the required packages for the STFP application

# Packages required for the app
pkglst <- c("shiny", "DT", "tidyverse", "caret", "leaps", "gbm", "rpart", "shinycssloaders")

OK <- pkglst %in% rownames(installed.packages())

# If any of the packages in pkglst are not installed, install them
if (any(okPkg == FALSE)) {
  install.packages(pkglst[!OK])
}

# Load the packages
lapply(pkglst, require, character.only = TRUE)

t <- trainControl(method = "cv", number = 3)
tune <- data.frame(cp = 0.05)

treeOut <- train(Attrition ~ .,
                 data = attrition[i,],
                 method = "rpart",
                 metric = "Accuracy",
                 tuneGrid = tune)

treeOut$results$Accuracy

print((treeOut$finalModel$variable.importance))

varImp(treeOut)

test <- testData()[, c("Attrition", input$treeVars)]

ttdf <- attrition[-i,]

preds <- predict.train(treeOut, newdata = ttdf)
predResults <- postResample(preds, obs = ttdf$Attrition)
