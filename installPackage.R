# Run this script to install and/or load the required packages for the STFP application

# Packages required for the app
pkglst <- c("shiny", "DT", "tidyverse", "caret", "leaps", "gbm", "rpart", "shinycssloaders", "rattle")

OK <- pkglst %in% rownames(installed.packages())

# If any of the packages in pkglst are not installed, install them
if (any(okPkg == FALSE)) {
  install.packages(pkglst[!OK])
}

# Load the packages
lapply(pkglst, require, character.only = TRUE)

t <- trainControl(method = "cv", number = 3)
tune <- data.frame(cp = 0.05)
i <- createDataPartition(attrition[[1]], p = 0.7, list = FALSE)

treeOut <- train(Attrition ~ .,
                 data = attrition[i,factVar],
                 method = "glm",
                 metric = "Accuracy")

treeOut$results$Accuracy

summary(treeOut$finalModel)

ttdf <- attrition[-i,]

row <- attrition[4,factVar]

preds <- predict.train(treeOut, newdata = row)
predResults <- postResample(preds, obs = ttdf$Attrition)
#----------------

