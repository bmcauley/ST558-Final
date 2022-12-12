# RUN THIS SCRIPT TO INSTALL THE PROPER PACKAGES AND SET UP GLOBAL ENVIRONMENT OBJECTS

# PACKAGES REQUIRED FOR THE APPLICATION
pkglst <- c("shiny", "DT", "tidyverse", "caret", "rpart", "shinycssloaders")

OK <- pkglst %in% rownames(installed.packages())

# IF ANY OF THE PACKAGES IN PKGLST ARE NOT INSTALLED, INSTALL THEM
if (any(OK == FALSE)) {
  install.packages(pkglst[!OK])
}

# LOAD THE PACKAGES
lapply(pkglst, require, character.only = TRUE)


#----------ENVIRONMENT SET UP----------#

# CLEANING THE DATASET LOADED FROM CSV FILE

attrition <- read_csv("HR Employee Attrition.csv") %>%
  select(-EmployeeCount, -EmployeeNumber, -Over18, -PerformanceRating, -RelationshipSatisfaction,
         -StandardHours) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(BusinessTravel = factor(
    BusinessTravel,
    levels = c("Non-Travel", "Travel_Frequently", "Travel_Rarely"),
    labels = c("None", "Frequent", "Rare")
  ),
  Education = factor(
    Education,
    levels = c(1, 2, 3, 4, 5),
    labels = c("Below College", "Some College", "Bachelor's", "Master's", "PhD")
  ),
  EnvironmentSatisfaction = factor(
    EnvironmentSatisfaction,
    levels = c(1, 2, 3, 4),
    labels = c("Low", "Medium", "High", "Very High")
  ),
  JobInvolvement = factor(
    JobInvolvement,
    levels = c(1, 2, 3, 4),
    labels = c("Low", "Medium", "High", "Very High")
  ),
  JobSatisfaction = factor(
    JobSatisfaction,
    levels = c(1, 2, 3, 4),
    labels = c("Low", "Medium", "High", "Very High")
  ),
  WorkLifeBalance = factor(
    WorkLifeBalance,
    levels = c(1, 2, 3, 4),
    labels = c("Bad", "Good", "Better", "Best")
  )
  )

#DEFINITION OF VARIABLE NAME LISTS FROM DATASET

var_choice <- names(attrition)
numVar <- names(select_if(attrition, is.numeric))
factVar <- names(select_if(attrition, is.factor))


# FUNCTION DEFINITION TO GENERATE UI INPUTS BASED ON VARIABLES PROVIDED BY THE USER

make_ui <- function(x, var) {
  
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    
    numericInput(
      var, var, value = floor(mean(rng)), min = rng[1], max = rng[2], step = 1
    )
    
  } else if (is.factor(x)) {
    levs <- levels(x)
    
    selectInput(
      var, var, choices = levs, selected = levs[1], multiple = FALSE
    )
    
  } else {
    NULL
  }
}