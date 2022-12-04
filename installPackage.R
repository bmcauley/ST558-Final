# Run this script to install and/or load the required packages for the STFP application

# Packages required for the app
pkglst <- c("shiny", "DT", "tidyverse", "caret", "leaps", "gbm")

OK <- pkglst %in% rownames(installed.packages())

# If any of the packages in pkglst are not installed, install them
if (any(okPkg == FALSE)) {
  install.packages(pkglst[!OK])
}

# Load the packages
lapply(pkglst, require, character.only = TRUE)
