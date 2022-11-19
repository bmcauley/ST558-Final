## Project Summary

### Overview

[App description and purpose]

### Packages

The following R packages were used in developing this app. For it to perform as expected, they should be installed and loaded.

- [shiny](https://shiny.rstudio.com/) - For building interactive web apps via R
- [tidyverse](https://www.tidyverse.org/) - For intuitive data manipulation and analysis; also enables use of its sub-packages, the following of which were used:
   - [ggplot2](https://ggplot2.tidyverse.org/), [readr](https://readr.tidyverse.org/), [dplyr](https://dplyr.tidyverse.org/), [tibble](https://tibble.tidyverse.org/), and [tidyr](https://tidyr.tidyverse.org/)
- [caret](https://github.com/topepo/caret/) - For training and plotting models for classification and regression problems
- [leaps](https://www.rdocumentation.org/packages/leaps/versions/3.1/topics/leaps) - For selection of the best subset of predictor variables 
- [gbm](https://github.com/gbm-developers/gbm#readme) - Needed for `caret` training to recognize `gbm` method

### Accessing the Application

Download the [installPackage.R](installPackage.R) script, or copy and run the code here to install any missing packages and load all of them.

```
pkglst <- c("shiny", "tidyverse", "caret", "leaps", "gbm")

OK <- pkglst %in% rownames(installed.packages())

if (any(okPkg == FALSE)) {
  install.packages(pkglst[!OK])
}

lapply(pkglst, require, character.only = TRUE)

```

Copy and run the following code in RStudio to open the app.

```
shiny::runGitHub("bmcauley/ST558-Final", ref = "main", subdir = "STFP")
```
