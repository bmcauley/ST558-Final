## Project Summary

### Overview

The purpose of this app is to demonstrate some of the capabilities R shiny provides for interactive data analysis--from exploring the raw data to making predictions with user-defined models. The analysis focuses on HR employee data sampled from IBM to predict whether an employee attrits based on a variety of factors that may impact their experience with an employer.

### Packages

The following R packages were used in developing this app. For it to perform as expected, they should be installed and loaded.

- [shiny](https://shiny.rstudio.com/) - For building interactive web apps via R
- [DT](https://rstudio.github.io/DT/) - For interfacing with the JavaScript DataTables library via R; easily enables advanced HTML controls for tables
- [tidyverse](https://www.tidyverse.org/) - For intuitive data manipulation and analysis; also enables use of its sub-packages, the following of which were used:
   - [ggplot2](https://ggplot2.tidyverse.org/), [readr](https://readr.tidyverse.org/), [dplyr](https://dplyr.tidyverse.org/), [tibble](https://tibble.tidyverse.org/), and [tidyr](https://tidyr.tidyverse.org/)
- [caret](https://github.com/topepo/caret/) - For training and plotting models for classification and regression problems
- [rpart](https://cran.r-project.org/web/packages/rpart/rpart.pdf) - Needed for `caret` training to recognize `rpart` method
- [shinycssloaders](https://github.com/gbm-developers/gbm#readme) - Enables loading animations for Shiny objects that are (re)calculating

### Accessing the Application

Download the [installPackage.R](installPackage.R) script, and run it in your RStudio environment. This file ensures all required packages are installed and loaded, and generates objects into the global environment.

Then, copy and run the following code in the console to open the app.

```
shiny::runGitHub("bmcauley/ST558-Final", ref = "main", subdir = "STFP")
```
