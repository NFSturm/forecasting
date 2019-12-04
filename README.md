# Forecasting
This repository contains code to conduct a recession analysis, making heavy use of the »caret«-package. Three models are compared in their performance as recession predictors: Boosting (gbm), Random Forests (ranger) and simple Logistic Regression (glm). Using the yield curve and up to 10 lags, the probability of a recession is estimated.

Data for the Yield Curve is drawn from FRED:
https://fred.stlouisfed.org/series/T10Y2Y

Data for US recession is from NBER:
https://www.nber.org/cycles.html

# Packages used 
* readxl
* zoo
* timetk
* xts
* tidyquant
* ggplot2
* rsample
* caret
* quantmod
* dplyr
* doParallel
* MLmetrics
* data.table
* readr

**Note**: Since recessions rarely happen (luckily!), the classes "Bust" (for recession) and "NoBust" (no recession) are heavily imbalanced. This creates some problem during the optimization process. 
