# Forecasting
This repository contains code to conduct a recession analysis, making heavy use of the »caret«-package. Three models are compared in their performance as recession predictors: Boosting (gbm), Random Forests (ranger) and simple Logistic Regression (glm). Using the yield curve and up to 10 lags, the probability of a recession is estimated.

Data for macro-variables is drawn from FRED:

Financial data is drawn from YahooFinance:
https://finance.yahoo.com/quote/%5EGSPC/history?p=%5EGSPC

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
