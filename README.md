# Forecasting

This repository contains code to conduct a recession analysis, making heavy use of the »caret«-package. Three models are compared in their performance as recession predictors: Boosting (gbm), Random Forests (ranger), Support Vector Machines (svmLinear and svmRadial), Decision Trees (rpart) as well as Logistic Regression (glm) as a benchmark.

Data for macro-variables is drawn from FRED:
* Consumer Confidence Index (CCI) https://fred.stlouisfed.org/series/CSCICP03USM665S
* Business Confidence Index (BCI) https://fred.stlouisfed.org/series/BSCICP03USM665S
* 10-Year Treasury Yield (10YCM) https://fred.stlouisfed.org/series/DGS10
* 3-Month Treasury Yield (Secondary Market) https://fred.stlouisfed.org/series/TB3MS
* Spot Crude Oil Price (WTI) https://fred.stlouisfed.org/series/WTISPLC

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
