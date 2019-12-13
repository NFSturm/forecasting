# Forecasting

This repository contains code to conduct a recession analysis. Three models are compared in their performance as recession predictors: Boosting (gbm), Random Forests (ranger), Support Vector Machines (svmLinear and svmRadial), Decision Trees (rpart) as well as Logistic Regression (glm) as a benchmark.

Data from FRED:
* 10-Year Treasury Yield (10YCM) https://fred.stlouisfed.org/series/DGS10
* 3-Month Treasury Yield (Secondary Market) https://fred.stlouisfed.org/series/TB3MS
* Spot Crude Oil Price (WTI) https://fred.stlouisfed.org/series/WTISPLC
* Immediate Rates (Less than 24 hours) https://fred.stlouisfed.org/series/IRSTCI01USM156N

Quandl:
* Michigan Consumer Survey https://www.quandl.com/data/UMICH/SOC1-University-of-Michigan-Consumer-Survey-Index-of-Consumer-Sentiment
* PMI https://www.quandl.com/data/ISM/MAN_PMI-PMI-Composite-Index

Financial data is drawn from YahooFinance:
* S&P500 https://finance.yahoo.com/quote/%5EGSPC/history?p=%5EGSPC

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

**Note**: Since recessions rarely happen (luckily!), the classes "Bust" (for recession) and "NoBust" (no recession) are heavily imbalanced. This creates some problem during the optimization process. To mend this issue, the optimizier uses the "prSummary"-function, which focusses on precision and recall. 
