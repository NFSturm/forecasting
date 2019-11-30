# Data Preprocessing
set.seed(28101997) # Seed für Reproduzierbarkeit
library(readxl)
library(zoo)
library(timetk)
library(xts)
library(tidyquant)
library(ggplot2)
library(rsample)
library(caret)
library(quantmod)
library(dplyr)
library(doParallel)
library(MLmetrics)
library(data.table)
library(Metrics)
spread <- read_excel("/Users/nfsturm/Documents/Forecasting/Dev/YieldSpread.xls", range = "A12:B11353", col_names = c("Date", "Spread"))
spread <- tk_tbl(spread)
spread_xts <- tk_xts(spread)
spread$Date <- as.yearmon(spread$Date)
month.end <- endpoints(spread_xts, on = "months")
month_by_day <- period.apply(spread_xts, INDEX = month.end, FUN = mean)
monthly_spread <- to.monthly(month_by_day)
monthly_spread <- monthly_spread$month_by_day.Open
colnames(monthly_spread) <- "Spread"
Date <- tk_index(monthly_spread)
Date_df <- tibble::enframe(Date)
spread_df <- as_tibble(coredata(monthly_spread$Spread))
monthly_spread_df <- bind_cols(Date_df, spread_df)
monthly_spread_df$name <- NULL
colnames(monthly_spread_df) <- c("Date", "Spread")


begin <- c("1980-01-01", "1981-07-01", "1990-07-01", "2001-03-01", "2007-12-01")
end <- c("1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01")
rec_dates <- tibble(begin, end)
rec_dates$begin <- as.Date(rec_dates$begin)
rec_dates$end <- as.Date(rec_dates$end)
monthly_spread_df$Date <- as.Date(monthly_spread_df$Date)

ggplot(monthly_spread_df, aes(x = Date, y = Spread)) + geom_line(col = "#4CA3DD") + theme_classic() + geom_hline(yintercept = 0) + theme(text = element_text(family = "Crimson", size = 12)) + labs(title = "Yield Spread von 1976 bis 2019", subtitle = "Monatlicher Durchschnitt (10-Year Maturity - 2-Year Maturity)", caption = "Quelle: FRED") + 
  geom_rect(data = rec_dates, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

recession <- tibble(indicator = rep("NoBust", 522))
Date <- tibble::enframe(index(monthly_spread))
recession <- bind_cols(Date, recession)
recession$name <- NULL
colnames(recession) <- c("Date", "Indicator")
recession$Date <- as_date(recession$Date)

# Gemäß Klassifizierung des NBER werden nun die fünf Rezessionen seit 1975 eingetragen.
recession[recession$Date >= "2007-12-01" & recession$Date <= "2009-06-01",]$Indicator <- "Bust"
recession[recession$Date >= "1980-01-01" & recession$Date <= "1980-07-01",]$Indicator <- "Bust"
recession[recession$Date >= "1981-07-01" & recession$Date <= "1982-11-01",]$Indicator <- "Bust"
recession[recession$Date >= "1990-07-01" & recession$Date <= "1991-03-01",]$Indicator <- "Bust"
recession[recession$Date >= "2001-03-01" & recession$Date <= "2001-11-01",]$Indicator <- "Bust"

# Zusammenführung der Datensätze
spread_data <- bind_cols(list(recession$Date, monthly_spread_df$Spread, recession$Indicator))
colnames(spread_data) <- c("Date", "Spread", "Indicator")
spread_data$Indicator <- as.factor(spread_data$Indicator)

# Feature-Engineering mit 10 Lags
n = 10
spread_data <- setDT(spread_data)[, paste("Lag", 1:n, sep = "") := shift(Spread, 1:n)][]
spread_data <- spread_data %>%
  fill(Lag1:Lag10, .direction = "up")
spread_data$Indicator <- relevel(spread_data$Indicator, ref = "Bust")

## Modellierung

set.seed(28101997)
splits <- initial_time_split(spread_data, prop = 2/3)
train_data <- training(splits)
test_data <- testing(splits)

Date_train <- train_data$Date
Date_train <- as_tibble(Date_train)
train_data$Date <- NULL

Date_test <- test_data$Date
Date_test <- as_tibble(Date_test)
test_data$Date <- NULL

test_data$Indicator <- ifelse(test_data$Indicator == "Bust", 1, 0)

# Warp-Antrieb
set.seed(28101997)
nr_cores <- detectCores() - 2
cl <- makeCluster(nr_cores) # Verwendung der Kernzahl minus 2
registerDoParallel(cl)

TimeLord <- trainControl(method = "timeslice",
                         initialWindow = 50,
                         horizon = 12,
                         fixedWindow = FALSE,
                         allowParallel = TRUE,
                         summaryFunction = prSummary,
                         classProbs = TRUE,
                         savePredictions = "final")

tuneLength_num <- 10

set.seed(28101997)
rf_mod <- train(Indicator~ .,
                data = train_data,
                method = "ranger",
                trControl = TimeLord,
                preProcess = c("center", "scale"),
                tuneLength=tuneLength_num, metric = "AUC")

set.seed(28101997)
boost_mod <- train(Indicator~ .,
                   data = train_data,
                   method = "gbm",
                   trControl = TimeLord,
                   preProcess = c("center", "scale"),
                   tuneLength=tuneLength_num,
                   verbose = FALSE, metric = "AUC")

set.seed(28101997)
logistic_mod <- train(Indicator~ .,
                      data = train_data,
                      method = "glm",
                      family = "binomial",
                      preProcess = c("center", "scale"),
                      trControl = TimeLord,
                      tuneLength=tuneLength_num, metric = "AUC")

set.seed(28101997)
svm_lin_mod <- train(Indicator~.,
                 data = train_data,
                 method = "svmLinear",
                 preProcess = c("center", "scale"),
                 trControl = TimeLord,
                 tuneLength = tuneLength_num, metric = "AUC")

set.seed(28101997)
svm_radial_mod <- train(Indicator~.,
                        data = train_data,
                        method = "svmRadial",
                        preProcess = c("center", "scale"),
                        trControl = TimeLord,
                        tuneLength = tuneLength_num, metric = "AUC")

#stopCluster(cl)

set.seed(28101997)
resamps <- resamples(list(boost = boost_mod, ranger = rf_mod, logistic = logistic_mod, svm_lin = svm_lin_mod, svm_radial = svm_radial_mod))
sum_resamps <- summary(resamps)
sum_resamps

dotplot(resamps, metric = "Recall", main = "Recall nach Modell")
dotplot(resamps, metric = "Precision", main = "Precision nach Modell")
dotplot(resamps, metric = "AUC", main = "Precision-Recall-AUC nach Modell")

begin <- c("1980-01-01", "1981-07-01", "1990-07-01", "2001-03-01")
end <- c("1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01")
rec_dates_train <- tibble(begin, end)
rec_dates_train$begin <- as.Date(rec_dates_train$begin)
rec_dates_train$end <- as.Date(rec_dates_train$end)
monthly_spread_df$Date <- as.Date(monthly_spread_df$Date)

# Random Forest

rf_preds <- rf_mod$pred
rf_preds <- rf_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

rf_preds <- rf_preds[order(rf_preds$rowIndex),]
rf_preds <- rf_preds[!duplicated(rf_preds$rowIndex),]
Dates_holdout <- Date_train[51:348,]
rf_preds_dates <- bind_cols(Dates_holdout, rf_preds)
colnames(rf_preds_dates) <- c("Date", "Recession_Prob", "rowIndex")
ggplot(rf_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# Logistic Regression

logistic_preds <- logistic_mod$pred
logistic_preds <- logistic_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

logistic_preds <- logistic_preds[order(logistic_preds$rowIndex),]
logistic_preds <- logistic_preds[!duplicated(logistic_preds$rowIndex),]
Dates_holdout <- Date_train[51:348,]
logistic_preds_dates <- bind_cols(Dates_holdout, logistic_preds)
colnames(logistic_preds_dates) <- c("Date", "Recession_Prob", "rowIndex")
ggplot(logistic_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# Boosting

boosting_preds <- boost_mod$pred
boosting_preds <- boosting_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

boosting_preds <- boosting_preds[order(boosting_preds$rowIndex),]
boosting_preds <- boosting_preds[!duplicated(boosting_preds$rowIndex),]
Dates_holdout <- Date_train[51:348,]
boosting_preds_dates <- bind_cols(Dates_holdout, boosting_preds)
colnames(boosting_preds_dates) <- c("Date", "Recession_Prob", "rowIndex")
ggplot(boosting_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# SVM (Linear)

svm_lin_preds <- svm_lin_mod$pred
svm_lin_preds <- svm_lin_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

svm_lin_preds <- svm_lin_preds[order(svm_lin_preds$rowIndex),]
svm_lin_preds <- svm_lin_preds[!duplicated(svm_lin_preds$rowIndex),]
Dates_holdout <- Date_train[51:348,]
svm_lin_preds_dates <- bind_cols(Dates_holdout, svm_lin_preds)
colnames(svm_lin_preds_dates) <- c("Date", "Recession_Prob", "rowIndex")
ggplot(svm_lin_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# SVM (RBF)

svm_radial_preds <- svm_radial_mod$pred
svm_radial_preds <- svm_radial_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

svm_radial_preds <- svm_radial_preds[order(svm_radial_preds$rowIndex),]
svm_radial_preds <- svm_radial_preds[!duplicated(svm_radial_preds$rowIndex),]
Dates_holdout <- Date_train[51:348,]
svm_radial_preds_dates <- bind_cols(Dates_holdout, svm_radial_preds)
colnames(svm_radial_preds_dates) <- c("Date", "Recession_Prob", "rowIndex")
ggplot(svm_radial_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# Test Performance

rf_test <- predict(rf_mod, test_data, type = "raw")
rf_cm <- confusionMatrix(rf_test, test_data$Indicator)
recall_rf <- rf_cm$byClass[1]
precision_rf <- rf_cm$byClass[5]
rf <- list(recall_rf, precision_rf)

boost_test <- predict(boost_mod, test_data, type = "raw")
boost_cm <- confusionMatrix(boost_test, test_data$Indicator)
recall_boost <- boost_cm$byClass[6]
precision_boost <- boost_cm$byClass[5]
boost <- list(recall_boost, precision_boost)

logistic_test <- predict(logistic_mod, test_data, type = "raw")
log_cm <- confusionMatrix(logistic_test, test_data$Indicator)
recall_log <- log_cm$byClass[6]
precision_log <- log_cm$byClass[5]
logistic <- list(recall_log, precision_log)

svm_lin_test <- predict(svm_lin_mod, test_data, type = "raw")
svm_lin_cm <- confusionMatrix(svm_lin_test, test_data$Indicator)
recall_svm_lin <- svm_lin_cm$byClass[6]
precision_svm_lin <- svm_lin_cm$byClass[5]
svm_lin <- list(recall_svm_lin, precision_svm_lin)

svm_radial_test <- predict(svm_radial_mod, test_data, type = "raw")
svm_radial_cm <- confusionMatrix(svm_radial_test, test_data$Indicator)
recall_svm_radial <- svm_radial_cm$byClass[6]
precision_svm_radial <- svm_radial_cm$byClass[5]
svm_radial <- list(recall_svm_radial, precision_svm_radial)
