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
library(readr)

#10Y-Treasury Yields
T10YCM <- read_excel("/Users/nfsturm/Documents/Forecasting/Dev/10YT-CM.xls", range = "A12:B15121", col_names = c("Date", "10Y-YIELD"))
T10YCM <- tk_tbl(T10YCM)
T10YCM_xts <- tk_xts(T10YCM)
T10YCM$Date <- as.yearmon(T10YCM$Date)
month.end <- endpoints(T10YCM_xts, on = "months")
month_by_day <- period.apply(T10YCM_xts, INDEX = month.end, FUN = mean)
monthly_T10YCM <- to.monthly(month_by_day)
monthly_T10YCM <- monthly_T10YCM$month_by_day.Open
colnames(monthly_T10YCM) <- "T10YCM"
Date <- tk_index(monthly_T10YCM)
Date_df <- tibble::enframe(Date)
T10YCM_df <- as_tibble(coredata(monthly_T10YCM$T10YCM))
monthly_T10YCM_df <- bind_cols(Date_df, T10YCM_df)
monthly_T10YCM_df$name <- NULL
colnames(monthly_T10YCM_df) <- c("Date", "T10YCM")
monthly_T10YCM_df <- monthly_T10YCM_df[-c(696),]

#3M-Treasury Yields
T3MSM <- read_excel("/Users/nfsturm/Documents/Forecasting/Dev/TB3MS.xls", range = "A348:B1042", col_names = c("Date", "3M-YIELD"))
T3MSM$Date <- as.yearmon(T3MSM$Date)

#Join I
spread_df <- bind_cols(monthly_T10YCM_df, T3MSM, .id = NULL)
spread_df$Date1 <- NULL

#S&P Index
SP500 <- read_csv("/Users/nfsturm/Documents/Forecasting/Dev/SP500.csv", col_names = TRUE)
colnames(SP500) = c("Date","Open","High","Low","Close","Adj_Close","Volume")
SP500$Date <- as.yearmon(SP500$Date)
SP500 <- select(SP500, c("Date", "Adj_Close"))

# Join II
df_0.3 <- left_join(spread_df, SP500, on = "Date")
df_0.3 <- df_0.3[-c(695),]

# Consumer Confidence Index (CCI)
CCI <- read_excel("/Users/nfsturm/Documents/Forecasting/Dev/CCIUSA.xls", range = "A12:B729", col_names = c("Date", "CCI"))
CCI$Date <- as.yearmon(CCI$Date)

# Join III
df_0.4 <- left_join(df_0.3, CCI, on = "Date")

# Business Confidence Index
BCI <- read_excel("/Users/nfsturm/Documents/Forecasting/Dev/CCIUSA.xls", range = "A12:B729", col_names = c("Date", "BCI"))
BCI$Date <- as.yearmon(BCI$Date)

# Join IV
df_0.5 <- left_join(df_0.4, BCI, on = "Date")
colnames(df_0.5) <- c("Date", "T10YCM", "T3MSM", "SP500", "CCI", "BCI")

# Bond-equivalence for T3MSM ++ Spread
df_0.5 <- df_0.5 %>% 
  mutate(T3MBE = 100*((365*T3MSM)/100)/(360-(91*T3MSM/100))) %>%
  mutate(Spread = T10YCM-T3MBE)

data <- select(df_0.5, c("Date", "Spread", "SP500", "CCI", "BCI"))

begin <- c("1970-01-01", "1973-12-01", "1980-02-01", "1981-08-01", "1990-08-01", "2001-04-01", "2008-01-01")
end <- c("1970-11-01", "1975-04-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01")
rec_dates <- tibble(begin, end)
rec_dates$begin <- as.Date(rec_dates$begin)
rec_dates$end <- as.Date(rec_dates$end)
data$Date <- as.Date(data$Date)

ggplot(data, aes(x = Date, y = Spread)) + geom_line(col = "#4CA3DD") + theme_classic() + geom_hline(yintercept = 0) + theme(text = element_text(family = "Crimson", size = 12)) + labs(title = "Yield Spread von 1976 bis 2019", subtitle = "Monatlicher Durchschnitt (10-Year Maturity - 2-Year Maturity)", caption = "Quelle: FRED") + 
  geom_rect(data = rec_dates, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

recession <- tibble(indicator = rep("NoBust", 694))
Date <- tibble::enframe(data$Date)
recession <- bind_cols(Date, recession)
recession$name <- NULL
colnames(recession) <- c("Date", "Indicator")
recession$Date <- as_date(recession$Date)

# Gemäß Klassifizierung des NBER werden nun die fünf Rezessionen seit 1975 eingetragen.
recession[recession$Date >= "2008-01-01" & recession$Date <= "2009-06-01",]$Indicator <- "Bust"
recession[recession$Date >= "2001-04-01" & recession$Date <= "2001-11-01",]$Indicator <- "Bust"
recession[recession$Date >= "1990-08-01" & recession$Date <= "1991-03-01",]$Indicator <- "Bust"
recession[recession$Date >= "1981-08-01" & recession$Date <= "1982-11-01",]$Indicator <- "Bust"
recession[recession$Date >= "1980-02-01" & recession$Date <= "1980-07-01",]$Indicator <- "Bust"
recession[recession$Date >= "1973-12-01" & recession$Date <= "1975-04-01",]$Indicator <- "Bust"
recession[recession$Date >= "1970-01-01" & recession$Date <= "1970-11-01",]$Indicator <- "Bust"

# Zusammenführung der Datensätze
data_rec <- bind_cols(data, Indicator = recession$Indicator)
data_rec$Indicator <- as.factor(data_rec$Indicator)

# Feature-Engineering with LagNo 12 (Train)
set.seed(28101997)
splits <- initial_time_split(data_rec, prop = 3/4)
train_data <- training(splits)
test_data <- testing(splits)

train_data <- setDT(train_data)[, paste("YieldLag", 12, sep = "") := shift(Spread, 12)][]
train_data <- setDT(train_data)[, paste("SP500Lag", 12, sep = "") := shift(SP500, 12)][]
train_data <- setDT(train_data)[, paste("CCILag", 12, sep = "") := shift(CCI, 12)][]
train_data <- setDT(train_data)[, paste("BCILag", 12, sep = "") := shift(BCI, 12)][]

train_data <- train_data %>%
  fill(c("SP500Lag12","YieldLag12", "CCILag12", "BCILag12"), .direction = "up") %>%
  select(Date, Indicator, SP500Lag12, CCILag12, BCILag12, YieldLag12)
train_data$Indicator <- relevel(train_data$Indicator, ref = "Bust")

# Feature-Engineering with LagNo 12 (Test)

test_data <- setDT(test_data)[, paste("YieldLag", 12, sep = "") := shift(Spread, 12)][]
test_data <- setDT(test_data)[, paste("SP500Lag", 12, sep = "") := shift(SP500, 12)][]
test_data <- setDT(test_data)[, paste("CCILag", 12, sep = "") := shift(CCI, 12)][]
test_data <- setDT(test_data)[, paste("BCILag", 12, sep = "") := shift(BCI, 12)][]

test_data <- test_data %>%
  fill(c("SP500Lag12","YieldLag12", "CCILag12", "BCILag12"), .direction = "up") %>%
  select(Date, Indicator, SP500Lag12, CCILag12, BCILag12, YieldLag12)

test_data$Indicator <- relevel(test_data$Indicator, ref = "Bust")

# Normalization

train_mean_YieldLag12 <- mean(train_data$YieldLag12)
train_mean_SP500Lag12 <- mean(train_data$SP500Lag12)
train_mean_CCILag12 <- mean(train_data$CCILag12)
train_mean_BCILag12 <- mean(train_data$BCILag12)

train_sd_YieldLag12 <- sd(train_data$YieldLag12)
train_sd_SP500Lag12 <- sd(train_data$SP500Lag12)
train_sd_CCILag12 <- sd(train_data$CCILag12)
train_sd_BCILag12 <- sd(train_data$BCILag12)

train_data$YieldLag12 <- (train_data$YieldLag12 - train_mean_YieldLag12)/(train_sd_YieldLag12)
train_data$SP500Lag12 <- (train_data$SP500Lag12 - train_mean_SP500Lag12)/(train_sd_SP500Lag12)
train_data$CCILag12 <- (train_data$CCILag12 - train_mean_CCILag12)/(train_sd_CCILag12)
train_data$BCILag12 <- (train_data$BCILag12 - train_mean_BCILag12)/(train_sd_BCILag12)

# Apply same scaler for test set

test_data$YieldLag12 <- (test_data$YieldLag12 - train_mean_YieldLag12)/(train_sd_YieldLag12)
test_data$SP500Lag12 <- (test_data$SP500Lag12 - train_mean_SP500Lag12)/(train_sd_SP500Lag12)
test_data$CCILag12 <- (test_data$CCILag12 - train_mean_CCILag12)/(train_sd_CCILag12)
test_data$BCILag12 <- (test_data$BCILag12 - train_mean_BCILag12)/(train_sd_BCILag12)

# Modellierung

Date_train <- train_data$Date
Date_train <- tibble::enframe(Date_train)
train_data$Date <- NULL

Date_test <- test_data$Date
Date_test <- tibble::enframe(Date_test)
test_data$Date <- NULL

# Warp-Antrieb
set.seed(28101997)
nr_cores <- detectCores() - 2
cl <- makeCluster(nr_cores) # Verwendung der Kernzahl minus 2
registerDoParallel(cl)

TimeLord <- trainControl(method = "timeslice",
                         initialWindow = 110,
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
                tuneLength=tuneLength_num, metric = "AUC")

set.seed(28101997)
boost_mod <- train(Indicator~ .,
                   data = train_data,
                   method = "gbm",
                   trControl = TimeLord,
                   tuneLength=tuneLength_num,
                   verbose = FALSE, metric = "AUC")

set.seed(28101997)
logistic_mod <- train(Indicator~ .,
                      data = train_data,
                      method = "glm",
                      family = "binomial",
                      trControl = TimeLord,
                      tuneLength=tuneLength_num, metric = "AUC")

set.seed(28101997)
svm_lin_mod <- train(Indicator~.,
                 data = train_data,
                 method = "svmLinear",
                 trControl = TimeLord,
                 tuneLength = tuneLength_num, metric = "AUC")

set.seed(28101997)
svm_radial_mod <- train(Indicator~.,
                        data = train_data,
                        method = "svmRadial",
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

begin <- c("1973-12-01", "1980-02-01", "1981-08-01", "1990-08-01")
end <- c("1975-04-01", "1980-07-01", "1982-11-01", "1991-03-01")
rec_dates_train <- tibble(begin, end)
rec_dates_train$begin <- as.Date(rec_dates_train$begin)
rec_dates_train$end <- as.Date(rec_dates_train$end)
data$Date <- as.Date(data$Date)

# Random Forest

rf_preds <- rf_mod$pred
rf_preds <- rf_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

rf_preds <- rf_preds[order(rf_preds$rowIndex),]
rf_preds <- rf_preds[!duplicated(rf_preds$rowIndex),]
Dates_holdout <- Date_train[111:462,]
rf_preds_dates <- bind_cols(Dates_holdout, rf_preds)
colnames(rf_preds_dates) <- c("Date", "Recession_Prob", "rowIndex")
ggplot(rf_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# Logistic Regression

logistic_preds <- logistic_mod$pred
logistic_preds <- logistic_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

logistic_preds <- logistic_preds[order(logistic_preds$rowIndex),]
logistic_preds <- logistic_preds[!duplicated(logistic_preds$rowIndex),]
Dates_holdout <- Date_train[111:462,]
logistic_preds_dates <- bind_cols(Dates_holdout, logistic_preds)
colnames(logistic_preds_dates) <- c("Date", "Recession_Prob", "rowIndex")
ggplot(logistic_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# Boosting

boosting_preds <- boost_mod$pred
boosting_preds <- boosting_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

boosting_preds <- boosting_preds[order(boosting_preds$rowIndex),]
boosting_preds <- boosting_preds[!duplicated(boosting_preds$rowIndex),]
Dates_holdout <- Date_train[111:462,]
boosting_preds_dates <- bind_cols(Dates_holdout, boosting_preds)
colnames(boosting_preds_dates) <- c("Date", "Recession_Prob", "rowIndex")
ggplot(boosting_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# SVM (Linear)

svm_lin_preds <- svm_lin_mod$pred
svm_lin_preds <- svm_lin_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

svm_lin_preds <- svm_lin_preds[order(svm_lin_preds$rowIndex),]
svm_lin_preds <- svm_lin_preds[!duplicated(svm_lin_preds$rowIndex),]
Dates_holdout <- Date_train[111:462,]
svm_lin_preds_dates <- bind_cols(Dates_holdout, svm_lin_preds)
colnames(svm_lin_preds_dates) <- c("Date", "Recession_Prob", "rowIndex")
ggplot(svm_lin_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# SVM (RBF)

svm_radial_preds <- svm_radial_mod$pred
svm_radial_preds <- svm_radial_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

svm_radial_preds <- svm_radial_preds[order(svm_radial_preds$rowIndex),]
svm_radial_preds <- svm_radial_preds[!duplicated(svm_radial_preds$rowIndex),]
Dates_holdout <- Date_train[111:462,]
svm_radial_preds_dates <- bind_cols(Dates_holdout, svm_radial_preds)
colnames(svm_radial_preds_dates) <- c("Date", "Recession_Prob", "rowIndex")
ggplot(svm_radial_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# Test Performance

rf_test_raw <- predict(rf_mod, test_data, type = "raw")
rf_test <- predict(rf_mod, test_data, type = "prob")
rf_cm <- confusionMatrix(rf_test_raw, test_data$Indicator)
recall_rf <- rf_cm$byClass[6]
precision_rf <- rf_cm$byClass[5]
rf <- list(recall_rf, precision_rf)

boost_test_raw <- predict(boost_mod, test_data, type = "raw")
boost_test <- predict(boost_mod, test_data, type = "prob")
boost_cm <- confusionMatrix(boost_test_raw, test_data$Indicator)
recall_boost <- boost_cm$byClass[6]
precision_boost <- boost_cm$byClass[5]
boost <- list(recall_boost, precision_boost)

logistic_test_raw <- predict(logistic_mod, test_data, type = "raw")
logistic_test <- predict(logistic_mod, test_data, type = "prob")
log_cm <- confusionMatrix(logistic_test_raw, test_data$Indicator)
recall_log <- log_cm$byClass[6]
precision_log <- log_cm$byClass[5]
logistic <- list(recall_log, precision_log)

svm_lin_test_raw <- predict(svm_lin_mod, test_data, type = "raw")
svm_lin_test <- predict(svm_lin_mod, test_data, type = "prob")
svm_lin_cm <- confusionMatrix(svm_lin_test_raw, test_data$Indicator)
recall_svm_lin <- svm_lin_cm$byClass[6]
precision_svm_lin <- svm_lin_cm$byClass[5]
svm_lin <- list(recall_svm_lin, precision_svm_lin)

svm_radial_test_raw <- predict(svm_radial_mod, test_data, type = "raw")
svm_radial_test <- predict(svm_radial_mod, test_data, type = "prob")
svm_radial_cm <- confusionMatrix(svm_radial_test_raw, test_data$Indicator)
recall_svm_radial <- svm_radial_cm$byClass[6]
precision_svm_radial <- svm_radial_cm$byClass[5]
svm_radial <- list(recall_svm_radial, precision_svm_radial)

# Visualizing test set performance

begin <- c("2001-04-01", "2008-01-01")
end <- c("2001-11-01", "2008-06-01")
rec_dates_test <- tibble(begin, end)
rec_dates_test$begin <- as.Date(rec_dates_test$begin)
rec_dates_test$end <- as.Date(rec_dates_test$end)

rf_dates_test <- bind_cols(Date_test, rf_test)
colnames(rf_dates_test) <- c("Date", "Recession_Prob", "Expansion_Prob")
ggplot(rf_dates_test, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

boost_dates_test <- bind_cols(Date_test, boost_test)
colnames(boost_dates_test) <- c("Date", "Recession_Prob", "Expansion_Prob")
ggplot(boost_dates_test, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

logistic_dates_test <- bind_cols(Date_test, logistic_test)
colnames(logistic_dates_test) <- c("Date", "Recession_Prob", "Expansion_Prob")
ggplot(logistic_dates_test, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

svm_lin_dates_test <- bind_cols(Date_test, svm_lin_test)
colnames(svm_lin_dates_test) <- c("Date", "Recession_Prob", "Expansion_Prob")
ggplot(svm_lin_dates_test, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

svm_radial_dates_test <- bind_cols(Date_test, svm_radial_test)
colnames(svm_radial_dates_test) <- c("Date", "Recession_Prob", "Expansion_Prob")
ggplot(svm_radial_dates_test, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)
