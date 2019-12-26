# Data Preprocessing
set.seed(28101997) # Seed for Reproducibility
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
library(rpart)
library(rpart.plot)

#10Y-Treasury Yields
T10YCM <- read_excel("/Users/nfsturm/Documents/Forecasting/Dev/Data/10YT-CM.xls", range = "A12:B15121", col_names = c("Date", "10Y-YIELD"))
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
T3MSM <- read_excel("/Users/nfsturm/Documents/Forecasting/Dev/Data/TB3MS.xls", range = "A348:B1042", col_names = c("Date", "3M-YIELD"))
T3MSM$Date <- as.yearmon(T3MSM$Date)

#Join I
spread_df <- bind_cols(monthly_T10YCM_df, T3MSM, .id = NULL)
spread_df$Date1 <- NULL

#S&P500 Index
SP500 <- read_csv("/Users/nfsturm/Documents/Forecasting/Dev/Data/SP500.csv", col_names = TRUE)
colnames(SP500) = c("Date","Open","High","Low","Close","Adj_Close","Volume")
SP500$Date <- as.yearmon(SP500$Date)
SP500 <- select(SP500, c("Date", "Adj_Close"))
SP500RE <- SP500 %>%
  mutate(Diff12M = Delt(Adj_Close, k = 12)) %>%
  na.omit(Diff12M) %>%
  select(Date, Diff12M)

# Join II
df_0.3 <- left_join(spread_df, SP500RE, on = "Date")
df_0.3 <- df_0.3[-c(695),]

# Michigan Consumer Confidence (MICS)
MICS <- read_csv("/Users/nfsturm/Documents/Forecasting/Dev/Data/UMICH-CS.csv", col_names = TRUE)
colnames(MICS) <- c("Date", "MICS")
MICS$Date <- as.yearmon(MICS$Date)
MICS <- MICS %>%
  mutate(MICS = Delt(MICS, k = 1)) %>%
  select(Date, MICS)

# Join III
df_0.4 <- left_join(df_0.3, MICS, on = "Date")

# Purchasing Manager Index (PMI)
PMI <- read_csv("/Users/nfsturm/Documents/Forecasting/Dev/Data/ISM_PMI.csv", col_names = TRUE)
PMI$Date <- as.yearmon(PMI$Date)
PMI <- PMI %>%
  mutate(PMI = Delt(PMI, k = 1)) %>%
  select(Date, PMI)

# Join IV
df_0.5 <- left_join(df_0.4, PMI, on = "Date")
colnames(df_0.5) <- c("Date", "T10YCM", "T3MSM", "SP500RE", "MICS", "PMI")

# Bond-equivalence for T3MSM ++ Spread
df_0.5 <- df_0.5 %>% 
  mutate(T3MBE = 100*((365*T3MSM)/100)/(360-(91*T3MSM/100))) %>%
  mutate(Spread = T10YCM-T3MBE)

# WTI Oil Price
OILWTI <- read_excel("/Users/nfsturm/Documents/Forecasting/Dev/Data/OILWTI.xls", range = "A180:B897", col_names = c("Date", "OILWTI"))
OILWTI$Date <- as.yearmon(OILWTI$Date)
OILWTI <- OILWTI %>%
  mutate(WTIDIFF1M = Delt(OILWTI, k = 1)) %>%
  select(Date, WTIDIFF1M)

# Join V
df_0.6 <- left_join(df_0.5, OILWTI, on = "Date")

# Effective Federal Funds Rate
FEDFUNDS <- read_excel("/Users/nfsturm/Documents/Forecasting/Dev/Data/FEDFUNDS.xls", range = "A12:B795", col_names = c("Date", "FEDFUNDS"))
FEDFUNDS$Date <- as.yearmon(FEDFUNDS$Date)

# Join VI
df_0.7 <- left_join(df_0.6, FEDFUNDS, on = "Date")

data <- select(df_0.7, c("Date", "Spread", "SP500RE", "MICS", "PMI", "WTIDIFF1M", "FEDFUNDS"))
data <- data %>%
  fill(MICS, .direction = "up")

begin <- c("1970-01-01", "1973-12-01", "1980-02-01", "1981-08-01", "1990-08-01", "2001-04-01", "2008-01-01")
end <- c("1970-11-01", "1975-04-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01")
rec_dates <- tibble(begin, end)
rec_dates$begin <- as.Date(rec_dates$begin)
rec_dates$end <- as.Date(rec_dates$end)
data$Date <- as.Date(data$Date)

# Plotting the Yield Spread
ggplot(data, aes(x = Date, y = Spread)) + geom_line(col = "#4CA3DD") + theme_classic() + geom_hline(yintercept = 0) + 
  theme(text = element_text(family = "Crimson", size = 12)) + labs(title = "Yield Spread von 1962 bis 2019", subtitle = "Monatlicher Durchschnitt (10-Year Maturity - 3-Month Maturity)", caption = "Quelle: FRED") + 
  geom_rect(data = rec_dates, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE) +
  theme(plot.title = element_text(size=22))

recession <- tibble(indicator = rep("NoBust", 694))
Date <- tibble::enframe(data$Date)
recession <- bind_cols(Date, recession)
recession$name <- NULL
colnames(recession) <- c("Date", "Indicator")
recession$Date <- as_date(recession$Date)

# NBER Business Cycles (Expansions & Contractions)
recession[recession$Date >= "2008-01-01" & recession$Date <= "2009-06-01",]$Indicator <- "Bust"
recession[recession$Date >= "2001-04-01" & recession$Date <= "2001-11-01",]$Indicator <- "Bust"
recession[recession$Date >= "1990-08-01" & recession$Date <= "1991-03-01",]$Indicator <- "Bust"
recession[recession$Date >= "1981-08-01" & recession$Date <= "1982-11-01",]$Indicator <- "Bust"
recession[recession$Date >= "1980-02-01" & recession$Date <= "1980-07-01",]$Indicator <- "Bust"
recession[recession$Date >= "1973-12-01" & recession$Date <= "1975-04-01",]$Indicator <- "Bust"
recession[recession$Date >= "1970-01-01" & recession$Date <= "1970-11-01",]$Indicator <- "Bust"

# Combination of datasets
data_rec <- bind_cols(data, Indicator = recession$Indicator)
data_rec$Indicator <- as.factor(data_rec$Indicator)

# Feature-Engineering with LagNo 12 (Train) - Simulates 12-month-ahead forecasting
set.seed(28101997)
splits <- initial_time_split(data_rec, prop = 3/4)
train <- training(splits)
test <- testing(splits)

# Standardization

Scaler <- preProcess(train, method = c("center", "scale"))
train_data <- predict(Scaler, train)
test_data <- predict(Scaler, test)

#LagNo <- 12 #Should be testted for 12, 6 and 3 months ahead
lag_nrs <- c(3,6, 12)
train_data <- setDT(train_data)[, paste0("SpreadLag", lag_nrs) := shift(Spread, lag_nrs)][]
train_data <- setDT(train_data)[, paste0("SP500RELag", lag_nrs) := shift(SP500RE,lag_nrs)][]
train_data <- setDT(train_data)[, paste0("MICSLag", lag_nrs) := shift(MICS, lag_nrs)][]
train_data <- setDT(train_data)[, paste0("PMILag", lag_nrs) := shift(PMI, lag_nrs)][]
train_data <- setDT(train_data)[, paste0("WTIDIFF1MLag", lag_nrs) := shift(WTIDIFF1M, lag_nrs)][]
train_data <- setDT(train_data)[, paste0("FEDFUNDSLag", lag_nrs) := shift(FEDFUNDS, lag_nrs)][]

train_data <- train_data %>%
  na.locf(na.rm = FALSE, fromLast = TRUE)

train_data$Indicator <- relevel(train_data$Indicator, ref = "Bust")

# Apply same lag to test data

test_data <- setDT(test_data)[, paste0("SpreadLag", lag_nrs) := shift(Spread, lag_nrs)][]
test_data <- setDT(test_data)[, paste0("SP500RELag", lag_nrs) := shift(SP500RE, lag_nrs)][]
test_data <- setDT(test_data)[, paste0("MICSLag", lag_nrs) := shift(MICS, lag_nrs)][]
test_data <- setDT(test_data)[, paste0("PMILag", lag_nrs) := shift(PMI, lag_nrs)][]
test_data <- setDT(test_data)[, paste0("WTIDIFF1MLag", lag_nrs) := shift(WTIDIFF1M, lag_nrs)][]
test_data <- setDT(test_data)[, paste0("FEDFUNDSLag", lag_nrs) := shift(FEDFUNDS, lag_nrs)][]

test_data <- test_data %>%
  na.locf(na.rm = FALSE, fromLast = TRUE)

test_data$Indicator <- relevel(test_data$Indicator, ref = "Bust")

# Extract dates
Date_train <- train_data$Date
Date_train <- tibble::enframe(Date_train)
train_data$Date <- NULL

Date_test <- test_data$Date
Date_test <- tibble::enframe(Date_test)
test_data$Date <- NULL

# Create 3 dataframes for different time horizons

train_3m <- train_data %>%
  select(Indicator, ends_with("3"))

train_6m <- train_data %>%
  select(Indicator, ends_with("6"))

train_12m <- train_data %>%
  select(Indicator, ends_with("12"))

# Same procedure for test data

test_3m <- test_data %>%
  select(Indicator, ends_with("3"))

test_6m <- test_data %>%
  select(Indicator, ends_with("6"))

test_12m <- test_data %>%
  select(Indicator, ends_with("12"))

# Warp-Drive: Engage!
set.seed(28101997)
nr_cores <- detectCores() - 2
cl <- makeCluster(nr_cores) # Core number minus 2
registerDoParallel(cl)

train_data <- train_12m
test_data <- test_12m

TimeLord <- trainControl(method = "timeslice",
                         initialWindow = 110,
                         horizon = 12,
                         fixedWindow = FALSE,
                         allowParallel = TRUE,
                         summaryFunction = prSummary,
                         classProbs = TRUE,
                         savePredictions = "final")

tuneLength_num <- 15

# Individual implementation
set.seed(28101997)

dectree_mod <- train(Indicator~ MICSLag12 + PMILag12 + SpreadLag12,
                     data = train_data,
                     method = "rpart",
                     trControl = TimeLord,
                     tuneLength=tuneLength_num, metric = "AUC")

boost_mod <- train(Indicator~ SpreadLag12 + SP500RELag12 + MICSLag12 + PMILag12,
                   data = train_data,
                   method = "gbm",
                   trControl = TimeLord,
                   tuneLength=tuneLength_num,
                   verbose = FALSE,
                   metric = "AUC")

logistic_mod <- train(Indicator~ SpreadLag12 + MICSLag12 + PMILag12,
                      data = train_data,
                      method = "glm",
                      family = binomial(link = "probit"),
                      trControl = TimeLord,
                      tuneLength=tuneLength_num, metric = "AUC")

svm_lin_mod <- train(Indicator~ SpreadLag12 + SP500RELag12 + MICSLag12 + PMILag12,
                 data = train_data,
                 method = "svmLinear",
                 trControl = TimeLord,
                 tuneLength = tuneLength_num, metric = "AUC")

svm_radial_mod <- train(Indicator~ SpreadLag12 + SP500RELag12 + MICSLag12 + PMILag12,
                        data = train_data,
                        method = "svmRadial",
                        trControl = TimeLord,
                        tuneLength = tuneLength_num, metric = "AUC")

#stopCluster(cl)

set.seed(28101997)
resamps <- resamples(list(boost = boost_mod, logistic = logistic_mod, svm_lin = svm_lin_mod, svm_radial = svm_radial_mod, decision_tree = dectree_mod))
sum_resamps <- summary(resamps)
sum_resamps

dotplot(resamps, metric = "Recall", main = "Recall nach Modell", par.settings=list(par.main.text=list(cex=2)))
dotplot(resamps, metric = "Precision", main = "Precision nach Modell", par.settings=list(par.main.text=list(cex=2)))
dotplot(resamps, metric = "AUC", main = "Precision-Recall-AUC nach Modell", par.settings=list(par.main.text=list(cex=2)))

begin <- c("1973-12-01", "1980-02-01", "1981-08-01", "1990-08-01", "2001-04-01")
end <- c("1975-04-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01")
rec_dates_train <- tibble(begin, end)
rec_dates_train$begin <- as.Date(rec_dates_train$begin)
rec_dates_train$end <- as.Date(rec_dates_train$end)
data$Date <- as.Date(data$Date)

# Decision Tree
dectree_preds <- dectree_mod$pred
dectree_preds <- dectree_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

dectree_preds <- dectree_preds[order(dectree_preds$rowIndex),]
dectree_preds <- dectree_preds[!duplicated(dectree_preds$rowIndex),]
Dates_holdout <- Date_train[111:520,]
dectree_preds_dates <- bind_cols(Dates_holdout, dectree_preds) %>%
  select(value, Recession_Prob)
colnames(dectree_preds_dates) <- c("Date", "Recession_Prob")
ggplot(dectree_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE) +
  labs(title = "Decision Tree Performance", subtitle = "CV-Hold-out-samples") + theme(plot.title = element_text(size=22))

# Rpart Plot 
rpart.plot(dectree_mod$finalModel, tweak = 1, type = 1)

# Logistic Regression

logistic_preds <- logistic_mod$pred
logistic_preds <- logistic_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

logistic_preds <- logistic_preds[order(logistic_preds$rowIndex),]
logistic_preds <- logistic_preds[!duplicated(logistic_preds$rowIndex),]
Dates_holdout <- Date_train[111:520,]
logistic_preds_dates <- bind_cols(Dates_holdout, logistic_preds) %>%
  select(value, Recession_Prob)
colnames(logistic_preds_dates) <- c("Date", "Recession_Prob")
ggplot(logistic_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE) +
  labs(title = "Logistic Regression Performance", subtitle = "CV-Hold-out-samples") + theme(plot.title = element_text(size=22))


# Boosting

boosting_preds <- boost_mod$pred
boosting_preds <- boosting_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

boosting_preds <- boosting_preds[order(boosting_preds$rowIndex),]
boosting_preds <- boosting_preds[!duplicated(boosting_preds$rowIndex),]
Dates_holdout <- Date_train[111:520,]
boosting_preds_dates <- bind_cols(Dates_holdout, boosting_preds) %>%
  select(value, Recession_Prob)
colnames(boosting_preds_dates) <- c("Date", "Recession_Prob")
ggplot(boosting_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE) + 
  labs(title = "Boosting Performance", subtitle = "CV-Hold-out-samples") + theme(plot.title = element_text(size=22))


# SVM (Linear)

svm_lin_preds <- svm_lin_mod$pred
svm_lin_preds <- svm_lin_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

svm_lin_preds <- svm_lin_preds[order(svm_lin_preds$rowIndex),]
svm_lin_preds <- svm_lin_preds[!duplicated(svm_lin_preds$rowIndex),]
Dates_holdout <- Date_train[111:520,]
svm_lin_preds_dates <- bind_cols(Dates_holdout, svm_lin_preds) %>%
  select(value, Recession_Prob)
colnames(svm_lin_preds_dates) <- c("Date", "Recession_Prob")
ggplot(svm_lin_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# SVM (RBF)

svm_radial_preds <- svm_radial_mod$pred
svm_radial_preds <- svm_radial_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

svm_radial_preds <- svm_radial_preds[order(svm_radial_preds$rowIndex),]
svm_radial_preds <- svm_radial_preds[!duplicated(svm_radial_preds$rowIndex),]
Dates_holdout <- Date_train[111:520,]
svm_radial_preds_dates <- bind_cols(Dates_holdout, svm_radial_preds) %>%
  select(value, Recession_Prob)
colnames(svm_radial_preds_dates) <- c("Date", "Recession_Prob")
ggplot(svm_radial_preds_dates, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# Test Performance

dectree_test_raw <- predict(dectree_mod, test_data, type = "raw")
dectree_test <- predict(dectree_mod, test_data, type = "prob")
dectree_cm <- confusionMatrix(dectree_test_raw, test_data$Indicator)
recall_dectree <- dectree_cm$byClass[6]
precision_dectree <- dectree_cm$byClass[5]
dectree <- list(recall_dectree, precision_dectree)
dectree

boost_test_raw <- predict(boost_mod, test_data, type = "raw")
boost_test <- predict(boost_mod, test_data, type = "prob")
boost_cm <- confusionMatrix(boost_test_raw, test_data$Indicator)
recall_boost <- boost_cm$byClass[6]
precision_boost <- boost_cm$byClass[5]
boost <- list(recall_boost, precision_boost)
boost

logistic_test_raw <- predict(logistic_mod, test_data, type = "raw")
logistic_test <- predict(logistic_mod, test_data, type = "prob")
log_cm <- confusionMatrix(logistic_test_raw, test_data$Indicator)
recall_log <- log_cm$byClass[6]
precision_log <- log_cm$byClass[5]
logistic <- list(recall_log, precision_log)
logistic

svm_lin_test_raw <- predict(svm_lin_mod, test_data, type = "raw")
svm_lin_test <- predict(svm_lin_mod, test_data, type = "prob")
svm_lin_cm <- confusionMatrix(svm_lin_test_raw, test_data$Indicator)
recall_svm_lin <- svm_lin_cm$byClass[6]
precision_svm_lin <- svm_lin_cm$byClass[5]
svm_lin <- list(recall_svm_lin, precision_svm_lin)
svm_lin

svm_radial_test_raw <- predict(svm_radial_mod, test_data, type = "raw")
svm_radial_test <- predict(svm_radial_mod, test_data, type = "prob")
svm_radial_cm <- confusionMatrix(svm_radial_test_raw, test_data$Indicator)
recall_svm_radial <- svm_radial_cm$byClass[6]
precision_svm_radial <- svm_radial_cm$byClass[5]
svm_radial <- list(recall_svm_radial, precision_svm_radial)
svm_radial

# Visualizing test set performance
# Could the models have predicted the Great Recession?

begin <- c("2008-01-01")
end <- c("2009-06-01")
rec_dates_test <- tibble(begin, end)
rec_dates_test$begin <- as.Date(rec_dates_test$begin)
rec_dates_test$end <- as.Date(rec_dates_test$end)

dectree_dates_test <- bind_cols(Date_test, dectree_test) %>%
  select(value, Bust)
colnames(dectree_dates_test) <- c("Date", "Recession_Prob")
ggplot(dectree_dates_test, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

boost_dates_test <- bind_cols(Date_test, boost_test) %>%
  select(value, Bust)
colnames(boost_dates_test) <- c("Date", "Recession_Prob")
ggplot(boost_dates_test, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

logistic_dates_test <- bind_cols(Date_test, logistic_test) %>%
  select(value, Bust)
colnames(logistic_dates_test) <- c("Date", "Recession_Prob")
ggplot(logistic_dates_test, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

svm_lin_dates_test <- bind_cols(Date_test, svm_lin_test) %>%
  select(value, Bust)
colnames(svm_lin_dates_test) <- c("Date", "Recession_Prob")
ggplot(svm_lin_dates_test, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

svm_radial_dates_test <- bind_cols(Date_test, svm_radial_test) %>%
  select(value, Bust)
colnames(svm_radial_dates_test) <- c("Date", "Recession_Prob")
ggplot(svm_radial_dates_test, aes(x = Date, y = Recession_Prob)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)
