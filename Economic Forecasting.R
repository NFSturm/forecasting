# Data Preparation and Loading of relevant packages
set.seed(28101997) # Set initial seed for reproducibility
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
library(gbm)
library(rpart)
library(kernlab)
library(caTools)
library(ROCR)

#10Y-Treasury Yields
T10YCM <- read_excel("/Users/nfsturm/Documents/Forecasting/Dev/Data/GS10.xls", range = "A12:B15121", col_names = c("Date", "10Y-YIELD"))
T10YCM$Date <- as.yearmon(T10YCM$Date)
T10YCM <- na.omit(T10YCM)

#3M-Treasury Yields
T3MSM <- read_excel("/Users/nfsturm/Documents/Forecasting/Dev/Data/TB3MS.xls", range = "A348:B1042", col_names = c("Date", "3M-YIELD"))
T3MSM$Date <- as.yearmon(T3MSM$Date)

#Join I
spread_df <- inner_join(T10YCM, T3MSM, on = "Date")

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
  mutate(WTI = Delt(OILWTI, k = 3)) %>%
  select(Date, WTI)

# Join V
df_0.6 <- left_join(df_0.5, OILWTI, on = "Date")

# Effective Federal Funds Rate
FEDFUNDS <- read_excel("/Users/nfsturm/Documents/Forecasting/Dev/Data/FEDFUNDS.xls", range = "A12:B795", col_names = c("Date", "FEDFUNDS"))
FEDFUNDS$Date <- as.yearmon(FEDFUNDS$Date)

# Join VI
df_0.7 <- left_join(df_0.6, FEDFUNDS, on = "Date")

data <- select(df_0.7, c("Date", "Spread", "SP500RE", "MICS", "PMI", "WTI", "FEDFUNDS"))
data <- data %>%
  fill(MICS, .direction = "up")

begin <- c("1970-01-01", "1973-12-01", "1980-02-01", "1981-08-01", "1990-08-01", "2001-04-01", "2008-01-01")
end <- c("1970-11-01", "1975-04-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01")
rec_dates <- tibble(begin, end)
rec_dates$begin <- as.Date(rec_dates$begin)
rec_dates$end <- as.Date(rec_dates$end)
data$Date <- as.Date(data$Date)

# Plotting the Yield Spread over time 
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

# Unification of datasets
data_rec <- bind_cols(data, Indicator = recession$Indicator)
data_rec$Indicator <- as.factor(data_rec$Indicator)
saveRDS(data_rec, "data_recessions.RDS")

# Train-Test-Split
set.seed(28101997)
splits <- initial_time_split(data_rec, prop = 3/4)
train <- training(splits)
test <- testing(splits)

# Standardization of variables

Scaler <- preProcess(train, method = c("center", "scale"))
train_data <- predict(Scaler, train)
test_data <- predict(Scaler, test)

#Feature-Engineering with LagNo 3,6,12 for different forecasting horizons
lag_nrs <- c(3,6, 12)
train_data <- setDT(train_data)[, paste0("SpreadLag", lag_nrs) := shift(Spread, lag_nrs)][]
train_data <- setDT(train_data)[, paste0("SP500RELag", lag_nrs) := shift(SP500RE,lag_nrs)][]
train_data <- setDT(train_data)[, paste0("MICSLag", lag_nrs) := shift(MICS, lag_nrs)][]
train_data <- setDT(train_data)[, paste0("PMILag", lag_nrs) := shift(PMI, lag_nrs)][]
train_data <- setDT(train_data)[, paste0("WTILag", lag_nrs) := shift(WTI, lag_nrs)][]
train_data <- setDT(train_data)[, paste0("FEDFUNDSLag", lag_nrs) := shift(FEDFUNDS, lag_nrs)][]

train_data <- train_data %>%
  na.locf(na.rm = FALSE, fromLast = TRUE)

train_data$Indicator <- relevel(train_data$Indicator, ref = "Bust")

# Same lag for test data

test_data <- setDT(test_data)[, paste0("SpreadLag", lag_nrs) := shift(Spread, lag_nrs)][]
test_data <- setDT(test_data)[, paste0("SP500RELag", lag_nrs) := shift(SP500RE, lag_nrs)][]
test_data <- setDT(test_data)[, paste0("MICSLag", lag_nrs) := shift(MICS, lag_nrs)][]
test_data <- setDT(test_data)[, paste0("PMILag", lag_nrs) := shift(PMI, lag_nrs)][]
test_data <- setDT(test_data)[, paste0("WTILag", lag_nrs) := shift(WTI, lag_nrs)][]
test_data <- setDT(test_data)[, paste0("FEDFUNDSLag", lag_nrs) := shift(FEDFUNDS, lag_nrs)][]

test_data <- test_data %>%
  na.locf(na.rm = FALSE, fromLast = TRUE)

test_data$Indicator <- relevel(test_data$Indicator, ref = "Bust")

# Coercion of data type 

train_data <- as_tibble(train_data)
test_data <- as_tibble(test_data)

# Extraction of dates
Date_train <- train_data$Date
Date_train <- tibble::enframe(Date_train)
train_data$Date <- NULL

Date_test <- test_data$Date
Date_test <- tibble::enframe(Date_test)
test_data$Date <- NULL

# Creating three data frames for different time horizons 

train_3m <- train_data %>%
  select(Indicator, ends_with("3"))

train_6m <- train_data %>%
  select(Indicator, ends_with("6"))

train_12m <- train_data %>%
  select(Indicator, ends_with("12"))

# Same for test data

test_3m <- test_data %>%
  select(Indicator, ends_with("3"))

test_6m <- test_data %>%
  select(Indicator, ends_with("6"))

test_12m <- test_data %>%
  select(Indicator, ends_with("12"))

# Creating control structure for caret
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

tuneLength_num <- 20

testdat <- ifelse(test_data$Indicator == "Bust", 1,0)

# PRAUC Function 
# Accessed from: https://github.com/andybega/auc-pr/blob/master/auc-pr.r

# AUC Function
auc_pr <- function(obs, pred) {
  xx.df <- prediction(pred, obs)
  perf  <- performance(xx.df, "prec", "rec")
  xy    <- data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])
  
  # take out division by 0 for lowest threshold
  xy <- subset(xy, !is.nan(xy$precision))
  
  # Designate recall = 0 as precision = x...arbitrary
  xy <- rbind(c(0, 0), xy)
  #xy <- xy[!(rowSums(xy)==0), ]
  
  res   <- trapz(xy$recall, xy$precision)
  res
}

# getVarComb-Funktion

getVarCombs <- function(df, y) {
  if(typeof(df) != "list") stop("Input must be a dataframe or similar structure")
  if(typeof(y) != "character") stop("Dependent variable must be specified as a character string")
  
  df_vars <- df[!(names(df) %in% y)]
  var_num <- length(names(df_vars))
  vars <- names(df_vars)
  
  getCombs <- function(vars, m) {
    obj <- combn(vars, m)
    
    out <- c()
    for (combn in 1:ncol(obj)) {
      vc <- paste0(obj[,combn], collapse = "+")
      out <- append(out, vc)
    }
    formula <- c()
    for (form in out) {
      fm <- paste0(y, " ~ ", form)
      formula <- append(formula, fm)
    }
    formula  
  }  
  
  all_combs <- c()
  for (i in 1:var_num) {
    combs <- getCombs(vars, m = i)
    all_combs <- append(all_combs, combs)
  }
  
  all_combs
}

var_combs <- getVarCombs(train_12m, y = "Indicator")

# Decision Tree Models

model_list <- list()
test_list <- list()
j <- 1
i <- 1 
for (comb in var_combs) {
  model <- train(formula(comb),
                 data = train_data,
                 method = "rpart",
                 trControl = TimeLord,
                 tuneLength=tuneLength_num, metric = "AUC")
  
  preds <- predict(model, test_data, type = "prob")
  preds_raw <- predict(model, test_data, type = "raw")
  auc <- auc_pr(testdat, preds$Bust)
  model_cm <- confusionMatrix(preds_raw, test_data$Indicator, mode = "prec_recall")
  recall_model <- model_cm$byClass["Recall"]
  precision_model <- model_cm$byClass["Precision"]
  test_res <- list(recall_model, precision_model, auc)
  
  test_list[[j]] <- test_res
  j <- j + 1
  
  model_list[[i]] <- model
  i <- i + 1
  
}

names(model_list) <- paste("dectree_mod", 1:63, sep = "_") 
names(test_list) <- paste("dectree_test_preds", 1:63, sep = "_")

for(i in 1:length(model_list)) {
  mod = model_list[[i]]
  mod[1] = 0
  assign(paste0("dectree_mod", i), mod)
}

insample_dectree <- summary(resamples(model_list))

outsample_dectree <- test_list

# Analysis of insample objects enables the modeller to create a ranking of best input combinations 

model_list <- list()
test_list <- list()
j <- 1
i <- 1 
for (comb in var_combs) {
  model <- train(formula(comb),
                 data = train_data,
                 method = "gbm",
                 trControl = TimeLord,
                 tuneLength=tuneLength_num, metric = "AUC",
                 verbose = FALSE)
  
  preds <- predict(model, test_data, type = "prob")
  preds_raw <- predict(model, test_data, type = "raw")
  auc <- auc_pr(testdat, preds$Bust)
  model_cm <- confusionMatrix(preds_raw, test_data$Indicator, mode = "prec_recall")
  recall_model <- model_cm$byClass["Recall"]
  precision_model <- model_cm$byClass["Precision"]
  test_res <- list(recall_model, precision_model, auc)
  
  test_list[[j]] <- test_res
  j <- j + 1
  
  model_list[[i]] <- model
  i <- i + 1
  
}

names(model_list) <- paste("boost_mod", 1:63, sep = "_") 
names(test_list) <- paste("boost_test_preds", 1:63, sep = "_")

for(i in 1:length(model_list)) {
  mod = model_list[[i]]
  mod[1] = 0
  assign(paste0("boost_mod", i), mod)
}

insample_boost <- summary(resamples(model_list))

outsample_boost <- test_list


# Logistic Regression

model_list <- list()
test_list <- list()
j <- 1
i <- 1 
for (comb in var_combs) {
  model <- train(formula(comb),
                 data = train_data,
                 method = "glm",
                 trControl = TimeLord,
                 tuneLength=tuneLength_num, metric = "AUC",
                 family = binomial(link = "probit"))
  
  preds <- predict(model, test_data, type = "prob")
  preds_raw <- predict(model, test_data, type = "raw")
  auc <- auc_pr(testdat, preds$Bust)
  model_cm <- confusionMatrix(preds_raw, test_data$Indicator, mode = "prec_recall")
  recall_model <- model_cm$byClass["Recall"]
  precision_model <- model_cm$byClass["Precision"]
  test_res <- list(recall_model, precision_model, auc)
  
  test_list[[j]] <- test_res
  j <- j + 1
  
  model_list[[i]] <- model
  i <- i + 1
  
}

names(model_list) <- paste("logistic_mod", 1:63, sep = "_") 
names(test_list) <- paste("logistic_test_preds", 1:63, sep = "_")

for(i in 1:length(model_list)) {
  mod = model_list[[i]]
  mod[1] = 0
  assign(paste0("logistic_mod", i), mod)
}

insample_logistic <- summary(resamples(model_list))

outsample_logistic <- test_list

# SVM (Linear)

model_list <- list()
test_list <- list()
j <- 1
i <- 1 
for (comb in var_combs) {
  model <- train(formula(comb),
                 data = train_data,
                 method = "svmLinear",
                 trControl = TimeLord,
                 tuneLength=tuneLength_num, metric = "AUC")
  
  preds <- predict(model, test_data, type = "prob")
  preds_raw <- predict(model, test_data, type = "raw")
  auc <- auc_pr(testdat, preds$Bust)
  model_cm <- confusionMatrix(preds_raw, test_data$Indicator, mode = "prec_recall")
  recall_model <- model_cm$byClass["Recall"]
  precision_model <- model_cm$byClass["Precision"]
  test_res <- list(recall_model, precision_model, auc)
  
  test_list[[j]] <- test_res
  j <- j + 1
  
  model_list[[i]] <- model
  i <- i + 1
  
}

names(model_list) <- paste("svmlin_mod", 1:63, sep = "_") 
names(test_list) <- paste("svmlin_test_preds", 1:63, sep = "_")

for(i in 1:length(model_list)) {
  mod = model_list[[i]]
  mod[1] = 0
  assign(paste0("svmlin_mod", i), mod)
}

insample_svmlin <- summary(resamples(model_list))

outsample_svmlin <- test_list

# No SVM model with a linear kernel achieved a positive test set result for a 50% threshold

# Due to computational reasons, the tuning length for SVM (Radial) was reduced to 6. 
tuneLength_num_rad <- 6

model_list <- list()
test_list <- list()
j <- 1
i <- 1 
for (comb in var_combs) {
  model <- train(formula(comb),
                 data = train_data,
                 method = "rpart",
                 trControl = TimeLord,
                 tuneLength=tuneLength_num, metric = "AUC")
  
  preds <- predict(model, test_data, type = "prob")
  preds_raw <- predict(model, test_data, type = "raw")
  auc <- auc_pr(testdat, preds$Bust)
  model_cm <- confusionMatrix(preds_raw, test_data$Indicator, mode = "prec_recall")
  recall_model <- model_cm$byClass["Recall"]
  precision_model <- model_cm$byClass["Precision"]
  test_res <- list(recall_model, precision_model, auc)
  
  test_list[[j]] <- test_res
  j <- j + 1
  
  model_list[[i]] <- model
  i <- i + 1
  
}

names(model_list) <- paste("svmrad_mod", 1:63, sep = "_") 
names(test_list) <- paste("svmrad_test_preds", 1:63, sep = "_")

for(i in 1:length(model_list)) {
  mod = model_list[[i]]
  mod[1] = 0
  assign(paste0("svmrad_mod", i), mod)
}

insample_svmrad <- summary(resamples(model_list))

outsample_svmrad <- test_list

#stopCluster(cl) # Stopping computing cluster

begin <- c("1973-12-01", "1980-02-01", "1981-08-01", "1990-08-01", "2001-04-01")
end <- c("1975-04-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01")
rec_dates_train <- tibble(begin, end)
rec_dates_train$begin <- as.Date(rec_dates_train$begin)
rec_dates_train$end <- as.Date(rec_dates_train$end)
data$Date <- as.Date(data$Date)

# Decision Tree
dectree_preds <- dectree_mod_train$pred
dectree_preds <- dectree_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

dectree_preds <- dectree_preds[order(dectree_preds$rowIndex),]
dectree_preds <- dectree_preds[!duplicated(dectree_preds$rowIndex),]
Dates_holdout <- Date_train[111:520,]
dectree_preds_dates <- bind_cols(Dates_holdout, dectree_preds) %>%
  select(value, Recession_Prob)
colnames(dectree_preds_dates) <- c("Datum", "P_Rezession")
ggplot(dectree_preds_dates, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# Rpart Plot 
rpart.plot(dectree_mod_train$finalModel, tweak = 1.2, type = 1)

# Logistic Regression

logistic_preds <- logistic_mod_train$pred
logistic_preds <- logistic_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

logistic_preds <- logistic_preds[order(logistic_preds$rowIndex),]
logistic_preds <- logistic_preds[!duplicated(logistic_preds$rowIndex),]
Dates_holdout <- Date_train[111:520,]
logistic_preds_dates <- bind_cols(Dates_holdout, logistic_preds) %>%
  select(value, Recession_Prob)
colnames(logistic_preds_dates) <- c("Datum", "P_Rezession")
ggplot(logistic_preds_dates, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# Boosting

boosting_preds <- boost_mod_train$pred
boosting_preds <- boosting_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

boosting_preds <- boosting_preds[order(boosting_preds$rowIndex),]
boosting_preds <- boosting_preds[!duplicated(boosting_preds$rowIndex),]
Dates_holdout <- Date_train[111:520,]
boosting_preds_dates <- bind_cols(Dates_holdout, boosting_preds) %>%
  select(value, Recession_Prob)
colnames(boosting_preds_dates) <- c("Datum", "P_Rezession")
ggplot(boosting_preds_dates, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# SVM (Linear)

svm_lin_preds <- svm_lin_mod_train$pred
svm_lin_preds <- svm_lin_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

svm_lin_preds <- svm_lin_preds[order(svm_lin_preds$rowIndex),]
svm_lin_preds <- svm_lin_preds[!duplicated(svm_lin_preds$rowIndex),]
Dates_holdout <- Date_train[111:520,]
svm_lin_preds_dates <- bind_cols(Dates_holdout, svm_lin_preds) %>%
  select(value, Recession_Prob)
colnames(svm_lin_preds_dates) <- c("Datum", "P_Rezession")
ggplot(svm_lin_preds_dates, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# SVM (RBF)

svm_radial_preds <- svm_radial_mod_train$pred
svm_radial_preds <- svm_radial_preds %>%
  select(Bust, rowIndex) %>%
  rename(Recession_Prob = Bust)

svm_radial_preds <- svm_radial_preds[order(svm_radial_preds$rowIndex),]
svm_radial_preds <- svm_radial_preds[!duplicated(svm_radial_preds$rowIndex),]
Dates_holdout <- Date_train[111:520,]
svm_radial_preds_dates <- bind_cols(Dates_holdout, svm_radial_preds) %>%
  select(value, Recession_Prob)
colnames(svm_radial_preds_dates) <- c("Datum", "P_Rezession")
ggplot(svm_radial_preds_dates, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + 
  theme(text = element_text(family = "Crimson", size = 15)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# Test Performance

dectree_test_raw <- predict(dectree_mod_test, test_data, type = "raw")
dectree_test <- predict(dectree_mod_test, test_data, type = "prob")
dectree_cm <- confusionMatrix(dectree_test_raw, test_data$Indicator)
dectree_df <- as_tibble(dectree_test, test_data$Indicator)
recall_dectree <- dectree_cm$byClass[6]
precision_dectree <- dectree_cm$byClass[5]
dectree <- list(recall_dectree, precision_dectree)
dectree

boost_test_raw <- predict(boost_mod_test, test_data, type = "raw")
boost_test <- predict(boost_mod_test, test_data, type = "prob")
boost_cm <- confusionMatrix(boost_test_raw, test_data$Indicator)
boost_df <- as_tibble(boost_test, test_data$Indicator)
recall_boost <- boost_cm$byClass[6]
precision_boost <- boost_cm$byClass[5]
boost <- list(recall_boost, precision_boost)
boost

logistic_test_raw <- predict(logistic_mod_test, test_data, type = "raw")
logistic_test <- predict(logistic_mod_test, test_data, type = "prob")
log_cm <- confusionMatrix(logistic_test_raw, test_data$Indicator)
log_df <- as_tibble(logistic_test, test_data$Indicator)
recall_log <- log_cm$byClass[6]
precision_log <- log_cm$byClass[5]
logistic <- list(recall_log, precision_log)
logistic

svm_lin_test_raw <- predict(svm_lin_mod_test, test_data, type = "raw")
svm_lin_test <- predict(voyager_mod30, test_data, type = "prob")
svm_lin_cm <- confusionMatrix(svm_lin_test_raw, test_data$Indicator)
recall_svm_lin <- svm_lin_cm$byClass[6]
precision_svm_lin <- svm_lin_cm$byClass[5]
svm_lin <- list(recall_svm_lin, precision_svm_lin)
svm_lin

svm_radial_test_raw <- predict(svm_radial_mod_test, test_data, type = "raw")
svm_radial_test <- predict(borgcube_mod60, test_data, type = "prob")
svm_radial_cm <- confusionMatrix(svm_radial_test_raw, test_data$Indicator)
svm_radial_df <- as_tibble(svm_radial_test, test_data$Indicator)

recall_svm_radial <- svm_radial_cm$byClass[6]
precision_svm_radial <- svm_radial_cm$byClass[5]
svm_radial <- list(recall_svm_radial, precision_svm_radial)
svm_radial

# Plots of test set performance 

begin <- c("2008-01-01")
end <- c("2009-06-01")
rec_dates_test <- tibble(begin, end)
rec_dates_test$begin <- as.Date(rec_dates_test$begin)
rec_dates_test$end <- as.Date(rec_dates_test$end)

dectree_dates_test <- bind_cols(Date_test, dectree_test) %>%
  select(value, Bust)
colnames(dectree_dates_test) <- c("Datum", "P_Rezession")
ggplot(dectree_dates_test, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

boost_dates_test <- bind_cols(Date_test, boost_test) %>%
  select(value, Bust)
colnames(boost_dates_test) <- c("Datum", "P_Rezession")
ggplot(boost_dates_test, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

logistic_dates_test <- bind_cols(Date_test, logistic_test) %>%
  select(value, Bust)
colnames(logistic_dates_test) <- c("Datum", "P_Rezession")
ggplot(logistic_dates_test, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

svm_lin_dates_test <- bind_cols(Date_test, svm_lin_test) %>%
  select(value, Bust)
colnames(svm_lin_dates_test) <- c("Datum", "P_Rezession")
ggplot(svm_lin_dates_test, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

svm_radial_dates_test <- bind_cols(Date_test, svm_radial_test) %>%
  select(value, Bust)
colnames(svm_radial_dates_test) <- c("Datum", "P_Rezession")
ggplot(svm_radial_dates_test, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_test, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

# Creating models on complete data

cp_data <- data_rec
all_dates <- cp_data$Date
cp_data$Date <- NULL

lag_nr <- 12
cp_data <- setDT(cp_data)[, paste0("SpreadLag", lag_nr) := shift(Spread, lag_nr)][]
cp_data <- setDT(cp_data)[, paste0("SP500RELag", lag_nr) := shift(SP500RE,lag_nr)][]
cp_data <- setDT(cp_data)[, paste0("MICSLag", lag_nr) := shift(MICS, lag_nr)][]
cp_data <- setDT(cp_data)[, paste0("PMILag", lag_nr) := shift(PMI, lag_nr)][]
cp_data <- setDT(cp_data)[, paste0("WTILag", lag_nr) := shift(WTI, lag_nr)][]
cp_data <- setDT(cp_data)[, paste0("FEDFUNDSLag", lag_nr) := shift(FEDFUNDS, lag_nr)][]

cp_data <- cp_data %>%
  select(Indicator, ends_with("12")) %>%
  na.locf(na.rm = FALSE, fromLast = TRUE)

Scaler <- preProcess(cp_data, method = c("center", "scale"))
cp_data <- predict(Scaler, cp_data)

# Decision Tree

best_tree <- rpart(Indicator~ SpreadLag12 + MICSLag12 + PMILag12, data = cp_data, control = rpart.control(cp = 0.01275917))
best_tree_preds <- predict(best_tree, cp_data)
best_tree_preds <- best_tree_preds[,1]
best_tree_df <- bind_cols(Datum = all_dates, P_Rezession = best_tree_preds)

# Boost
cp_data_bern <- cp_data
cp_data_bern$Indicator <- ifelse(cp_data_bern$Indicator == "Bust", 1,0)
best_boost <- gbm(Indicator ~ SpreadLag12 + SP500RELag12 + MICSLag12, data = cp_data_bern, distribution = "bernoulli", n.trees = 950, interaction.depth = 7, shrinkage = 0.1, n.minobsinnode = 10, verbose = FALSE)
best_boost_preds <- predict(best_boost, cp_data, n.trees = 950, type = "response")
best_boost_df <- bind_cols(Datum = all_dates, P_Rezession = best_boost_preds)

# Probit

best_probit <- glm(Indicator~ SpreadLag12 + SP500RELag12 + MICSLag12 + PMILag12, family = binomial(link = "probit"), data = cp_data_bern)
best_probit_preds <- predict(best_probit, cp_data, type = "response")
best_probit_df <- bind_cols(Datum = all_dates, P_Rezession = best_probit_preds)

# SVM Linear

best_svmlin <- ksvm(Indicator~., data = cp_data, C = 1, kernel = "vanilladot", prob.model = TRUE)
best_svmlin_preds <- predict(best_svmlin, cp_data, type = "probabilities")
best_svmlin_preds <- best_svmlin_preds[, 1]
best_svmlin_df <- bind_cols(Datum = all_dates, P_Rezession = best_svmlin_preds)

# SVM Radial 

best_svmrad <- ksvm(Indicator~., data = cp_data, kernel = "rbfdot", C = 4, kpar = list(sigma = 1.542669), prob.model = TRUE)
best_svmrad_preds <- predict(best_svmrad, cp_data, type = "probabilities")
best_svmrad_preds <- best_svmrad_preds[, 1]
best_svmrad_df <- bind_cols(Datum = all_dates, P_Rezession = best_svmrad_preds)

# Performance on complete data

begin <- c("1970-01-01", "1973-12-01", "1980-02-01", "1981-08-01", "1990-08-01", "2001-04-01", "2008-01-01")
end <- c("1970-11-01", "1975-04-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01")
rec_dates_all <- tibble(begin, end)
rec_dates_all$begin <- as.Date(rec_dates_all$begin)
rec_dates_all$end <- as.Date(rec_dates_all$end)

best_tree_plot <- bind_cols(Datum = all_dates, P_Rezession = best_tree_df) %>%
  select(Datum, P_Rezession)
colnames(best_tree_plot) <- c("Datum", "P_Rezession")
ggplot(best_tree_plot, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_all, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

best_boost_plot <- bind_cols(Datum = all_dates, P_Rezession = best_boost_df) %>%
  select(Datum, P_Rezession)
colnames(best_boost_plot) <- c("Datum", "P_Rezession")
ggplot(best_boost_plot, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_all, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

best_logistic_plot <- bind_cols(Datum = all_dates, P_Rezession = best_probit_df) %>%
  select(Datum, P_Rezession)
colnames(best_logistic_plot) <- c("Datum", "P_Rezession")
ggplot(best_logistic_plot, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_all, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

best_svmlin_plot <- bind_cols(Datum = all_dates, P_Rezession = best_svmlin_df) %>%
  select(Datum, P_Rezession)
colnames(best_svmlin_plot) <- c("Datum", "P_Rezession")
ggplot(best_svmlin_plot, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_all, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

best_svmrad_plot <- bind_cols(Datum = all_dates, P_Rezession = best_svmrad_df) %>%
  select(Datum, P_Rezession)
colnames(best_svmrad_plot) <- c("Datum", "P_Rezession")
ggplot(best_svmrad_plot, aes(x = Datum, y = P_Rezession)) + geom_line(col = "#4CA3DD") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + 
  geom_rect(data = rec_dates_all, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

