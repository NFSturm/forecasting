## Data Preprocessing
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

Nach dem Preprocessing der ökonomischen Daten können wir nun die Yield-Kurve der US-Treasury-Bills als Zeitreihe darstellen.

ggplot(monthly_spread_df, aes(x = Date, y = Spread)) + geom_line(col = "#9fd6ff") + theme_classic() + geom_hline(yintercept = 0) + theme(text = element_text(family = "Crimson", size = 12)) + labs(title = "Yield Spread von 1976 bis 2019", subtitle = "Monatlicher Durchschnitt", caption = "Quelle: FRED") + geom_rect(data = rec_dates, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

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
spread_data$Indicator <- as_factor(spread_data$Indicator)

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
                         classProbs = TRUE)

tuneLength.num <- 10

set.seed(28101997)
rf_mod <- train(Indicator~ . - Date,
                data = train_data,
                method = "ranger",
                trControl = TimeLord,
                tuneLength=tuneLength.num, metric = "AUC")

boost_mod <- train(Indicator~ . - Date,
                   data = train_data,
                   method = "gbm",
                   trControl = TimeLord,
                   tuneLength=tuneLength.num,
                   verbose = FALSE, metric = "AUC")

logistic_mod <- train(Indicator~ . - Date,
                      data = train_data,
                      method = "glm",
                      family = "binomial",
                      trControl = TimeLord,
                      tuneLength=tuneLength.num, metric = "AUC")

#stopCluster(cl)

set.seed(28101997)
resamps <- resamples(list(boost = boost_mod, ranger = rf_mod, logistic = logistic_mod))
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


Date_train <- train_data$Date
Date_train <- as_tibble(Date_train)


pred_rf <- as_tibble(predict.train(rf_mod, type = "prob"))
rf_train_pred <- bind_cols(Date = Date_train$value, Spread_Pred = pred_rf$Bust)
ggplot(rf_train_pred, aes(x = Date, y = Spread_Pred)) + geom_line(col = "#9fd6ff") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

pred_logistic <- as_tibble(predict.train(logistic_mod, type = "prob"))
logistic_train_pred <- bind_cols(Date = Date_train$value, Spread_Pred = pred_logistic$Bust)
ggplot(logistic_train_pred, aes(x = Date, y = Spread_Pred)) + geom_line(col = "#9fd6ff") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)

pred_boost <- as_tibble(predict.train(boost_mod, type = "prob"))
boost_train_pred <- bind_cols(Date = Date_train$value, Spread_Pred = pred_boost$Bust)
ggplot(boost_train_pred, aes(x = Date, y = Spread_Pred)) + geom_line(col = "#9fd6ff") + theme_classic() + theme(text = element_text(family = "Crimson", size = 12)) + geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.5, fill= "grey80", inherit.aes = FALSE)
