library(caret)
library(MLmetrics)
library(doParallel)
library(e1071)
library(svmRadialSigma)
set.seed(28101997)
nr_cores <- detectCores()
cl <- makeCluster(nr_cores)
registerDoParallel(cl)

train_data <- train_12m
test_data <- test_12m

TimeLord <- trainControl(method = "timeslice",
                         initialWindow = 110,
                         horizon = 12,
                         fixedWindow = FALSE,
                         allowParallel = FALSE,
                         summaryFunction = prSummary,
                         classProbs = TRUE,
                         savePredictions = "final")

tuneLength_num <- 15

set.seed(28101997)
borgcube_mod1 <- caret::train(Indicator ~ SpreadLag12,
                              data = train_data,
                              method = "svmRadialSigma", 
                              trControl = TimeLord,
                              tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod1)
res
borgcube_test_raw <- predict(borgcube_mod1, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod1, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube1 <- list(recall_borgcube, precision_borgcube)
borgcube1

borgcube_mod2 <- caret::train(Indicator ~ SP500RELag12,
                              data = train_data,
                              method = "svmRadialSigma", 
                              trControl = TimeLord,
                              tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod2)
res
borgcube_test_raw <- predict(borgcube_mod2, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod2, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube2 <- list(recall_borgcube, precision_borgcube)
borgcube2

borgcube_mod3 <- caret::train(Indicator ~ MICSLag12,
                              data = train_data,
                              method = "svmRadialSigma", 
                              trControl = TimeLord,
                              tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod3)
res
borgcube_test_raw <- predict(borgcube_mod3, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod3, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube3 <- list(recall_borgcube, precision_borgcube)
borgcube3

borgcube_mod4 <- caret::train(Indicator ~ PMILag12,
                              data = train_data,
                              method = "svmRadialSigma", 
                              trControl = TimeLord,
                              tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod4)
res
borgcube_test_raw <- predict(borgcube_mod4, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod4, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube4 <- list(recall_borgcube, precision_borgcube)
borgcube4

borgcube_mod5 <- caret::train(Indicator ~ WTILag12,
                              data = train_data,
                              method = "svmRadialSigma", 
                              trControl = TimeLord,
                              tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod5)
res
borgcube_test_raw <- predict(borgcube_mod5, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod5, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube5 <- list(recall_borgcube, precision_borgcube)
borgcube5

borgcube_mod6 <- caret::train(Indicator ~ FEDFUNDSLag12,
                              data = train_data,
                              method = "svmRadialSigma", 
                              trControl = TimeLord,
                              tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod6)
res
borgcube_test_raw <- predict(borgcube_mod6, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod6, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube6 <- list(recall_borgcube, precision_borgcube)
borgcube6

borgcube_mod7 <- caret::train(Indicator ~ SpreadLag12 + SP500RELag12,
                              data = train_data,
                              method = "svmRadialSigma", 
                              trControl = TimeLord,
                              tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod7)
res
borgcube_test_raw <- predict(borgcube_mod7, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod7, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube7 <- list(recall_borgcube, precision_borgcube)
borgcube7

borgcube_mod8 <- caret::train(Indicator ~ SpreadLag12 + PMILag12,
                              data = train_data,
                              method = "svmRadialSigma", 
                              trControl = TimeLord,
                              tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod8)
res
borgcube_test_raw <- predict(borgcube_mod8, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod8, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube8 <- list(recall_borgcube, precision_borgcube)
borgcube8

borgcube_mod9 <- caret::train(Indicator ~ SpreadLag12 + MICSLag12,
                              data = train_data,
                              method = "svmRadialSigma", 
                              trControl = TimeLord,
                              tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod9)
res
borgcube_test_raw <- predict(borgcube_mod9, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod9, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube9 <- list(recall_borgcube, precision_borgcube)
borgcube9


borgcube_mod10 <- caret::train(Indicator ~ SpreadLag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod10)
res
borgcube_test_raw <- predict(borgcube_mod10, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod10, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube10 <- list(recall_borgcube, precision_borgcube)
borgcube10

borgcube_mod11 <- caret::train(Indicator ~ SpreadLag12 + FEDFUNDSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod11)
res
borgcube_test_raw <- predict(borgcube_mod11, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod11, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube11 <- list(recall_borgcube, precision_borgcube)
borgcube11

borgcube_mod12 <- caret::train(Indicator ~ SP500RELag12 + MICSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod12)
res
borgcube_test_raw <- predict(borgcube_mod12, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod12, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube12 <- list(recall_borgcube, precision_borgcube)
borgcube12

borgcube_mod13 <- caret::train(Indicator ~ SP500RELag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod13)
res
borgcube_test_raw <- predict(borgcube_mod13, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod13, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube13 <- list(recall_borgcube, precision_borgcube)
borgcube13

borgcube_mod14 <- caret::train(Indicator ~ SP500RELag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod14)
res
borgcube_test_raw <- predict(borgcube_mod14, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod14, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube14 <- list(recall_borgcube, precision_borgcube)
borgcube14

borgcube_mod15 <- caret::train(Indicator ~ SP500RELag12 + FEDFUNDSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod15)
res
borgcube_test_raw <- predict(borgcube_mod15, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod15, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube15 <- list(recall_borgcube, precision_borgcube)
borgcube15

borgcube_mod16 <- caret::train(Indicator ~ MICSLag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod16)
res
borgcube_test_raw <- predict(borgcube_mod16, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod16, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube16 <- list(recall_borgcube, precision_borgcube)
borgcube16

borgcube_mod17 <- caret::train(Indicator ~ MICSLag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod17)
res
borgcube_test_raw <- predict(borgcube_mod17, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod17, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube17 <- list(recall_borgcube, precision_borgcube)
borgcube17

borgcube_mod18 <- caret::train(Indicator ~ MICSLag12 + FEDFUNDSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod18)
res
borgcube_test_raw <- predict(borgcube_mod18, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod18, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube18 <- list(recall_borgcube, precision_borgcube)
borgcube18

borgcube_mod19 <- caret::train(Indicator ~ WTILag12 + FEDFUNDSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod19)
res
borgcube_test_raw <- predict(borgcube_mod19, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod19, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube19 <- list(recall_borgcube, precision_borgcube)
borgcube19

borgcube_mod20 <- caret::train(Indicator ~ WTILag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod20)
res
borgcube_test_raw <- predict(borgcube_mod20, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod20, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube20 <- list(recall_borgcube, precision_borgcube)
borgcube20

borgcube_mod21 <- caret::train(Indicator ~ FEDFUNDSLag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod21)
res
borgcube_test_raw <- predict(borgcube_mod21, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod21, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube21 <- list(recall_borgcube, precision_borgcube)
borgcube21

borgcube_mod22 <- caret::train(Indicator ~ SpreadLag12 + SP500RELag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod22)
res
borgcube_test_raw <- predict(borgcube_mod22, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod22, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube22 <- list(recall_borgcube, precision_borgcube)
borgcube22

borgcube_mod23 <- caret::train(Indicator ~ SpreadLag12 + SP500RELag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod23)
res
borgcube_test_raw <- predict(borgcube_mod23, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod23, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube23 <- list(recall_borgcube, precision_borgcube)
borgcube23

borgcube_mod24 <- caret::train(Indicator ~ SpreadLag12 + SP500RELag12 + MICSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod24)
res
borgcube_test_raw <- predict(borgcube_mod24, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod24, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube24 <- list(recall_borgcube, precision_borgcube)
borgcube24

borgcube_mod25 <- caret::train(Indicator ~ SpreadLag12 + SP500RELag12 + FEDFUNDSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod25)
res
borgcube_test_raw <- predict(borgcube_mod25, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod25, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube25 <- list(recall_borgcube, precision_borgcube)
borgcube25

borgcube_mod26 <- caret::train(Indicator ~ SpreadLag12 + SP500RELag12 + MICSLag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod26)
res
borgcube_test_raw <- predict(borgcube_mod26, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod26, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube26 <- list(recall_borgcube, precision_borgcube)
borgcube26

borgcube_mod27 <- caret::train(Indicator ~ SpreadLag12 + MICSLag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod27)
res
borgcube_test_raw <- predict(borgcube_mod27, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod27, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube27 <- list(recall_borgcube, precision_borgcube)
borgcube27

borgcube_mod28 <- caret::train(Indicator ~ WTILag12 + MICSLag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod28)
res
borgcube_test_raw <- predict(borgcube_mod28, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod28, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube28 <- list(recall_borgcube, precision_borgcube)
borgcube28

borgcube_mod29 <- caret::train(Indicator ~ FEDFUNDSLag12 + MICSLag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod29)
res
borgcube_test_raw <- predict(borgcube_mod29, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod29, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube29 <- list(recall_borgcube, precision_borgcube)
borgcube29

borgcube_mod30 <- caret::train(Indicator ~ FEDFUNDSLag12 + WTILag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod30)
res
borgcube_test_raw <- predict(borgcube_mod30, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod30, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube30 <- list(recall_borgcube, precision_borgcube)
borgcube30

borgcube_mod31 <- caret::train(Indicator ~ SpreadLag12 + MICSLag12 + SP500RELag12 + WTILag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod31)
res
borgcube_test_raw <- predict(borgcube_mod31, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod31, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube31 <- list(recall_borgcube, precision_borgcube)
borgcube31

borgcube_mod32 <- caret::train(Indicator ~ SpreadLag12 + MICSLag12 + SP500RELag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod32)
res
borgcube_test_raw <- predict(borgcube_mod32, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod32, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube32 <- list(recall_borgcube, precision_borgcube)
borgcube32

borgcube_mod33 <- caret::train(Indicator ~ PMILag12 + MICSLag12 + SP500RELag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod33)
res
borgcube_test_raw <- predict(borgcube_mod33, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod33, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube33 <- list(recall_borgcube, precision_borgcube)
borgcube33

borgcube_mod34 <- caret::train(Indicator ~ FEDFUNDSLag12 + SP500RELag12 + PMILag12 + SpreadLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod34)
res
borgcube_test_raw <- predict(borgcube_mod34, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod34, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube34 <- list(recall_borgcube, precision_borgcube)
borgcube34

borgcube_mod35 <- caret::train(Indicator ~ PMILag12 + MICSLag12 + SP500RELag12 + FEDFUNDSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod35)
res
borgcube_test_raw <- predict(borgcube_mod35, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod35, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube35 <- list(recall_borgcube, precision_borgcube)
borgcube35

borgcube_mod36 <- caret::train(Indicator ~ WTILag12 + MICSLag12 + SP500RELag12 + FEDFUNDSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod36)
res
borgcube_test_raw <- predict(borgcube_mod36, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod36, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube36 <- list(recall_borgcube, precision_borgcube)
borgcube36

borgcube_mod37 <- caret::train(Indicator ~.,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod37)
res
borgcube_test_raw <- predict(borgcube_mod37, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod37, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube37 <- list(recall_borgcube, precision_borgcube)
borgcube37

borgcube_mod38 <- caret::train(Indicator ~ WTILag12 + PMILag12 + SP500RELag12 + FEDFUNDSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod38)
res
borgcube_test_raw <- predict(borgcube_mod38, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod38, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube38 <- list(recall_borgcube, precision_borgcube)
borgcube38

borgcube_mod39 <- caret::train(Indicator ~ SpreadLag12 + FEDFUNDSLag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod39)
res
borgcube_test_raw <- predict(borgcube_mod39, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod39, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube39 <- list(recall_borgcube, precision_borgcube)
borgcube39

borgcube_mod40 <- caret::train(Indicator ~ SpreadLag12 + FEDFUNDSLag12 + MICSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod40)
res
borgcube_test_raw <- predict(borgcube_mod40, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod40, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube40 <- list(recall_borgcube, precision_borgcube)
borgcube40

borgcube_mod41 <- caret::train(Indicator ~SpreadLag12 + FEDFUNDSLag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod41)
res
borgcube_test_raw <- predict(borgcube_mod41, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod41, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube41 <- list(recall_borgcube, precision_borgcube)
borgcube41

borgcube_mod42 <- caret::train(Indicator ~ SpreadLag12 + PMILag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod42)
res
borgcube_test_raw <- predict(borgcube_mod42, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod42, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube42 <- list(recall_borgcube, precision_borgcube)
borgcube42

borgcube_mod43 <- caret::train(Indicator ~ SpreadLag12 + MICSLag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod43)
res
borgcube_test_raw <- predict(borgcube_mod43, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod43, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube43 <- list(recall_borgcube, precision_borgcube)
borgcube43

borgcube_mod44 <- caret::train(Indicator ~ FEDFUNDSLag12 + SP500RELag12 + PMILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod44)
res
borgcube_test_raw <- predict(borgcube_mod44, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod44, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube44 <- list(recall_borgcube, precision_borgcube)
borgcube44

borgcube_mod45 <- caret::train(Indicator ~ FEDFUNDSLag12 + SP500RELag12 + MICSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod45)
res
borgcube_test_raw <- predict(borgcube_mod45, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod45, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube45 <- list(recall_borgcube, precision_borgcube)
borgcube45

borgcube_mod46 <- caret::train(Indicator ~ WTILag12 + SP500RELag12 + FEDFUNDSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod46)
res
borgcube_test_raw <- predict(borgcube_mod46, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod46, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube46 <- list(recall_borgcube, precision_borgcube)
borgcube46

borgcube_mod47 <- caret::train(Indicator ~ MICSLag12 + WTILag12 + FEDFUNDSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod47)
res
borgcube_test_raw <- predict(borgcube_mod47, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod47, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube47 <- list(recall_borgcube, precision_borgcube)
borgcube47

borgcube_mod48 <- caret::train(Indicator ~ SP500RELag12 + PMILag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod48)
res
borgcube_test_raw <- predict(borgcube_mod48, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod48, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube48 <- list(recall_borgcube, precision_borgcube)
borgcube48

borgcube_mod49 <- caret::train(Indicator ~ SP500RELag12 + MICSLag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod49)
res
borgcube_test_raw <- predict(borgcube_mod49, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod49, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube49 <- list(recall_borgcube, precision_borgcube)
borgcube49

borgcube_mod50 <- caret::train(Indicator ~ SpreadLag12 + FEDFUNDSLag12 + SP500RELag12 + MICSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod50)
res
borgcube_test_raw <- predict(borgcube_mod50, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod50, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube50 <- list(recall_borgcube, precision_borgcube)
borgcube50

borgcube_mod51 <- caret::train(Indicator ~ SpreadLag12 + FEDFUNDSLag12 + SP500RELag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod51)
res
borgcube_test_raw <- predict(borgcube_mod51, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod51, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube51 <- list(recall_borgcube, precision_borgcube)
borgcube51

borgcube_mod52 <- caret::train(Indicator ~ SpreadLag12 + FEDFUNDSLag12 + PMILag12 + MICSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod52)
res
borgcube_test_raw <- predict(borgcube_mod52, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod52, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube52 <- list(recall_borgcube, precision_borgcube)
borgcube52

borgcube_mod53 <- caret::train(Indicator ~ SpreadLag12 + FEDFUNDSLag12 + PMILag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod53)
res
borgcube_test_raw <- predict(borgcube_mod53, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod53, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube53 <- list(recall_borgcube, precision_borgcube)
borgcube53

borgcube_mod54 <- caret::train(Indicator ~ SpreadLag12 + FEDFUNDSLag12 + MICSLag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod54)
res
borgcube_test_raw <- predict(borgcube_mod54, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod54, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube54 <- list(recall_borgcube, precision_borgcube)
borgcube54

borgcube_mod55 <- caret::train(Indicator ~ SpreadLag12 + SP500RELag12 + PMILag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod55)
res
borgcube_test_raw <- predict(borgcube_mod55, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod55, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube55 <- list(recall_borgcube, precision_borgcube)
borgcube55

borgcube_mod56 <- caret::train(Indicator ~ FEDFUNDSLag12 + SP500RELag12 + MICSLag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod56)
res
borgcube_test_raw <- predict(borgcube_mod56, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod56, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube56 <- list(recall_borgcube, precision_borgcube)
borgcube56

borgcube_mod57 <- caret::train(Indicator ~ FEDFUNDSLag12 + PMILag12 + MICSLag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod57)
res
borgcube_test_raw <- predict(borgcube_mod57, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod57, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube57 <- list(recall_borgcube, precision_borgcube)
borgcube57

borgcube_mod58 <- caret::train(Indicator ~ SP500RELag12 + PMILag12 + MICSLag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod58)
res
borgcube_test_raw <- predict(borgcube_mod58, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod58, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube58 <- list(recall_borgcube, precision_borgcube)
borgcube58

borgcube_mod59 <- caret::train(Indicator ~ SpreadLag12 + FEDFUNDSLag12 + SP500RELag12 + PMILag12 + MICSLag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod59)
res
borgcube_test_raw <- predict(borgcube_mod59, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod59, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube59 <- list(recall_borgcube, precision_borgcube)
borgcube59

borgcube_mod60 <- caret::train(Indicator ~ SpreadLag12 + FEDFUNDSLag12 + SP500RELag12 + PMILag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod60)
res
borgcube_test_raw <- predict(borgcube_mod60, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod60, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube60 <- list(recall_borgcube, precision_borgcube)
borgcube60

borgcube_mod61 <- caret::train(Indicator ~ SpreadLag12 + FEDFUNDSLag12 + SP500RELag12 + MICSLag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod61)
res
borgcube_test_raw <- predict(borgcube_mod61, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod61, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube61 <- list(recall_borgcube, precision_borgcube)
borgcube61

borgcube_mod62 <- caret::train(Indicator ~ SpreadLag12 + FEDFUNDSLag12 + PMILag12 + MICSLag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod62)
res
borgcube_test_raw <- predict(borgcube_mod62, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod62, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube62 <- list(recall_borgcube, precision_borgcube)
borgcube62

borgcube_mod63 <- caret::train(Indicator ~ SP500RELag12 + FEDFUNDSLag12 + PMILag12 + MICSLag12 + WTILag12,
                               data = train_data,
                               method = "svmRadialSigma", 
                               trControl = TimeLord,
                               tuneLength=tuneLength_num,   metric = "AUC")

res <- confusionMatrix(borgcube_mod63)
res
borgcube_test_raw <- predict(borgcube_mod63, test_data, type = "raw")
borgcube_test <- predict(borgcube_mod63, test_data, type = "prob")
borgcube_cm <- confusionMatrix(borgcube_test_raw, test_data$Indicator, mode = "prec_recall")
recall_borgcube <- borgcube_cm$byClass[6]
precision_borgcube <- borgcube_cm$byClass[5]
borgcube63 <- list(recall_borgcube, precision_borgcube)
borgcube63

outsample_svmrad <- sapply(ls(pattern = "borgcube\\d+"), get)


insample_svmrad <- resamples(list(borgcube_mod1 ,borgcube_mod2 ,borgcube_mod3 ,borgcube_mod4 ,borgcube_mod5 ,borgcube_mod6 ,borgcube_mod7 ,borgcube_mod8 ,borgcube_mod9 ,borgcube_mod10 ,borgcube_mod11 ,borgcube_mod12,
                                  borgcube_mod13 ,borgcube_mod14 ,borgcube_mod15 ,borgcube_mod16 ,borgcube_mod17 ,borgcube_mod18,
                                  borgcube_mod19 ,borgcube_mod20 ,borgcube_mod21 ,borgcube_mod22 ,borgcube_mod23 ,borgcube_mod24,
                                  borgcube_mod25 ,borgcube_mod26 ,borgcube_mod27 ,borgcube_mod28 ,borgcube_mod29 ,borgcube_mod30,
                                  borgcube_mod31 ,borgcube_mod32 ,borgcube_mod33 ,borgcube_mod34 ,borgcube_mod35 ,borgcube_mod36,
                                  borgcube_mod37 ,borgcube_mod38 ,borgcube_mod39 ,borgcube_mod40 ,borgcube_mod41 ,borgcube_mod42,
                                  borgcube_mod43 ,borgcube_mod44 ,borgcube_mod45 ,borgcube_mod46 ,borgcube_mod47 ,borgcube_mod48,
                                  borgcube_mod49 ,borgcube_mod50 ,borgcube_mod51 ,borgcube_mod52 ,borgcube_mod53 ,borgcube_mod54,
                                  borgcube_mod55 ,borgcube_mod56 ,borgcube_mod57 ,borgcube_mod58 ,borgcube_mod59 ,borgcube_mod60,
                                  borgcube_mod61 ,borgcube_mod62 ,borgcube_mod63))

saveRDS(outsample_svmrad, "outsample_svmrad.RDS")
saveRDS(insample_svmrad, "insample_svmrad.RDS")