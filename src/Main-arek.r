source("utils.r")
source("PrepareData.r")

raw_data = read.csv("../Datasets/JobChanges/aug_train.csv", header=T, sep=",", na.strings=c("","NA"))
raw_data <- raw_data[1:3000,]
data <- prepareData(raw_data)

## --------------------------------------------------------------------------------
## ---------------- Podzielenie zbioru na pule i zbior testowy --------------------
## --------------------------------------------------------------------------------

test_idx <- 0.8 * nrow(data) # ostatnie 20% danych to zbior testowy
data_pool <- data[1:test_idx,]
data_test <- data[-test_idx,]

## --------------------------------------------------------------------------------
## --Wybranie k próbek z puli do początkowego zbioru trenującego i podpisanie ich--
## --------------------------------------------------------------------------------

## set the seed to make your partition reproducible
set.seed(123)

onePercentOfSamples <- floor(0.1 * nrow(data_pool))
train_idx <- sample(seq_len(nrow(data_pool)), size = onePercentOfSamples)

training_set <- data_pool[train_idx, ]; # str(training_set)
validating_set <- data_pool[-train_idx, ]; # str(validating_set)
correct_answers <- validating_set$target
validating_set$target <- NULL

## ------------------------------------------------------------------------
## -----------Trenowanie modelu za pomocą zbioru trenującego---------------
## ------------------------------------------------------------------------

model <- svm(target ~ ., data = training_set, kernel = "linear")
print(model)

## --------------------------------------------------------------------------------
## --------------Predykcja na zbiorze walidacyjnym---------------
## --------------------------------------------------------------------------------

predict(model, newdata = validating_set)
pred <- predict(model, newdata = validating_set)
pred_rounded <-round(pred)
results <- data.frame(pred_rounded, correct_answers)
colnames(results) <- c('Predicted', 'Actual')

accuracy <- GetAccuracy(results)
print(accuracy)
print(paste("Accuracy:", gsub(" ", "", paste(accuracy * 100, "%"))))
