source("utils.r")
source("PrepareData.r")

k <- 10

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

initial_train_size <- floor(0.01 * nrow(data_pool))
train_idx <- sample(seq_len(nrow(data_pool)), size = initial_train_size)

training_set <- data_pool[train_idx, ]; # str(training_set)
validating_set <- data_pool[-train_idx, ]; # str(validating_set)

## ------------------------------------------------------------------------
## -----------Trenowanie modelu za pomocą zbioru trenującego---------------
## ------------------------------------------------------------------------

model <- svm(target ~ ., data = training_set, kernel = "linear", probability = TRUE)
print(model)

## --------------------------------------------------------------------------------
## ----------------------Predykcja na zbiorze walidacyjnym-------------------------
## --------------------------------------------------------------------------------

pred_validation <- predict(model, validating_set, probability = TRUE)

## --------------------------------------------------------------------------------
## -----------Użycie modelu na zbiorze testowym i zmierzenie wydajności------------
## --------------------------------------------------------------------------------

pred_test <- predict(model, data_test)
results <- data.frame(pred_test, data_test$target)
colnames(results) <- c('Predicted', 'Actual')

accuracy <- GetAccuracy(results)
print(accuracy)
print(paste("Accuracy:", gsub(" ", "", paste(accuracy * 100, "%"))))

## --------------------------------------------------------------------------------
## -----------Wybranie k próbek niosących najwięcej informacji---------------------
## --------------------------------------------------------------------------------

probabs <- attr(pred_validation, "probabilities");
scores <- abs(probabs[,1] - probabs[,2])

scores_df <- data.frame(1:nrow(validating_set), probabs, scores)
colnames(scores_df) <- c('Index', 'Probability', 'Score')
scores_df <- scores_df[order(scores),]
scores_df_k <- scores_df[1:k,]

chosen_samples <- validating_set[scores_df_k$Index,]


# --------------------------------------------------------------------------------
## -----Przeniesienie wybranych próbek ze zbioru walidacyjnego do treningowego----
## -------------------------------------------------------------------------------


