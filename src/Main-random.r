rm(list = setdiff(ls(), "data"))

source("utils.r")
source("PrepareData.r")
library("ggplot2");


k <- 1

raw_data = read.csv("../Datasets/JobChanges/aug_train.csv", header=T, sep=",", na.strings=c("","NA"))
data <- prepareData(raw_data)

## --------------------------------------------------------------------------------
## ---------------- Podzielenie zbioru na pule i zbior testowy --------------------
## --------------------------------------------------------------------------------

test_idx <- 0.8 * nrow(data) # ostatnie 20% danych to zbior testowy
data_pool <- data[1:test_idx,]
data_test <- data[-test_idx,]
#data_test <- data[1:1000,]

## --------------------------------------------------------------------------------
## --Wybranie k próbek z puli do początkowego zbioru trenującego i podpisanie ich--
## --------------------------------------------------------------------------------

## set the seed to make your partition reproducible
set.seed(1235)

initial_train_size <- 2
#train_idx <- sample(seq_len(nrow(data_pool)), size = initial_train_size)

training_set <- data_pool[1:initial_train_size, ]; # str(training_set)
validating_set <- data_pool[-initial_train_size, ]; # str(validating_set)
training_set_random <- training_set;
training_set_conf <- training_set;
validating_set_random <- validating_set;
validating_set_conf <- validating_set;
accuracies_random <- c()
accuracies_conf <- c()
sample_count <- c()
for (i in 1:100) {
  
  ## ------------------------------------------------------------------------
  ## -----------Trenowanie modelu za pomocą zbioru trenującego---------------
  ## ------------------------------------------------------------------------
  
  model_random <- svm(target ~ ., data = training_set_random, kernel = "linear", probability = TRUE)
  model_conf <- svm(target ~ ., data = training_set_conf, kernel = "linear", probability = TRUE)
  print(model_random)
  print(model_conf)
  
  ## --------------------------------------------------------------------------------
  ## ----------------------Predykcja na zbiorze walidacyjnym-------------------------
  ## --------------------------------------------------------------------------------
  
  pred_validation_random <- predict(model_random, validating_set_random, probability = TRUE)
  pred_validation_conf <- predict(model_conf, validating_set_conf, probability = TRUE)
  
  ## --------------------------------------------------------------------------------
  ## -----------Użycie modelu na zbiorze testowym i zmierzenie wydajności------------
  ## --------------------------------------------------------------------------------
  
  pred_test_random <- predict(model_random, data_test)
  pred_test_conf <- predict(model_conf, data_test)
  results_random <- data.frame(pred_test_random, data_test$target)
  results_conf <- data.frame(pred_test_conf, data_test$target)
  colnames(results_random) <- c('Predicted', 'Actual')
  colnames(results_conf) <- c('Predicted', 'Actual')
  
  accuracy_random <- GetAccuracy(results_random)
  accuracy_conf <- GetAccuracy(results_conf)
  
  accuracies_random <- append(accuracies_random, accuracy_random)
  accuracies_conf <- append(accuracies_conf, accuracy_conf)
  sample_count <- append(sample_count, initial_train_size + k * i)
  
  print(paste("Accuracy Random:", gsub(" ", "", paste(accuracy_random * 100, "%"))))
  print(paste("Accuracy Confidence:", gsub(" ", "", paste(accuracy_conf * 100, "%"))))
  
  ## --------------------------------------------------------------------------------
  ## -----------Wybranie k próbek niosących najwięcej informacji---------------------
  ## --------------------------------------------------------------------------------
  
  probabs_random <- attr(pred_validation_random, "probabilities");
  probabs_conf <- attr(pred_validation_conf, "probabilities");
  scores_random <- abs(probabs_random[,1] - probabs_random[,2])
  scores_conf <- abs(probabs_conf[,1] - probabs_conf[,2])
  

  scores_random_df <- data.frame(1:nrow(validating_set_random), scores_random)
  scores_conf_df <- data.frame(1:nrow(validating_set_conf), scores_conf)
  colnames(scores_random_df) <- c('Index', 'Score')
  colnames(scores_conf_df) <- c('Index', 'Score')

  scores_random_df <-scores_random_df[sample(nrow(scores_random_df)),] # shuffle data
  scores_conf_df <-scores_conf_df[order(scores_conf_df$Score),] # sort data
  scores_random_df_k <- scores_random_df[1:k,]
  scores_conf_df_k <- scores_conf_df[1:k,]
  
  chosen_samples_random <- validating_set[scores_random_df_k$Index,]
  chosen_samples_conf <- validating_set[scores_conf_df_k$Index,]
  
  print(paste("training_random data size:", nrow(training_set_random)))
  print(paste("training_conf data size:", nrow(training_set_conf)))
  
  # --------------------------------------------------------------------------------
  ## -----Przeniesienie wybranych próbek ze zbioru walidacyjnego do treningowego----
  ## -------------------------------------------------------------------------------
  
  training_set_random <- rbind(training_set_random, chosen_samples_random)
  training_set_conf <- rbind(training_set_conf, chosen_samples_conf)
  validating_set_random <- validating_set[-scores_random_df_k$Index,]
  validating_set_conf <- validating_set[-scores_conf_df_k$Index,]
  
  print("")
}

print(paste(accuracies_random, accuracies_conf))
accuracies_random <- as.data.frame(accuracies_random)
accuracies_conf <- as.data.frame(accuracies_conf)
sample_count <- initial_train_size:(nrow(accuracies_conf) + 1)

p = ggplot() + 
  geom_line(data = accuracies_random, aes(x = sample_count, y = accuracies_random, colour = "accuracies_random")) +
  geom_line(data = accuracies_conf, aes(x = sample_count, y = accuracies_conf, colour = "accuracies_conf")) +
  xlab('sample_count') +
  ylab('accuracies_conf') + 
  labs(title="Accuracy TP/All")

print(p)
