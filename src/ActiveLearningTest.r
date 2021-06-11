source("utils.r")

ActiveLearningTest <- function(data, initial_train_size, k, iterations, random) {


    ## --------------------------------------------------------------------------------
    ## ---------------- Podzielenie zbioru na pule i zbior testowy --------------------
    ## --------------------------------------------------------------------------------

    test_idx <- 0.8 * nrow(data) # ostatnie 20% danych to zbior testowy
    data_pool <- data[1:test_idx,]
    data_test <- data[-test_idx,]

    ## --------------------------------------------------------------------------------
    ## --Wybranie k próbek z puli do początkowego zbioru trenującego i podpisanie ich--
    ## --------------------------------------------------------------------------------
    
    set.seed(1235)

    training_set <- data_pool[1:initial_train_size, ]; # str(training_set)
    validating_set <- data_pool[-initial_train_size, ]; # str(validating_set)

    # loops <- nrow(validating_set)/k
    accuracies <- c()
    sample_count <- c()
    for (i in 1:iterations) {

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
    accuracies <- append(accuracies, accuracy)
    sample_count <- append(sample_count, initial_train_size + k*i)
    print(paste("Accuracy:", gsub(" ", "", paste(accuracy * 100, "%"))))

    ## --------------------------------------------------------------------------------
    ## -----------Wybranie k próbek niosących najwięcej informacji---------------------
    ## --------------------------------------------------------------------------------

    probabs <- attr(pred_validation, "probabilities")
    scores <- abs(probabs[,1] - probabs[,2])

    scores_df <- data.frame(1:nrow(validating_set), scores)
    colnames(scores_df) <- c('Index', 'Score')
    if (random) {
      scores_df <- scores_df[sample(nrow(scores_df)),] # shuffle data
    } else {
      scores_df <- scores_df[order(scores_df$Score),]
    }
    scores_df_k <- scores_df[1:k,]

    chosen_samples <- validating_set[scores_df_k$Index,]

    print(paste("training data size", nrow(training_set)))

    # --------------------------------------------------------------------------------
    ## -----Przeniesienie wybranych próbek ze zbioru walidacyjnego do treningowego----
    ## -------------------------------------------------------------------------------

    training_set <- rbind(training_set, chosen_samples)
    validating_set <- validating_set[-scores_df_k$Index,]

    print(i)
    }
    acc_data_frame <- data.frame(sample_count, accuracies)
    colnames(acc_data_frame) <- c('LabelledDataSize', 'Accuracy')
    return (acc_data_frame)
}

