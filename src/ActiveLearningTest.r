source("utils.r")

ActiveLearningTest <- function(data, initial_train_size, k, iterations) {
    accuracies <- c()

    ## --------------------------------------------------------------------------------
    ## ---------------- Podzielenie zbioru na pule i zbior testowy --------------------
    ## --------------------------------------------------------------------------------

    test_idx <- 0.8 * nrow(data) # ostatnie 20% danych to zbior testowy
    data_pool <- data[1:test_idx,]
    data_test <- data[-test_idx,]

    ## --------------------------------------------------------------------------------
    ## --Wybranie k próbek z puli do początkowego zbioru trenującego i podpisanie ich--
    ## --------------------------------------------------------------------------------

    training_set <- data_pool[1:initial_train_size, ]; # str(training_set)
    validating_set <- data_pool[-initial_train_size, ]; # str(validating_set)

    # loops <- nrow(validating_set)/k

    for (i in 1:iterations) {

    ## ------------------------------------------------------------------------
    ## -----------Trenowanie modelu za pomocą zbioru trenującego---------------
    ## ------------------------------------------------------------------------

    model <- svm(target ~ ., data = training_set, probability = TRUE)
    print(model)

    ## --------------------------------------------------------------------------------
    ## ----------------------Predykcja na zbiorze walidacyjnym-------------------------
    ## --------------------------------------------------------------------------------

    pred_validation <- predict(model, validating_set, probability = TRUE)
    sapply(data,class)

    ## --------------------------------------------------------------------------------
    ## -----------Użycie modelu na zbiorze testowym i zmierzenie wydajności------------
    ## --------------------------------------------------------------------------------

    pred_test <- predict(model, data_test)
    results <- data.frame(pred_test, data_test$target)
    colnames(results) <- c('Predicted', 'Actual')

    accuracy <- GetAccuracy(results)
    accuracies <- append(accuracies, accuracy)
    print(paste("Accuracy:", gsub(" ", "", paste(accuracy * 100, "%"))))

    ## --------------------------------------------------------------------------------
    ## -----------Wybranie k próbek niosących najwięcej informacji---------------------
    ## --------------------------------------------------------------------------------

    probabs <- attr(pred_validation, "probabilities")
    scores <- abs(probabs[,1] - probabs[,2])

    scores_df <- data.frame(1:nrow(validating_set), probabs, scores)
    colnames(scores_df) <- c('Index', 'Probability', 'Score')
    scores_df <- scores_df[order(scores),]
    #scores_df <- scores_df[sample(nrow(scores_df)),] # shuffle data
    scores_df_k <- scores_df[1:k,]

    chosen_samples <- validating_set[scores_df_k$Index,]

    print(paste("training data size", nrow(training_set)))

    # --------------------------------------------------------------------------------
    ## -----Przeniesienie wybranych próbek ze zbioru walidacyjnego do treningowego----
    ## -------------------------------------------------------------------------------

    training_set <- rbind(training_set, chosen_samples)
    validating_set <- validating_set[-scores_df_k$Index,]

    print(i)

    return (accuracies)
    }
}