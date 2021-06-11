library(e1071)

GetAccuracy <- function(prediction) {
    correct_vals <- 0
    for (row in 1:nrow(prediction)) {
        #if (round(prediction[row, 'Predicted']) == prediction[row, 'Actual']) {
        if (prediction[row, 'Predicted'] == prediction[row, 'Actual']) {
            correct_vals <- correct_vals + 1
        }
    }
    return (correct_vals/nrow(prediction))
}
