library(e1071)

GetAccuracy <- function(prediction) {
    correct_vals <- 0
    for (row in 1:nrow(prediction)) {
        if (round(prediction[row, 'Predicted']) == prediction[row, 'Actual']) {
            correct_vals <- correct_vals + 1
        }
    }
    return (correct_vals/nrow(prediction))
}

entropy <- function(vect) {
    A <- unique(vect)
    entropy <- 0
    for (v in A) {
        nom <- sum(vect == v)
        denom <- length(vect)
        Ps <- nom/denom
        skladnik <- -Ps*log2(Ps)
        entropy <- entropy + skladnik
        if (!isSilent) print(paste("-", nom, "/", denom, "*log", nom, "/", denom, "=", round(skladnik, digits = 3)))
    }
    # entropy <- round(entropy, digits = 2)
    if (!isSilent) print(paste("=", entropy))
    return (entropy)
}