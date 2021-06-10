set.seed(10111)
data <- read.csv("Datasets/JobChanges/aug_train.csv", header=T, sep=",")
data0 <- data
df <- data.frame(data, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = FALSE)
data <- data[1:100,]
df <- df[1:100,]

library(e1071)

dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)

plot(svmfit, dat)
