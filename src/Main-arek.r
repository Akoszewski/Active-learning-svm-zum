source("utils.r")

data = read.csv("../Datasets/JobChanges/aug_train.csv", header=T, sep=",")
df = data.frame(data, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = default.stringsAsFactors())

# CRITERIA <- df[c(1),-c(1:2)]
# df <- df[-c(1), -c(1)]
# print(df)

# dat = data.frame(x, y = as.factor(target))
df <- df[1:1000,]
# print(df)

n_train  <-  round(0.5*nrow(df))
df_train  <-  df[c(0:n_train),]
df_test   <- df[-c(0:n_train),]

svmfit = svm(target ~ ., data = df_train, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)


# datatest = read.csv("../Datasets/JobChanges/aug_test.csv", header=T, sep=",")
# dft = data.frame(datatest, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = default.stringsAsFactors())
predict(svmfit, df_test)

# # x <- data.matrix(DATA[,-c(1)])
# x <- apply(as.matrix(DATA[,-c(1)]), 2, as.numeric)
# # x <- DATA[,-c(1)]
# y <- as.integer(data.matrix(DATA[,c(1)]))

# y[y==2]=-1

# n_train  <-  round(0.8*nrow(x))
# x_train  <-  x[c(0:n_train),]
# x_test   <-  x[-c(0:n_train),]
# y_train  <-  y[c(0:n_train)]
# y_test   <-  y[-c(0:n_train)]

# # SVMRFE(x_train, y_train, CRITERIA)
# model <- svm(x, y, kernel = "linear", scale=FALSE)
