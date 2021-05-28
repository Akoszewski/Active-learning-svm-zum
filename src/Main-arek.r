source("utils.r")

data = read.csv("../Datasets/JobChanges/aug_train.csv", header=T, sep=",")
df = data.frame(data, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = default.stringsAsFactors())

DATA <- as.data.frame(t(as.matrix(df)))
CRITERIA <- DATA[c(1),-c(1:2)]
DATA <- DATA[-c(1), -c(1)]

# x <- data.matrix(DATA[,-c(1)])
x <- apply(as.matrix(DATA[,-c(1)]), 2, as.numeric)
# x <- DATA[,-c(1)]
y <- as.integer(data.matrix(DATA[,c(1)]))

y[y==2]=-1

n_train  <-  round(0.8*nrow(x))
x_train  <-  x[c(0:n_train),]
x_test   <-  x[-c(0:n_train),]
y_train  <-  y[c(0:n_train)]
y_test   <-  y[-c(0:n_train)]

SVMRFE(x_train, y_train, CRITERIA)