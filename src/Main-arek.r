source("utils.r")
source("PrepareData.r")

df = read.csv("../Datasets/JobChanges/aug_train.csv", header=T, sep=",", na.strings=c("","NA"))
df <- df[1:3000,]
df <- prepareData(df)

n_train  <-  round(0.5*nrow(df))
df_train  <-  df[c(0:n_train),]
df_test   <- df[-c(0:n_train),]

model <- svm(target ~ ., data = df_train, kernel = "linear")
pred <- predict(model, df_test)
pred_rounded <-round(pred)
print(model)
print(pred_rounded)
