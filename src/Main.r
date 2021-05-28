# library("randomForest");library("caret");library("pROC");library("ROCR");library("plyr");library("missForest"); library("gbm");library("pdp");library("ggplot2"); library("iml");library("dplyr");library("kernlab")

data <- read.csv("../Datasets/JobChanges/aug_train.csv", header=T, sep=",")

df <- data.frame(data, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = FALSE)

print(paste0("### head: ", head(data)));
print(paste0("### class: ",class(df)));
print(paste0("### dim: ",  dim(df)));
print(paste0("### names: ",names(df)));

print(paste0("### df: "));
print(df[1:5,c(1:14)])
str(df)
CRITERIA <- df[c(1),-c(1:2)]
DATA <- df[-c(1), -c(1)]

