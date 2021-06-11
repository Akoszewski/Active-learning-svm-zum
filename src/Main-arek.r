rm(list = setdiff(ls(), "data"))

source("utils.r")
source("PrepareData.r")
source("ActiveLearningTest.r")


raw_data = read.csv("../Datasets/JobChanges/aug_train.csv", header=T, sep=",", na.strings=c("","NA"))
raw_data <- raw_data[1:1000,]
data <- prepareData(raw_data)

k <- 10
initial_train_size <- k

# Demonstration of the active learning testing function
accuracies1 <- ActiveLearningTest(data, initial_train_size, k, 20)
accuracies2 <- ActiveLearningTest(data, initial_train_size, k, 30)


