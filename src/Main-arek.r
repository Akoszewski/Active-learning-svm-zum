rm(list = setdiff(ls(), "data"))

source("utils.r")
source("PrepareData.r")
source("ActiveLearningTest.r")

library('ggplot2')

raw_data = read.csv("../Datasets/JobChanges/aug_train.csv", header=T, sep=",", na.strings=c("","NA"))
#raw_data <- raw_data[1:3000,]
data <- prepareData(raw_data)

k <- 2
initial_train_size <- k

# Demonstration of the active learning testing function
accuracies_random <- ActiveLearningTest(data, initial_train_size, k, 20, TRUE)
accuracies_conf <- ActiveLearningTest(data, initial_train_size, k, 20, FALSE)

wykres = ggplot() + 
  geom_line(data = accuracies_random, aes(x = LabelledDataSize, y = Accuracy), color = "blue") +
  geom_line(data = accuracies_conf, aes(x = LabelledDataSize,, y = Accuracy), color = "red") +
  xlab('Labelled Data Size') +
  ylab('Accuracy')

print(wykres)
