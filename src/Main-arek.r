rm(list = setdiff(ls(), "data"))

source("utils.r")
source("PrepareData.r")
source("ActiveLearningTest.r")

raw_data = read.csv("../Datasets/JobChanges/aug_train.csv", header=T, sep=",", na.strings=c("","NA"))
#raw_data <- raw_data[1:3000,]
data <- prepareData(raw_data)
set.seed(1235)
# data <-data[sample(nrow(data)),] # shuffle data

k <- 1
initial_train_size <- 2
iterations <- 20
random <- FALSE
# Demonstration of the active learning testing function
accuracies_random <- ActiveLearningTest(data, initial_train_size, k, iterations, TRUE)
accuracies_conf <- ActiveLearningTest(data, initial_train_size, k, iterations, random)

wykres = ggplot() + 
  geom_line(data = accuracies_random, aes(x = LabelledDataSize, y = Accuracy, colour = "accuracies_random")) +
  geom_line(data = accuracies_conf, aes(x = LabelledDataSize, y = Accuracy, colour = "accuracies_conf")) +
  xlab('sample_count') +
  ylab('accuracies_conf') + 
  labs(title="Accuracy TP/All")

print(wykres)
