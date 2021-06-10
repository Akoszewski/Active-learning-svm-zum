library("randomForest");library("caret");library("pROC");library("ROCR");library("plyr");library("missForest"); library("gbm");library("pdp");library("ggplot2"); library("iml");library("dplyr");library("kernlab"); library(DataExplorer)

data <- read.csv("Datasets/JobChanges/aug_train.csv", header=T, sep=",")
data0 <- data
df <- data.frame(data, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = FALSE)
data <- data[1:100,]
df <- df[1:100,]

print(paste0("### head(df): ", head(data)));
print(paste0("### class(df): ",class(df)));
print(paste0("### dim(df): ",  dim(df)));
print(paste0("### names(df): ",names(df)));

print(paste0("### print(df): ")); print(df[1:5,c(1:14)])
print(paste0("### str(df): ")); str(df)

# plot_correlation(data)

#split into testing and training data
## 75% of the sample size
smp_size_target <- floor(0.75 * nrow(data))


## set the seed to make your partition reproducible
set.seed(123)
train_ind_target <- sample(seq_len(nrow(data)), size = smp_size_target)
train_ind_target <- sample(seq_len(nrow(data)), size = smp_size_target)

train_target <- data[train_ind_target, ]; # str(train_target)
test_target <- data[-train_ind_target, ]; # str(test_target)

## ------------------------------------------------------------------------
## -------------------------Gradient Boosting------------------------------
## ------------------------------------------------------------------------


set.seed(123)
fitControl <- trainControl(method = 'repeatedcv', number=10, repeats=10) #10 fold CV this time
gbmGrid <- expand.grid(n.trees = seq(50, 4000,300), interaction.depth = c(1,5,9), shrinkage= c(0.01, 0.001), n.minobsinnode=5) #n.minobsinnode=5 maybe best for regression
#how many models to test
nrow(gbmGrid)


mod_gbm_target <- train(
  target ~ .,
  data = data,
  method = "gbm",
  trControl = fitControl,
  tuneGrid = gbmGrid, verbose=F)

plot(mod_gbm_target)
mod_gbm_target
summary(mod_gbm_target)
#check perfomance
getTrainPerf(mod_gbm_target)
pred_target <- predict(mod_gbm_target,test_target )
print(pred_target)
#better performance than SVM RMSE = 18




## ------------------------------------------------------------------------
## --------------------Support Vector machine------------------------------
## ------------------------------------------------------------------------

#The gamma parameter in the RBF kernel determines the reach of a single training instance. If the value of Gamma is low,
#then every training instance will have a far reach. Conversely, high values of gamma mean that training instances will have a close reach. So, with a high value of gamma, the SVM decision boundary 
#will simply be dependent on just the points that are closest to the decision boundary, effectively ignoring points that are farther away. In comparison, a low value of gamma will result in a decision boundary that will consider points that are further from it.
#As a result, high values of gamma typically produce highly flexed decision boundaries, and low values of gamma often results in a decision boundary that is more linear.
set.seed(123)

#### First for mortality
mod_svm_target <- train(
  target ~ .,
  data = training_set,
  method = "svmRadial",
  trControl = trainControl(method = "cv", number = 5), #5 fold cross validation
  tuneGrid = expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0), C= 2^c(0:5)))

plot(mod_svm_target)




#check perfomance
getTrainPerf(mod_svm_target)
