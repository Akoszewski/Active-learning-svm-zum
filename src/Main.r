library("randomForest");library("caret");library("pROC");library("ROCR");
library("plyr");library("missForest"); library("gbm");library("pdp");
library("ggplot2"); library("iml");library("dplyr");library("kernlab");
library(DataExplorer);library(e1071)

data <- read.csv("Datasets/JobChanges/aug_train.csv", header=T, sep=",", na.strings=c("","NA"))
data_backup <- data

#data <- data[1:100,]
#df <- df[1:100,]

## --------------------------------------------------------------------------------
## ------------------------------ data exploration---------------------------------
## --------------------------------------------------------------------------------

library(DataExplorer)
plot_missing(data) ## Are there missing values, and what is the missing data profile? None in this case
plot_bar(data) ## How does the categorical frequency for each discrete variable look like?
plot_histogram(data) ## What is the distribution of each continuous variable?


print(paste0("### head(data): ", head(data)));
print(paste0("### class(data): ",class(data)));
print(paste0("### dim(data): ",  dim(data)));
print(paste0("### names(data): ",names(data)));
print(paste0("### print(data): ")); print(data[1:5,c(1:14)])
print(paste0("### str(data): ")); str(data)

# plot_correlation(data)




## ------------------------------------------------------------------------
## -------------------------Przygotowywanie danych-------------------------
## ------------------------------------------------------------------------

data$enrollee_id = NULL

library(dplyr)
# Insert to missing value
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
## Insert to missing value with mode
data = data %>%
  mutate(gender = replace(gender,
                          is.na(gender),
                          getmode(gender)),
         experience = replace(experience,
                              is.na(experience),
                              getmode(experience)),
         major_discipline = replace(major_discipline,
                                    is.na(major_discipline),
                                    getmode((major_discipline))),
         education_level = replace(education_level,
                                   is.na(education_level),
                                   getmode(education_level)),
         enrolled_university = replace(enrolled_university,
                                       is.na(enrolled_university),
                                       getmode(enrolled_university)),
         last_new_job = replace(last_new_job, 
                                is.na(last_new_job),
                                getmode(last_new_job)))


data$gender = as.factor(data$gender)
data$last_new_job = as.factor(data$last_new_job)
data$education_level = as.factor(data$education_level)
data$experience = as.factor(data$experience)
data$relevent_experience = as.factor(data$relevent_experience)
data$major_discipline = as.factor(data$major_discipline)
data$enrolled_university = as.factor(data$enrolled_university)
data$city = as.factor(data$city)

# predicting missing value
library(party)
df_without_size = subset(data, select = -company_size)
na_idx = which(is.na(df_without_size$company_type), arr.ind=TRUE)
train_without_size = df_without_size[-na_idx, ]
test_without_size = df_without_size[na_idx, ]
train_without_size$company_type = as.factor(train_without_size$company_type)
# predict missing value company type
fit_ctree = ctree(formula = company_type ~ ., data = train_without_size)
pred_company_type = predict(fit_ctree, test_without_size)
pred_company_type = lapply(pred_company_type, as.character)
v = c()
for (i in 1:length(pred_company_type)) {
  v <- c(v, pred_company_type[[i]])
}

data = data %>%
  mutate(company_type = replace(company_type,
                                is.na(company_type),
                                v))
data$company_type = as.factor(data$company_type)

na_idx = which(is.na(data$company_size), arr.ind=TRUE)
train_size_without_na = data[-na_idx, ]
test_size_with_na = data[na_idx, ]
train_size_without_na$company_size = as.factor(train_size_without_na$company_size)
# predict missing value company type
fit_ctree = ctree(formula = company_size ~ ., data = train_size_without_na)
pred_size_type1 = predict(fit_ctree, test_size_with_na)
pred_size_type1 = lapply(pred_size_type1, as.character)
v = c()
for (i in 1:length(pred_size_type1)){
  v <- c(v, pred_size_type1[[i]])
}
data = data %>%
  mutate(company_size = replace(company_size,
                                is.na(company_size),
                                v))
data$company_size = as.factor(data$company_size)



## --------------------------------------------------------------------------------
## --Wybranie k próbek z puli do początkowego zbioru trenującego i podpisanie ich--
## --------------------------------------------------------------------------------

## set the seed to make your partition reproducible
set.seed(123)

onePercentOfSamples <- floor(0.1 * nrow(data))
train_idx <- sample(seq_len(nrow(data)), size = onePercentOfSamples)

training_set <- data[train_idx, ]; # str(training_set)
validating_set <- data[-train_idx, ]; # str(validating_set)
validating_set$target <- NULL


## ------------------------------------------------------------------------
## -----------Trenowanie modelu za pomocą zbioru trenującego---------------
## ------------------------------------------------------------------------

model = svm(target ~ ., data = training_set, kernel = "linear", cost = 10, scale = FALSE, probability = TRUE)
print(model)

## --------------------------------------------------------------------------------
## --------------Predykcja prawdopodobieństw na zbiorze walidacyjnym---------------
## --------------------------------------------------------------------------------

predict(model, newdata = validating_set, probability = TRUE)

## ------------------------------------------------------------------------
## ---------------------------entropy--------------------------------------
## ------------------------------------------------------------------------

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

