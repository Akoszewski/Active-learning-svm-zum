library(dplyr)
library(party)

prepareData <- function(df) {
    df$enrollee_id = NULL
    getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    df = df %>%
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


    df$gender = as.factor(df$gender)
    df$last_new_job = as.factor(df$last_new_job)
    df$education_level = as.factor(df$education_level)
    df$experience = as.factor(df$experience)
    df$relevent_experience = as.factor(df$relevent_experience)
    df$major_discipline = as.factor(df$major_discipline)
    df$enrolled_university = as.factor(df$enrolled_university)
    df$city = as.factor(df$city)

    # przewydywanie brakujacych wartosci
    df_without_size = subset(df, select = -company_size)
    na_idx = which(is.na(df_without_size$company_type), arr.ind=TRUE)
    train_without_size = df_without_size[-na_idx, ]
    test_without_size = df_without_size[na_idx, ]
    train_without_size$company_type = as.factor(train_without_size$company_type)

    fit_ctree = ctree(formula = company_type ~ ., data = train_without_size)
    pred_company_type = predict(fit_ctree, test_without_size)
    pred_company_type = lapply(pred_company_type, as.character)
    v = c()
    for (i in 1:length(pred_company_type)){
    v <- c(v, pred_company_type[[i]])
    }
    df = df %>%
    mutate(company_type = replace(company_type,
                                    is.na(company_type),
                                    v))
    df$company_type = as.factor(df$company_type)

    na_idx = which(is.na(df$company_size), arr.ind=TRUE)
    train_size_without_na = df[-na_idx, ]
    test_size_with_na = df[na_idx, ]
    train_size_without_na$company_size = as.factor(train_size_without_na$company_size)
    fit_ctree = ctree(formula = company_size ~ ., data = train_size_without_na)
    pred_size_type1 = predict(fit_ctree, test_size_with_na)
    pred_size_type1 = lapply(pred_size_type1, as.character)
    v = c()
    for (i in 1:length(pred_size_type1)){
    v <- c(v, pred_size_type1[[i]])
    }
    df = df %>%
    mutate(company_size = replace(company_size,
                                    is.na(company_size),
                                    v))
    df$company_size = as.factor(df$company_size)
    df$target = as.factor(df$target)
    return (df)
}