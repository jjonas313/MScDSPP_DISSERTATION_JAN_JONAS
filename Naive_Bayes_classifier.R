# NAIVE BAYES CLASSIFIER

library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(caret)
library(knitr)

surrogate_labeled <- read.csv("at_train_data.csv")
data_train <- surrogate_labeled %>% select("date", "sentence", "label")
data_train$train <- TRUE

gold_standard_labeled <- read.csv("at_valid_data.csv")
data_test <- gold_standard_labeled %>% select("date", "sentence", "label")
data_test$train <- FALSE 

# join datasets

df_sentence <- rbind(data_train, data_test)


# create corpus
df_corpus <- corpus(df_sentence, text_field = "sentence")

df_dfm <- df_corpus %>% 
  tokens() %>%
  dfm() %>%
  dfm_remove(stopwords("en"))

# subset into training and test dfms

dfm_train <- dfm_subset(df_dfm, train)

dfm_test <- dfm_subset(df_dfm, !train)

dim(dfm_train)
dim(dfm_test)

# estiamte NB model

nb_train <- textmodel_nb(x = dfm_train, 
                         y = dfm_train$label,
                         prior = "docfreq", 
                         distribution = "multinomial")

head(sort(coef(nb_train)[,"uncertain"], decreasing = T), 20)
head(sort(coef(nb_train)[,"certain"], decreasing = T), 20)


# confusion matrix for accuracy
dfm_train$predicted_classification_nb <- predict(nb_train, type = "class")

confusion_train <- table(predicted_classification = dfm_train$predicted_classification_nb, 
                         true_classification = dfm_train$label)

confusion_train

confusion_train_statistics <- confusionMatrix(confusion_train, 
                                              positive = "uncertain",
                                              mode = "everything")


confusion_train_statistics

F1 <- confusion_train_statistics$byClass[7]
F1

# Test set accuracy

dfm_test$predicted_classification_nb <- predict(nb_train, 
                                                newdata = dfm_test, 
                                                type = "class")

confusion_test <- table(predicted_classification = dfm_test$predicted_classification_nb, 
                        true_classification = dfm_test$label)

confusion_test_statistics <- confusionMatrix(confusion_test, 
                                             positive = "uncertain", 
                                             mode = "everything")

confusion_test_statistics

F1 <- confusion_test_statistics$byClass[1]
F1

