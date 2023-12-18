library(caret)
library(xgboost)
library(pROC)
library(tidyverse)
library(tidymodels)
library(probably)
# source("code/data_manipulation/tackle_model_data_manipulation.R")

setwd("C:/Users/Michael Egle/BDB2024")

source("code/util/create_and_standardize_week_data.R")


plays <- read_csv("data/plays.csv")
tackles <- read_csv("data/tackles.csv")


# Preliminary data split for testing and training 

tictoc::tic()
preliminary_model_data <- readRDS("misc/participation_model_pred.RDS")
tictoc::toc()

prelim_split <- preliminary_model_data %>% 
  ungroup() %>% 
  select(-c(gameId:y),
         -will_have_chance_to_make_tackle,
         -pred_tackle_participation,
         -prob) %>%
  drop_na() %>% 
  initial_split(prop = 0.8)

testing(prelim_split)

train_x <- training(prelim_split) %>% #Independent variables for train
  select(
    -will_make_tackle,
  ) %>% 
  as.matrix()

train_y <- training(prelim_split) %>% #Dependent variable for train
  select(will_make_tackle) %>% 
  as.matrix()

test_x <- testing(prelim_split) %>% #Independent vars for test
  select(
    -will_make_tackle
  ) %>% 
  as.matrix()

test_y <- testing(prelim_split) %>% #Dependent var for test
  select(will_make_tackle) %>% 
  as.matrix()


# Convert train and test data in xgboost matrix
xgboost_train = xgb.DMatrix(data=train_x, label=train_y)
xgboost_test = xgb.DMatrix(data=test_x, label=test_y)


# Creating training model
tictoc::tic()
tackle_model <- xgboost(data = xgboost_train,                      
                        max.depth = 6,                           
                        nrounds=100,
                        eta = 0.1,
                        gamma = 1,
                        params = list(objective = "binary:logistic"),
                        eval_metric = 'auc') 
tictoc::toc()
summary(tackle_model)

importance <- xgb.importance(colnames(train_x), model = tackle_model)

