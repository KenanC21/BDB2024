library(caret)
library(xgboost)
library(pROC)
library(tidyverse)
library(tidymodels)
library(probably)
# source("code/data_manipulation/tackle_model_data_manipulation.R")

setwd("C:/Users/Michael Egle/BDB2024")

# source("code/util/create_and_standardize_week_data.R")


plays <- read_csv("data/plays.csv")
tackles <- read_csv("data/tackles.csv")


# Preliminary data split for testing and training 

tictoc::tic()
preliminary_model_data <- readRDS("misc/participation_model_pred.RDS")
tictoc::toc()

set.seed(30)
prelim_split <- preliminary_model_data %>% 
  initial_split(prop = 0.8)

testing(prelim_split)

train_x <- training(prelim_split) %>% #Independent variables for train
  select(
    distance_to_ball_carrier,
    difference_min_distance_to_ball_carrier,
    max_angle_formed_by_blocker_and_ball_carrier,
    ball_carrier_s_difference,
    ball_carrier_dir_difference,
    dir,
    ball_carrier_s,
    s,
    ball_carrier_distance_to_sideline,
    possible_blockers_within_7_yards,
    ball_carrier_distance_to_endzone
  ) %>% 
  as.matrix()

train_y <- training(prelim_split) %>% #Dependent variable for train
  select(will_make_tackle) %>% 
  as.matrix()

test_x <- testing(prelim_split) %>% #Independent vars for test
  select(
    distance_to_ball_carrier,
    difference_min_distance_to_ball_carrier,
    max_angle_formed_by_blocker_and_ball_carrier,
    ball_carrier_s_difference,
    ball_carrier_dir_difference,
    dir,
    ball_carrier_s,
    s,
    ball_carrier_distance_to_sideline,
    possible_blockers_within_7_yards,
    ball_carrier_distance_to_endzone
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

pred <- predict(tackle_model, newdata = test_x)

importance <- xgb.importance(colnames(train_x), model = tackle_model)

roc <- roc(as.numeric(test_y), pred)
plot(roc)
auc <- auc(roc)

