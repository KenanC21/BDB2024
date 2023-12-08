library(caret)
library(xgboost)
library(pROC)
library(tidyverse)
library(tidymodels)
library(probably)
source("tackle_model_data_manipulation.R")

# Preliminary data split for testing and training 

prelim_split <- preliminary_model_data %>% 
  mutate(ball_carrier_dir_difference = dir - ball_carrier_dir,
         ball_carrier_s_difference = s - ball_carrier_s,
         ball_carrier_a_difference = a - ball_carrier_a,
         ball_carrier_x_difference = x - ball_carrier_x,
         ball_carrier_y_difference = y - ball_carrier_y,
         ball_carrier_distance_to_sideline = pmin(160/3 - ball_carrier_y, y)) %>% 
  select(will_make_tackle, distance_to_ball_carrier, ball_carrier_dir_difference,
         ball_carrier_s_difference, ball_carrier_a_difference,
         ball_carrier_x_difference, ball_carrier_y_difference,
         ball_carrier_distance_to_sideline) %>% 
  initial_split(prop = 0.8)

testing(prelim_split)


# Split training and testing
train_x <- training(prelim_split) %>% #Independent variables for train
  select(-will_make_tackle) %>% 
  as.matrix()

train_y <- training(prelim_split) %>% #Dependent variable for train
  select(will_make_tackle) %>% 
  as.matrix()

test_x <- testing(prelim_split) %>% #Independent vars for test
  select(-will_make_tackle) %>% 
  as.matrix()

test_y <- testing(prelim_split) %>% #Dependent var for test
  select(will_make_tackle) %>% 
  as.matrix()


# Convert train and test data in xgboost matrix
xgboost_train = xgb.DMatrix(data=train_x, label=train_y)
xgboost_test = xgb.DMatrix(data=test_x, label=test_y)


# Creating training model
train_model <- xgboost(data = xgboost_train,                      
                 max.depth=3,                           
                 nrounds=100,
                 params = list(objective = "binary:logistic"),
                 eval_metric = 'auc') 
summary(train_model)


# Importance of features
importance = xgb.importance(colnames(train_x), model = train_model)

# Use training model above to make predictions on test data
pred_test = predict(train_model, xgboost_test)
pred_test


# Convert prediction type to binomial response with a cutoff of 0.5
# meaning our model will focus on players with at least a 50% chance of
# making the tackle, missing or assisting 
pred_test_fact = factor(ifelse(pred_test>=.5, 1, 0))


# Append factored reponse of having chance to make a tackle 
pred <- testing(prelim_split) %>% 
  bind_cols(pred = pred_test_fact)


# 95% confident the accuracy of our model will predict a player having a chance
# to make the tackle to be between .813 and .8842
conf_mat = confusionMatrix(as.factor(as.numeric(test_y)), pred_test_fact)
print(conf_mat)
 

