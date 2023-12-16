library(caret)
library(xgboost)
library(pROC)
library(ROCR)
library(ROCit)
library(tidyverse)
library(tidymodels)
library(probably)
# source("code/data_manipulation/tackle_model_data_manipulation.R")

setwd("/Users/michaelegle/BDB2024")

source("code/util/create_and_standardize_week_data.R")


plays <- read_csv("data/plays.csv")
tackles <- read_csv("data/tackles.csv")


# Preliminary data split for testing and training 

preliminary_model_data <- create_and_standardize_week_data(1)

prelim_split <- preliminary_model_data %>% 
  ungroup() %>% 
  # mutate(ball_carrier_dir_difference = dir - ball_carrier_dir,
  #        ball_carrier_s_difference = s - ball_carrier_s,
  #        ball_carrier_a_difference = a - ball_carrier_a,
  #        ball_carrier_x_difference = x - ball_carrier_x,
  #        ball_carrier_y_difference = y - ball_carrier_y,
  #        ball_carrier_distance_to_sideline = pmin(160/3 - ball_carrier_y, y)) %>% 
  select(-c(gameId:y),
         -will_make_tackle) %>%
  drop_na() %>% 
  initial_split(prop = 0.8)

testing(prelim_split)


# target: will_have_chance_to_make_tackle
# predictors: anything with defender locations/angles with blockers and ballcarrier

# Split training and testing
train_x <- training(prelim_split) %>% #Independent variables for train
  select(-will_have_chance_to_make_tackle) %>% 
  as.matrix()

train_y <- training(prelim_split) %>% #Dependent variable for train
  select(will_have_chance_to_make_tackle) %>% 
  as.matrix()

test_x <- testing(prelim_split) %>% #Independent vars for test
  select(-will_have_chance_to_make_tackle) %>% 
  as.matrix()

test_y <- testing(prelim_split) %>% #Dependent var for test
  select(will_have_chance_to_make_tackle) %>% 
  as.matrix()


# Convert train and test data in xgboost matrix
xgboost_train = xgb.DMatrix(data=train_x, label=train_y)
xgboost_test = xgb.DMatrix(data=test_x, label=test_y)


# Creating training model
tictoc::tic()
train_model <- xgboost(data = xgboost_train,                      
                 max.depth=3,                           
                 nrounds=100,
                 params = list(objective = "binary:logistic"),
                 eval_metric = 'auc') 
tictoc::toc()
summary(train_model)


# Importance of features
importance = xgb.importance(colnames(train_x), model = train_model)

# Use training model above to make predictions on test data
pred_test = predict(train_model, xgboost_test)
pred_test


# Convert prediction type to binomial response with a cutoff of 0.5
# meaning our model will focus on players with at least a 50% chance of
# making the tackle, missing or assisting 
pred_test_fact = factor(ifelse(pred_test >= 0.36, 1, 0))


# Append factored reponse of having chance to make a tackle 
pred <- testing(prelim_split) %>% 
  bind_cols(pred = pred_test_fact) %>% 
  bind_cols(prob = pred_test)


# 95% confident the accuracy of our model will predict a player having a chance
# to make the tackle to be between .813 and .8842
conf_mat = confusionMatrix(as.factor(as.numeric(test_y), levels = c(0, 1)), pred_test_fact)
print(conf_mat)

# Since we're imbalanced, we need to consider not just the sensitivity/specificity,
# but the negative and positive prediction values as well

table(pred_test_fact, pred$will_have_chance_to_make_tackle)

# While this is useful, this label is showing the NPV and PPV, not specificity/sensitivity
plot.roc(test_y,  pred_test, print.thres = T, print.auc = T)

ROCit_obj <- rocit(score = pred$prob ,class = pred$will_have_chance_to_make_tackle)
plot(ROCit_obj)
ksplot(ROCit_obj)

ROCit_obj$Cutoff

cutoff_df <- data.frame(cutoff = ROCit_obj$Cutoff,
                        FPR = ROCit_obj$FPR,
                        TPR = ROCit_obj$TPR)

cal_plot_windowed(.data = pred,
                  truth = will_have_chance_to_make_tackle,
                  estimate = prob,
                  window_size = 0.05)

# Good looking calibration plot, just hope we're not overfitting
# may have to do additional tests for that



