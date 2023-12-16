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

preliminary_model_data <- create_and_standardize_week_data(2)

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
  select(
    difference_min_distance_to_ball_carrier,
    max_angle_formed_by_blocker_and_ball_carrier,
    min_distance_to_ball_carrier,
    dir,
    distance_to_ball_carrier,
    s,
    possible_blockers_within_3_yards,
    ball_carrier_distance_to_sideline,
    ball_carrier_distance_to_endzone,
    ball_carrier_dir_difference,
    ball_carrier_s_difference
  ) %>% 
  as.matrix()

train_y <- training(prelim_split) %>% #Dependent variable for train
  select(will_have_chance_to_make_tackle) %>% 
  as.matrix()

test_x <- testing(prelim_split) %>% #Independent vars for test
  select(
    difference_min_distance_to_ball_carrier,
    max_angle_formed_by_blocker_and_ball_carrier,
    min_distance_to_ball_carrier,
    dir,
    distance_to_ball_carrier,
    s,
    possible_blockers_within_3_yards,
    ball_carrier_distance_to_sideline,
    ball_carrier_distance_to_endzone,
    ball_carrier_dir_difference,
    ball_carrier_s_difference
  ) %>% 
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

threshold_helper <- function(threshold, labels, prediction_probs)
{
  prediction_labels <- ifelse(prediction_probs >= threshold, 1, 0)
  
  tp <- sum(labels == 1 & prediction_labels == 1)
  tn <- sum(labels == 0 & prediction_labels == 0)
  fp <- sum(labels == 1 & prediction_labels == 0)
  fn <- sum(labels == 0 & prediction_labels == 1)
  
  sens <- tp / (tp + fn)
  spec <- tn / (tn + fp)
  ppv <- tp / (tp + fp)
  npv <- tn / sum(tn + fn)
  
  return(data.frame(threshold, sens, spec, ppv, npv))
}

get_prediction_metrics <- function(labels, prediction_probs)
{
  thresholds <- seq(0, 1, 0.001)
  
  threshold_analysis <- pmap_dfr(.l = list(thresholds),
                                 .f = threshold_helper,
                                 labels = labels,
                                 prediction_probs = prediction_probs,
                                 .progress = T)
  
  return(threshold_analysis)
}

cutoffs <- get_prediction_metrics(testing(prelim_split)$will_have_chance_to_make_tackle,
                                  pred_test)

cutoffs %>% 
  pivot_longer(cols = c(sens:npv),
               values_to = "value",
               names_to = "metric") %>% 
  ggplot(aes(x = threshold, y = value, group = metric, color = metric)) +
  geom_line()

# I think the point around 0.26 is what we'll want to use. Maximizes the PPV and
# sensitivity, both of which are concerned with positive predictions

# Let's find the exact point

cutoffs %>% 
  mutate(sens_cross_point = ifelse(sens > ppv, 1, 0)) %>% 
  filter(sens_cross_point == 1) %>% 
  head(10)

cutoffs %>% 
  mutate(npv_cross_point = ifelse(npv > spec, 1, 0)) %>% 
  filter(npv_cross_point == 1) %>% 
  head(10)

# Let's use the cutoff of 0.274


# Convert prediction type to binomial response with a cutoff of 0.5
# meaning our model will focus on players with at least a 50% chance of
# making the tackle, missing or assisting 
pred_test_fact = factor(ifelse(pred_test >= 0.274, 1, 0))

# Don't like any of the preloaded packages for threshold analysis. Going to create
# my own function for calculating these



# Append factored reponse of having chance to make a tackle 
pred <- testing(prelim_split) %>% 
  bind_cols(pred = pred_test_fact) %>% 
  bind_cols(prob = pred_test)


# 95% confident the accuracy of our model will predict a player having a chance
# to make the tackle to be between .813 and .8842
conf_mat = confusionMatrix(as.factor(as.numeric(test_y), levels = c(0, 1)), pred_test_fact,
                           positive = "1")
print(conf_mat)

# Since we're imbalanced, we need to consider not just the sensitivity/specificity,
# but the negative and positive prediction values as well

# table(pred_test_fact, pred$will_have_chance_to_make_tackle)

# ROC gives a slightly different answer for threshold since it's 
# only going off of negative and positive predictive value even though it
# says specificity and sensitivity on the plot
plot.roc(test_y,  pred_test, print.thres = T, print.auc = T)


