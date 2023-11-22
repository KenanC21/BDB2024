library(xgboost)
library(tidyverse)
library(tidymodels)
library(probably)
source("code/data_manipulation/tackle_model_data_manipulation.R")

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

train_x <- training(prelim_split) %>% 
  select(-will_make_tackle) %>% 
  as.matrix()

train_y <- training(prelim_split) %>% 
  select(will_make_tackle) %>% 
  as.matrix()

test_x <- testing(prelim_split) %>% 
  select(-will_make_tackle) %>% 
  as.matrix()

test_y <- testing(prelim_split) %>% 
  select(will_make_tackle) %>% 
  as.matrix()



prelim_tackle_model <- xgboost(data = train_x, label = train_y,
                               nrounds = 100,
                               params = list(objective = "binary:logistic"))


pred_y <- predict(prelim_tackle_model, newdata = test_x)

brier_score <- mean((test_y - pred_y)^2)
brier_score

importance <- xgb.importance(colnames(train_x), model = prelim_tackle_model)

pred <- testing(prelim_split) %>% 
  bind_cols(pred = pred_y)

# Look at the calibration plot
pred %>% 
  cal_plot_windowed(truth = will_make_tackle, estimate = pred)

