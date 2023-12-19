# NOTE: This is for the final model to be trained on the leave one week out
# (LOWO) data for the final data aggregation

library(tidymodels)
library(xgboost)

train_tackle_model <- function(train_weeks)
{
  # TODO
  
  train_x <- train_weeks %>% 
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
  
  train_y <- train_weeks %>% 
    select(will_make_tackle) %>% 
    as.matrix()
  
  xgboost_train = xgb.DMatrix(data=train_x, label=train_y)
  
  # Creating training model
  tackle_model <- xgboost(data = xgboost_train,
                          max.depth = 6,
                          nrounds = 200,
                          eta = 0.2,
                          gamma = 0.6,
                          params = list(objective = "binary:logistic"),
                          eval_metric = 'auc') 
  
  return(tackle_model)
}
