# NOTE: This is for the final model to be trained on the leave one week out
# (LOWO) data for the final data aggregation

#' Train the final participation model
#'
#' Iterates through the weeks to perform the LOWO prediction process
#'
#' @param train_weeks - tracking data for each week's defensive players
#'
#' @return - a dataframe of players who have a chance to make the tackle, 
#'           with all other important predictor variables
#'
train_participation_model <- function(train_weeks)
{
  all_participation_predictions <- pmap_dfr(.l = list(1:9),
                                            .f = train_participation_model_helper,
                                            train_weeks = train_weeks)
  
  return(all_participation_predictions)
}

#' Helper function for model training
#'
#' Perform the training and out of sample prediction on the held out week
#'
#' @param train_weeks - the tracking data passed from the above function
#' @param week - the week to hold out for prediction
#'
#' @return - predictions for the held out week, based on a model trainined on the
#'           remaining weeks
#'
train_participation_model_helper <- function(train_weeks, holdout_week)
{
  # Filter out the week to keep as a holdout week since we want out of sample predictions
  
  print(paste("Holding out week", holdout_week))
  
  # test_set <- week %>% 
  #   filter(week == week)
  train <- train_weeks %>% 
    filter(week != holdout_week)
  
  test <- train_weeks %>% 
    filter(week == holdout_week)
  
  # Split training and testing
  train_x <- train %>% #Independent variables for train
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
  
  train_y <- train %>% #Dependent variable for train
    select(will_have_chance_to_make_tackle) %>% 
    as.matrix()
  
  test_x <- test %>% #Independent vars for test
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
  
  test_y <- test %>% #Dependent var for test
    select(will_have_chance_to_make_tackle) %>% 
    as.matrix()
  
  # Convert train and test data in xgboost matrix
  xgboost_train = xgb.DMatrix(data=train_x, label=train_y)
  xgboost_test = xgb.DMatrix(data=test_x, label=test_y)
  
  # Creating training model
  participation_model_iter <- xgboost(data = xgboost_train,
                                      max.depth = 6,                           
                                      nrounds=100,
                                      eta = 0.1,
                                      gamma = 1,
                                      params = list(objective = "binary:logistic"),
                                      eval_metric = 'auc') 
  
  pred_test <- predict(participation_model_iter, xgboost_test)
  # TODO - replace this with the actual value we decide on
  # for decision threshold
  pred_tackle_participation <- ifelse(pred_test >= 0.267, 1, 0)
  
  final_df <- test %>% 
    bind_cols(pred_tackle_participation = pred_tackle_participation) %>% 
    bind_cols(prob = pred_test) %>% 
    filter(pred_tackle_participation == 1)
  
  return(final_df)
}
