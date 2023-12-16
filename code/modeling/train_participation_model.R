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
  
  print(paste("Holding out week", week))
  
  # test_set <- week %>% 
  #   filter(week == week)
  train <- train_weeks %>% 
    filter(week != holdout_week)
  
  test <- train_weeks %>% 
    filter(week == holdout_week)
  
  train_x <- train %>% #Independent variables for train
    select(-will_have_chance_to_make_tackle) %>% 
    as.matrix()
  
  train_y <- train %>% #Dependent variable for train
    select(will_have_chance_to_make_tackle) %>% 
    as.matrix()
  
  test_x <- test %>% #Independent vars for test
    select(-will_have_chance_to_make_tackle) %>% 
    as.matrix()
  
  test_y <- test %>% #Dependent var for test
    select(will_have_chance_to_make_tackle) %>% 
    as.matrix()
  
  # Convert train and test data in xgboost matrix
  xgboost_train = xgb.DMatrix(data=train_x, label=train_y)
  xgboost_test = xgb.DMatrix(data=test_x, label=test_y)
  
  # Creating training model
  # tictoc::tic()
  participation_model_iter <- xgboost(data = xgboost_train,
                                      # TODO - update these to be optimal parameters
                                      max.depth=3,                           
                                      nrounds=100,
                                      params = list(objective = "binary:logistic"),
                                      eval_metric = 'auc') 
  # tictoc::toc()
  
  pred_test <- predict(participation_model_iter, xgboost_test)
  # TODO - replace this with the actual value we decide on
  # for decision threshold
  pred_tackle_participation <- ifelse(pred_test >= 0.138, 1, 0)
  
  final_df <- test %>% 
    bind_cols(pred)
  
  return(final_df)
}
