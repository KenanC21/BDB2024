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
  # TODO
  
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
train_participation_model_helper <- function(train_weeks, week)
{
  # Filter out the week to keep as a holdout week since we want out of sample predictions
  
  # test_set <- week %>% 
  #   filter(week == week)
  
  print(paste("Holding out week", week))
  
  return(data.frame())
}
