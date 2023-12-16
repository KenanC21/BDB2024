# NOTE: This is for the final model to be trained on the leave one week out
# (LOWO) data for the final data aggregation

library(tidymodels)
library(xgboost)

train_tackle_model <- function(train_weeks)
{
  # TODO
  
  train_x <- train_weeks %>% 
    select(
      # TODO - select all of the model predictor variables
    ) %>% 
    as.matrix()
  
  train_y <- train_weeks %>% 
    select(will_make_tackle) %>% 
    as.matrix()
  
  xgboost_train = xgb.DMatrix(data=train_x, label=train_y)
  
  # Creating training model
  tackle_model <- xgboost(data = xgboost_train,                      
                           # TODO - update with final model parameters
                           params = list(objective = "binary:logistic"),
                           eval_metric = 'auc') 
  
  return(tackle_model)
}
