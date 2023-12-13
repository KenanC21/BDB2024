source("code/modeling/train_participation_model.R")
source("code/modeling/train_tackle_model.R")


# Leave one week out modeling framework
# How this works --
# For each week w:
# - filter out w as a holdout set
# - based on the remaining 8 weeks:
#   - train the participation model
#   - train the tackle probability model
# - based on week w:
#   - predict the probability of participation
#   - create the hypothetical position circles for points where a player has a "chance" at making a tackle
#   - determine the optimal position within that circle for the player to make a tackle
#   - save week's results

# This will be used to train the final models/create final metric values

#' Leave One Week Out model training and evaluation
#'
#' @param data - the tracking data for all weeks
#' @param model_type - which of the two models that will be trained: participation or tackle
#'
#' @return
#' @export
#'
#' @examples
lowo_evaluate <- function(data, model_type)
{
  if (model_type == "tackle")
  {
    # TODO - call other function where the model will be trained
    
    
    
    return(0)
  }
  if (model_type == "participation")
  {
    # TODO - call other function where the model will be trained
    
    return(0)
  }
}


