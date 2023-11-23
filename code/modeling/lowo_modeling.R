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

#' Leave One Week Out evaluation
#'
#' @param week - the week for which to evaluate the final metric values (between 1 and 9)
#'
#' @return - results from the given week
#'
lowo_evaluate <- function(week)
{
  all_weeks <- 1:9
  
  train_weeks <- all_weeks[all_weeks != week]
  
  # Model Training ######
  # Train participation model ######
  
  # TODO
  
  # Train tackle model #######
  
  # TODO
  
  # Evaluation ######
  # Predict participation probability #######
  
  # TODO
  
  # Create hypothetical positioning circle for a given player ########
  
  # TODO 
  
  # Determine optimal position for each player-frame observation #######
  
  # TODO 
  
  # Aggregate week results ######
  
  # TODO
  
  return(0)
}


