# NOTE: This is for the final model to be trained on the leave one week out
# (LOWO) data for the final data aggregation

#' Determine the optimal position of a player at a given frame of a play
#'
#' @param week - the dataframe of all hypothetical positions for the player-frame
#'               observation in question. 
#'
#' @return - dataframe of the optimal position (x, y) for each player-frame
#'           observation. will have to include player, frame, play, and game IDs
#'           in order to map back to original dataframe 
#'
determine_optimal_position <- function(week)
{
  # TODO - any column that involves the position of the player must be updated
  #        with their "new" position on the field
  
  week_updated <- week %>% 
    mutate()
  
  return(NULL)
}