

#' Predict tackle probability at a specific hypothetical point
#'
#' @param hypothetical_positions 
#' @param tackle_model 
#'
#' @return
#' @export
#'
#' @examples
predict_at_hypothetical_point <- function(hypothetical_positions, tackle_model)
{
  # We'll need to add the positions of the possible blockers and ball carrier
  # back to this data, as well as recalculate the angles, distances, etc
  
  prediction_data <- hypothetical_positions %>% 
    left_join(
      # TODO - add the offensive player positions
    ) %>% 
    mutate(
      # TODO - calculate the angles, distances, etc
    ) %>% 
    group_by(nflId, gameId, playId, frameId, hypothetical_position_id) %>% 
    summarize(
      # TODO - aggregate down to one observation for each player-frame-hypothetical position observation
    )
  
  # Then use predict function
  
  pred <- predict(tackle_model, prediction_data)
  
  final_predictions <- prediction_data %>% 
    bind_cols(tackle_prob = pred) %>% 
    select(nflId, gameId, playId, frameId, hypothetical_position_id,
           x, y, true_x, true_y, tackle_prob)
  
  return(final_predictions)
  
}