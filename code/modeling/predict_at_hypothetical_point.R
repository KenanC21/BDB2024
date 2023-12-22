source("code/util/load_tracking_data.R")
source("code/util/add_play_information.R")
source("code/util/standardize_tracking_data.R")

#' Get the offensive player locations for the 
#'
#' @param week 
#'
#' @return
#' @export
#'
#' @examples
get_offensive_player_locations <- function(week, plays)
{
  # TODO 
  
  week_standardized <- load_tracking_data(week) %>% 
    standardize_tracking_data() %>% 
    add_play_information(plays) %>% 
    filter(off_def == "offense") %>% 
    select(off_nflId = nflId, gameId, playId, frameId, off_x = x, off_y = y, is_ball_carrier,
           ball_carrier_x, ball_carrier_y) %>% 
    filter(is_ball_carrier == 0)
  
  return(week_standardized)
}



#' Predict tackle probability at a specific hypothetical point
#'
#' @param hypothetical_positions 
#' @param tackle_model 
#'
#' @return
#' @export
#'
#' @examples
predict_at_hypothetical_point <- function(hypothetical_positions, tackle_model, week)
{
  # We'll need to add the positions of the possible blockers and ball carrier
  # back to this data, as well as recalculate the angles, distances, etc
  
  future::plan("multisession")
  
  gameId_list <- hypothetical_positions %>% pull(gameId) %>% unique()
  
  offensive_player_locations <- get_offensive_player_locations(week, plays)
  
  final_predictions_all_weeks <- furrr::future_pmap_dfr(.l = list(gameId_list),
                                                        .f = predict_at_hypothetical_point_helper,
                                                        hypothetical_positions = hypothetical_positions,
                                                        tackle_model = tackle_model,
                                                        week = week,
                                                        offensive_player_locations = offensive_player_locations,
                                                        .progress = T)
  
  return(final_predictions_all_weeks)
}

predict_at_hypothetical_point_helper <- function(hypothetical_positions, tackle_model, game, week,
                                                 offensive_player_locations)
{
  prediction_data <- hypothetical_positions %>% 
    filter(gameId == game) %>% 
    left_join(offensive_player_locations,
              by = c("gameId", "playId", "frameId")) %>% 
    # same code as the create_and_standardize_week_data() function
    mutate(distance_to_ball_carrier = sqrt((x - ball_carrier_x)^2 + (y - ball_carrier_y)^2),
           distance_blocker_to_defender = sqrt((off_x - x)^2 + (off_y - y)^2),
           distance_blocker_to_ball_carrier = sqrt((ball_carrier_x - off_x)^2 + (ball_carrier_y - off_y)^2),
           theta = acos((distance_blocker_to_defender^2 + distance_blocker_to_ball_carrier^2 - distance_to_ball_carrier^2) /
                          (2 * distance_blocker_to_defender * distance_blocker_to_ball_carrier)),
           # refresher: law of cosines says cos(b) = (c^2 + a^2 - b^2) / 2*a*c
           # where a is the distance from possible blocker to ball carrier
           # b is the distance from defender to ball carrier
           # and c is the distance from possible blocker to defender
           theta = ifelse(theta > pi, theta - pi, theta)) %>% 
    group_by(nflId, gameId, playId, frameId, hypothetical_position_id) %>% 
    summarize(
      # TODO - aggregate down to one observation for each player-frame-hypothetical position observation
      across(c(x, y,
               ball_carrier_s_difference,
               ball_carrier_dir_difference,
               dir,
               ball_carrier_s,
               s,
               ball_carrier_distance_to_sideline,
               ball_carrier_distance_to_endzone,
               distance_to_ball_carrier,
               min_distance_to_ball_carrier
      ), first),
      max_distance_from_blocker = max(distance_blocker_to_defender),
      possible_blockers_within_3_yards = sum(distance_blocker_to_defender <= 3),
      possible_blockers_within_4_yards = sum(distance_blocker_to_defender <= 4),
      possible_blockers_within_5_yards = sum(distance_blocker_to_defender <= 5),
      possible_blockers_within_6_yards = sum(distance_blocker_to_defender <= 6),
      possible_blockers_within_7_yards = sum(distance_blocker_to_defender <= 7),
      possible_blockers_with_over_90_degree_angle = sum(theta >= pi/2),
      possible_blockers_with_over_135_degree_angle = sum(theta >= 3 * pi / 4),
      max_angle_formed_by_blocker_and_ball_carrier = max(theta)
    ) %>% 
    ungroup() %>%
    mutate(min_distance_to_ball_carrier = pmin(min_distance_to_ball_carrier, distance_to_ball_carrier),
           difference_min_distance_to_ball_carrier = distance_to_ball_carrier - min_distance_to_ball_carrier)
  
  # Then use predict function
  
  prediction_data_matrix <- prediction_data %>% 
    select(distance_to_ball_carrier,
           difference_min_distance_to_ball_carrier,
           max_angle_formed_by_blocker_and_ball_carrier,
           ball_carrier_s_difference,
           ball_carrier_dir_difference,
           dir,
           ball_carrier_s,
           s,
           ball_carrier_distance_to_sideline,
           possible_blockers_within_7_yards,
           ball_carrier_distance_to_endzone) %>% 
    as.matrix() %>% 
    xgb.DMatrix()
  
  pred <- predict(tackle_model, newdata = prediction_data_matrix)
  
  final_predictions <- hypothetical_positions %>% 
    filter(gameId == game) %>% 
    bind_cols(tackle_prob = pred) %>% 
    select(nflId, gameId, playId, frameId, week, hypothetical_position_id,
           x, y, true_x, true_y, tackle_prob)
  
  return(final_predictions)
}
