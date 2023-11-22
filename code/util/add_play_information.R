#' Add play information to tracking data
#'
#' @param track - dataframe of player tracking data
#' @param play - dataframe of play information for ballcarrier, player role, etc
#'
#' @return - tracking data with new columns for ballcarrier coordinates
#'
add_play_information <- function(track, play)
{
  track_with_play_info <- track %>% 
    left_join(play, by = c("playId", "gameId")) %>% 
    mutate(is_ball_carrier = if_else(nflId == ballCarrierId, 1, 0, 0),
           off_def = ifelse(club == possessionTeam, "offense", "defense")) %>% 
    group_by(gameId, playId, frameId) %>% 
    mutate(ball_carrier_x = x[is_ball_carrier == 1],
           ball_carrier_y = y[is_ball_carrier == 1],
           ball_carrier_s = s[is_ball_carrier == 1],
           ball_carrier_a = a[is_ball_carrier == 1],
           ball_carrier_o = o[is_ball_carrier == 1],
           ball_carrier_dir = dir[is_ball_carrier == 1],
           distance_to_ball_carrier = sqrt((x - ball_carrier_x)^2 + (y - ball_carrier_y)^2)) %>% 
    ungroup() %>% 
    mutate(end_precedence = case_when(event == "tackle" ~ 3,
                                      event == "touchdown" ~ 3,
                                      event == "out_of_bounds" ~ 3,
                                      event == "fumble_defense_recovered" ~ 2,
                                      event == "qb_slide" ~ 2,
                                      event == "qb_sack" ~ 2, # these should maybe be removed?
                                      event == "fumble_offense_recovered" ~ 1, # a little bit lower since an offensive player can advance the ball, in which case we'd want to include anything that happens afterwards
                                      event == "fumble" ~ 1,
                                      T ~ 0),
           start_precedence = case_when(event == "pass_outcome_caught" ~ 3,
                                        event == "run" ~ 3,
                                        event == "handoff" ~ 3,
                                        # These two are less ideal for start events but will work
                                        event == "lateral" ~ 2,
                                        event == "pass_arrived" ~ 2,
                                        T ~ 0)) %>% 
    group_by(gameId, playId) %>% 
    mutate(end_frame = max(frameId[end_precedence == max(end_precedence)]),
           start_frame = min(frameId[start_precedence == max(start_precedence)]),
           frame_of_interest = case_when(frameId == start_frame ~ 1,
                                         frameId == end_frame ~ 0)) %>% 
    # remove plays where there was no good start or end point to a play
    filter(max(start_precedence) > 0) %>% 
    filter(max(end_precedence) > 0) %>% 
    ungroup() %>% 
    group_by(gameId, playId, nflId) %>% 
    fill(frame_of_interest, .direction = "down") %>% 
    mutate(frame_of_interest = replace_na(frame_of_interest, 0)) %>% 
    ungroup()
  
  return(track_with_play_info)
}

