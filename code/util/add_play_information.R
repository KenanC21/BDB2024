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
    mutate(is_ball_carrier = ifelse(nflId == ballCarrierId, 1, 0),
           off_def = ifelse(club == possessionTeam, "offense", "defense")) %>% 
    group_by(gameId, playId, frameId) %>% 
    mutate(ball_carrier_x = x[is_ball_carrier == 1],
           ball_carrier_y = y[is_ball_carrier == 1]) %>% 
    ungroup()
  
  return(track_with_play_info)
}