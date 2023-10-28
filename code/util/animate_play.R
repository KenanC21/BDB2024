source("code/util/field_plot.R")
library(assertthat)
library(gganimate)

#' Animate a play
#'
#' @param tracking_data - raw tracking data
#' @param play_id - play ID of desired play to animate
#'
#' @return - animation object of the given play
#'
animate_play <- function(tracking_data, game_id, play_id)
{
  play <- tracking_data %>% 
    filter(gameId == game_id) %>% 
    filter(playId == play_id)
  
  assert_that(nrow(play) > 0, msg = "No such play ID found in data")
  
  play.length.ex <- length(unique(play$frameId))
  
  anim <- football_field +
    geom_point(data = play, aes(x = x, y = y, color = club, group = nflId), alpha = 0.7,
               size = 6.5) +
    scale_color_manual(values = c("#002244", "#654321", "#e31837"), guide = "none") +
    geom_text(data = play, aes(x = x, y = y, label = jerseyNumber), color = "white",
              vjust = 0.36, size = 3.5) +
    transition_time(frameId) +
    theme(text = element_text(family = "Roboto"),
          axis.text = element_blank(),
          axis.title = element_blank()) +
    ease_aes('linear') +
    coord_fixed()
  
  final_animation <- animate(anim, fps = 10, nframes = play.length.ex + 5, width = 1000, end_pause = 5)
  
  return(final_animation)
}
