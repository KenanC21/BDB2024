# TODO - this will be a function (or series of functions) to load in the data
# for the final scripts
library(tidyverse)

source("code/util/add_play_information.R")
source("code/util/add_tackle_information.R")
source("code/util/standardize_tracking_data.R")
source("code/util/load_tracking_data.R")

create_and_standardize_week_data <- function(week)
{
  print(paste("Loading Week", week))
  week_standardized <- load_tracking_data(week) %>% 
    standardize_tracking_data() %>% 
    add_play_information(plays) %>% 
    add_tackle_information(tackles) %>% 
    filter(frame_of_interest == 1) %>% 
    group_by(gameId, playId, frameId) %>% 
    # going to make this data even bigger before it gets smaller so we can add 
    # important columns such as angles and distances to possible blockers.
    # first list out all the possible blockers and their locations
    mutate(possible_blocker_id = list(nflId[off_def == "offense" & is_ball_carrier == 0]),
           possible_blocker_x = list(x[off_def == "offense" & is_ball_carrier == 0]),
           possible_blocker_y = list(y[off_def == "offense" & is_ball_carrier == 0])) %>% 
    ungroup() %>% 
    # aggregate up to 10 observations for each player-frame observation, one row
    # for each defender-possible blocker pair
    unnest(cols = starts_with("possible_blocker_")) %>% 
    # only want defenders
    filter(off_def == "defense") %>% 
    # get rid of the ball
    filter(!is.na(nflId)) %>% 
    # use geometry to find the angle created by the defender, each possible blocker, and the ball carrier
    # TODO - I *think* this is calculating the angle that we should be concerned with, but not entirely sure
    mutate(distance_to_possible_blocker = sqrt((x - possible_blocker_x)^2 + (y - possible_blocker_y)^2),
           # do some geometry
           distance_to_ball_carrier_x = x - ball_carrier_x,
           distance_to_ball_carrier_y = y - ball_carrier_y,
           distance_to_possible_blocker_x = possible_blocker_x - x,
           distance_to_possible_blocker_y = possible_blocker_y - y,
           # refresher: cos(theta) = (a * b) / (||a||||b||)
           # where a and b are two vectors
           # in this case we set a to be the vector formed by the defender and ball carrier
           # and b to be the vector formed by the defender and the possible blocker
           theta = acos(((distance_to_ball_carrier_x * distance_to_possible_blocker_x) + 
                           (distance_to_ball_carrier_y * distance_to_possible_blocker_y)) / 
                          (distance_to_ball_carrier * distance_to_possible_blocker)),
           theta = ifelse(theta > pi, theta - pi, theta)) %>% 
    group_by(gameId, playId, nflId, frameId) %>%
    # TODO - this can have a little more done on it, should ideally be a summarize()
    # or at least have an intermediate mutate() to get as much information out of
    # blocker locations as possible
    slice_max(order_by = theta, n = 1, with_ties = F)
  
  return(week_standardized)
}