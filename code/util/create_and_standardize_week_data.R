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
    # TODO - maybe try the angles from other points, see if those yield any interesting results
    mutate(distance_blocker_to_defender = sqrt((possible_blocker_x - x)^2 + (possible_blocker_y - y)^2),
           distance_blocker_to_ball_carrier = sqrt((ball_carrier_x - possible_blocker_x)^2 + (ball_carrier_y - possible_blocker_y)^2),
           theta = acos((distance_blocker_to_defender^2 + distance_blocker_to_ball_carrier^2 - distance_to_ball_carrier^2) /
                          (2 * distance_blocker_to_defender * distance_blocker_to_ball_carrier)),
           # refresher: law of cosines says cos(b) = (c^2 + a^2 - b^2) / 2*a*c
           # where a is the distance from possible blocker to ball carrier
           # b is the distance from defender to ball carrier
           # and c is the distance from possible blocker to defender
           theta = ifelse(theta > pi, theta - pi, theta)) %>% 
    group_by(gameId, playId, nflId, frameId) %>%
    # TODO - this can have a little more done on it, should ideally be a summarize()
    # or at least have an intermediate mutate() to get as much information out of
    # blocker locations as possible
    summarize(across(c(x, y, s, a, dir, 
                       will_have_chance_to_make_tackle, will_make_tackle,
                       distance_to_ball_carrier,
                       ball_carrier_distance_to_sideline,
                       ball_carrier_distance_to_endzone, ball_carrier_s, 
                       ball_carrier_a, ball_carrier_dir,
                       ball_carrier_a_difference, ball_carrier_s_difference,
                       ball_carrier_dir_difference),
                     first),
              max_distance_from_blocker = max(distance_blocker_to_defender),
              possible_blockers_within_3_yards = sum(distance_blocker_to_defender <= 3),
              possible_blockers_within_4_yards = sum(distance_blocker_to_defender <= 4),
              possible_blockers_within_5_yards = sum(distance_blocker_to_defender <= 5),
              possible_blockers_within_6_yards = sum(distance_blocker_to_defender <= 6),
              possible_blockers_within_7_yards = sum(distance_blocker_to_defender <= 7),
              possible_blockers_with_over_90_degree_angle = sum(theta >= pi/2),
              possible_blockers_with_over_135_degree_angle = sum(theta >= 3 * pi / 4),
              max_angle_formed_by_blocker_and_ball_carrier = max(theta)) %>% 
    ungroup() %>% 
    group_by(gameId, playId, frameId) %>% 
    mutate(min_distance_to_ball_carrier = min(distance_to_ball_carrier),
           difference_min_distance_to_ball_carrier = distance_to_ball_carrier - min_distance_to_ball_carrier) %>% 
    ungroup()
      
  
  
  return(week_standardized)
}
