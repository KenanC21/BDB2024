library(tidyverse)

# set your working directory to wherever you have your repository
setwd("/Users/michaelegle/BDB2024")
week1 <- read_csv("data/tracking_week_1.csv")
source("code/util/animate_play.R")
source("code/util/standardize_tracking_data.R")
source("code/util/add_play_information.R")
source("code/util/add_tackle_information.R")

plays <- read_csv("data/plays.csv")
tackles <- read_csv("data/tackles.csv")

# change game_id and play_id to whichever play you'd like to visualize

week1_standardized <- week1 %>% 
  standardize_tracking_data() %>% 
  add_play_information(plays) %>% 
  add_tackle_information(tackles)

animate_play(week1_standardized, game_id = 2022090800, play_id = 101)

test <- week1_standardized %>% 
  filter(playId == 2043) %>% 
  filter(gameId == 2022090800) %>% 
  filter(frame_of_interest == 1) %>% 
  filter(frameId == 18)

test %>% 
  ggplot(aes(x = x, y = y, color = club)) +
  geom_point() +
  coord_fixed()

###### DO NOT DELETE OR OVERRIDE ###############################################
tictoc::tic()
test_with_values <- test %>% 
  filter(frame_of_interest == 1) %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(possible_blocker_id = list(nflId[off_def == "offense" & is_ball_carrier == 0]),
         possible_blocker_x = list(x[off_def == "offense" & is_ball_carrier == 0]),
         possible_blocker_y = list(y[off_def == "offense" & is_ball_carrier == 0])) %>% 
  ungroup() %>% 
  unnest(cols = starts_with("possible_blocker_")) %>% 
  filter(off_def == "defense") %>%
  filter(!is.na(nflId)) %>% 
  mutate(distance_to_possible_blocker = sqrt((x - possible_blocker_x)^2 + (y - possible_blocker_y)^2),
         # do some geometry
         # NOTE: this is outdated from the final code for this task
         distance_blocker_to_defender_x = x - possible_blocker_x,
         distance_blocker_to_defender_y = y - possible_blocker_y,
         distance_blocker_to_defender = sqrt(distance_blocker_to_defender_x^2 + distance_blocker_to_defender_y^2),
         distance_blocker_to_ball_carrier_x = possible_blocker_x - ball_carrier_x,
         distance_blocker_to_ball_carrier_y = possible_blocker_y - ball_carrier_y,
         distance_blocker_to_ball_carrier = sqrt(distance_blocker_to_ball_carrier_x^2 + distance_blocker_to_ball_carrier_y^2),
         theta = acos(((distance_blocker_to_defender_x * distance_blocker_to_ball_carrier_x) + 
                        (distance_blocker_to_defender_y * distance_blocker_to_ball_carrier_y)) / 
                        (distance_blocker_to_defender * distance_blocker_to_ball_carrier)),
         theta = ifelse(theta > pi, theta - pi, theta)) %>% 
  group_by(gameId, playId, nflId, frameId) %>% 
tictoc::toc()
################################################################################

# Data Aggregation
players <- read_csv("data/players.csv")
games <- read_csv("games.csv")
tackles <- read_csv("data/tackles.csv")

# Load in Tracking Data
for (wk in 1:9){
  week <- read_csv(paste0("tracking_week_", wk, '.csv')) 
  assign(paste0("week", wk), week)
}

# subset run plays, passResult = NA
plays_na <- subset(plays, is.na(plays$passResult))
