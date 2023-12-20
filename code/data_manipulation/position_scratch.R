library(tidyverse)
library(future)
library(furrr)

path <- "C:/Users/Michael Egle/BDB2024"

setwd(path)

source("code/util/create_and_standardize_week_data.R")
source("code/modeling/train_participation_model.R")
source("code/modeling/train_tackle_model.R")
source("code/modeling/create_position_radius.R")
source("code/modeling/predict_at_hypothetical_point.R")

plays <- read_csv("data/plays.csv")
tackles <- read_csv("data/tackles.csv")


test <- readRDS("misc/participation_model_pred.RDS")

tackle_model <- readRDS("models/tackle_model.RDS")

test_subset <- test %>% 
  filter(week == 1)

tictoc::tic()
test_position_radius <- create_position_radius(test_subset)
tictoc::toc()

print(nrow(test_position_radius))

test_position_radius_with_all_predictors <- test_position_radius %>% 
  left_join(test_subset %>% 
              select(nflId, gameId, playId, frameId, 
                     ball_carrier_s_difference,
                     ball_carrier_dir_difference,
                     dir,
                     ball_carrier_s,
                     s,
                     ball_carrier_distance_to_sideline,
                     ball_carrier_distance_to_endzone,
                     # note that the min distance can change, we will update
                     # that later
                     min_distance_to_ball_carrier))

# now test the predict_at_hypothetical_point function


offensive_player_locations <- get_offensive_player_locations(1, plays)

tictoc::tic()

prediction_data <- test_position_radius_with_all_predictors %>% 
  filter(week == 1) %>% 
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

tictoc::toc()

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

final_predictions <- test_position_radius_with_all_predictors %>% 
  bind_cols(tackle_prob = pred) %>% 
  select(nflId, gameId, playId, frameId, hypothetical_position_id,
         x, y, true_x, true_y, tackle_prob)

final_predictions %>% 
  filter(nflId == 52462) %>% 
  filter(gameId == 2022090800) %>% 
  filter(playId == 1836) %>% 
  filter(frameId == 20) -> new_pred_test #%>% 
  ggplot(aes(x = x, y = y, fill = tackle_prob)) +
  geom_raster()


