library(xgboost)
library(probably)
library(tidymodels)

source("code/data_manipulation/participation_model_data_manipulation.R")

prelim_data <- week1_standardized %>% 
  filter(frame_of_interest == 1,
         off_def == "defense")

prelim_split <- prelim_data %>% 
  mutate(ball_carrier_dir_difference = dir - ball_carrier_dir,
         ball_carrier_s_difference = s - ball_carrier_s,
         ball_carrier_a_difference = a - ball_carrier_a,
         ball_carrier_x_difference = x - ball_carrier_x,
         ball_carrier_y_difference = y - ball_carrier_y,
         ball_carrier_distance_to_sideline = pmin(160/3 - ball_carrier_y, y)) %>% 
  select(will_have_chance_to_make_tackle, distance_to_ball_carrier,
         ball_carrier_dir_difference,
         ball_carrier_s_difference, ball_carrier_a_difference,
         ball_carrier_x_difference, ball_carrier_y_difference,
         ball_carrier_distance_to_sideline) %>% 
  initial_split(prop = 0.8)


testing(prelim_split)

train_x <- training(prelim_split) %>% 
  select(-will_have_chance_to_make_tackle) %>% 
  as.matrix()

train_y <- training(prelim_split) %>% 
  select(will_have_chance_to_make_tackle) %>% 
  as.matrix()

test_x <- testing(prelim_split) %>% 
  select(-will_have_chance_to_make_tackle) %>% 
  as.matrix()

test_y <- testing(prelim_split) %>% 
  select(will_have_chance_to_make_tackle) %>% 
  as.matrix()



prelim_participation_model <- xgboost(data = train_x, label = train_y,
                                      nrounds = 100,
                                      params = list(objective = "binary:logistic"))

xgb.importance(model = prelim_participation_model)




