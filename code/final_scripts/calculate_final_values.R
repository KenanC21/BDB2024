# TODO - this will have all final code to run everything at once
library(svDialogs)
library(future)
library(furrr)
# path <- dlg_input(message = "Input your working directory:",
#                   default = "C:/DIRECTORY/BDB2024")$res

path <- "C:/Users/Michael Egle/BDB2024"

setwd(path)
source("code/util/create_and_standardize_week_data.R")
source("code/modeling/train_participation_model.R")
source("code/modeling/train_tackle_model.R")
source("code/modeling/create_position_radius.R")
source("code/modeling/predict_at_hypothetical_point.R")

# load in supplementary data
plays <- read_csv("data/plays.csv")
tackles <- read_csv("data/tackles.csv")

# Step 1: Load in / clean data for training
# - standardize tracking data - done
# - add play info - done
# - add tackle info - done
# - filter down to frames of interest - done
# - add blocker info - done
# - filter down to defensive player observations - done
# - aggregate back down to one observation per player-frame from adding blocker info - done

tictoc::tic()
future::plan("multisession")
all_data <- furrr::future_pmap_dfr(.l = list(1:9),
                                   .f = create_and_standardize_week_data,
                                   .progress = T)
tictoc::toc()

print("Loaded in all tracking data")

# Make sure that all the data is there
all_data %>%
  group_by(week) %>%
  summarize(games = n_distinct(gameId),
            plays = n_distinct(paste(gameId, playId, sep = "_")),
            frames = n_distinct(paste(gameId, playId, frameId, sep = "_")))

# TODO
# Step 2: Train participation model
# - use 80/20 training/validation split to obtain optimal parameters
# - use leave-one-week-out (LOWO) approach to ensure out of sample predictions
#   with the optimal paramters found above
# - append predictions to original dataframe
# - filter down to player-frame observations where the player had a chance to make a tackle

tictoc::tic()
participation_model_predictions <- train_participation_model(all_data)
tictoc::toc()

saveRDS(participation_model_predictions, "misc/participation_model_pred.RDS")


# TODO
# Step 3: Train tackle probability model
# TODO - Use 80/20 training/validation split to obtain optimal parameters
# TODO - Train final model on all observations using optimal parameters found above

tackle_model <- train_tackle_model(participation_model_predictions)

# TODO
# Step 4: Create each player's hypothetical position circles
# TODO - update all columns that involve a player's position (distance, angles, etc)

all_position_circles <- create_position_radius(participation_model_predictions)

all_position_circles_with_all_predictor_variables <- all_position_circles %>%
  left_join(participation_model_predictions %>% select(
    nflId, gameId, playId, frameId,
    # TODO - add the remaining predictor variables that do NOT change w.r.t. the
    #        defender's location
  ))

# TODO
# Step 5: Predict tackle probability at each point

tackle_probs_by_position <- predict_at_hypothetical_point(all_position_circles, tackle_model)

# TODO
# Step 6: Find distance from optimal position for each player-frame observation

optimal_positions <- tackle_probs_by_position %>%
  mutate(distance_from_actual_position = sqrt((x - true_x)^2 + (y - true_y)^2)) %>%
  group_by(nflId, gameId, playId, frameId) %>%
  arrange(distance_from_actual_position) %>%
  summarize(max_tackle_prob = max(tackle_prob),
            optimal_position_x = first(x[tackle_probability == max_tackle_prob]),
            optimal_position_y = first(y[tackle_probability == max_tackle_prob]),
            distance_from_actual_position = first(distance_from_actual_position[tackle_prob== max_tackle_prob]),
            actual_tackle_prob = tackle_prob[x == true_x & y == true_y],
            prob_difference = actual_tackle_prob - max_tackle_prob)

saveRDS(optimal_positions, "misc/optimal_positions.RDS")

# TODO
# Step 7: Aggregate results for game/season level

saveRDS(tackle_model, "models/tackle_model.RDS")




