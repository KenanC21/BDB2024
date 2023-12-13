# TODO - this will have all final code to run everything at once
library(svDialogs)
path <- dlg_input(message = "Input your working directory:",
                  default = "C:/DIRECTORY/BDB2024")$res

setwd(path)
source("code/util/create_and_standardize_week_data.R")
source("code/modeling/lowo_modeling.R")

# load in supplementary data
plays <- read_csv("data/plays.csv")
tackles <- read_csv("data/tackles.csv")

# Step 1: Load in / clean data for training
# - standardize tracking data
# - add play info
# - add tackle info
# - filter down to frames of interest
# - add blocker info
# - filter down to defensive player observations
# - aggregate back down to one observation per player-frame from adding blocker info

tictoc::tic()
all_data <- pmap_dfr(.l = list(1:9),
                     .f = create_and_standardize_week_data,
                     .progress = T)
tictoc::toc()

print("Loaded in all tracking data")

# Make sure that all the data is there
# all_data %>% 
#   group_by(week) %>% 
#   summarize(games = n_distinct(gameId),
#             plays = n_distinct(paste(gameId, playId, sep = "_")),
#             frames = n_distinct(paste(gameId, playId, frame, sep = "_")))

# TODO
# Step 2: Train participation model
# - use 80/20 training/validation split to obtain optimal parameters
# - use leave-one-week-out (LOWO) approach to ensure out of sample predictions 
#   with the optimal paramters found above
# - append predictions to original dataframe
# - filter down to player-frame observations where the player had a chance to make a tackle

participation_model_predictions <- train_participation_model(data.frame())




# TODO
# Step 3: Train tackle probability model
# - Use 80/20 training/validation split to obtain optimal parameters
# - Train final model on all observations using optimal parameters found above

tackle_model <- train_tackle_model(participation_model_predictions)

# TODO
# Step 4: Create each player's hypothetical position circles
# - update all columns that involve a player's position (distance, angles, etc)

# TODO
# Step 5: Predict tackle probability at each point

# TODO
# Step 6: Find distance from optimal position for each player-frame observation

# TODO
# Step 7: Aggregate results for game/season level






