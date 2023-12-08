# TODO - this will have all final code to run everything at once
source("code/util/create_and_standardize_week_data.R")

# TODO
# Step 1: Load in / clean data for training
# - standardize tracking data
# - add play info
# - add tackle info
# - filter down to frames of interest
# - add blocker info
# - filter down to defensive player observations
# - aggregate back down to one observation per player-frame from adding blocker info

all_data

# TODO
# Step 2: Train participation model
# - use leave-one-week-out (LOWO) approach to ensure out of sample predictions
# - append predictions to original dataframe
# - filter down to player-frame obsercations where the player had a chance to make a tackle

# TODO
# Step 3: Train tackle probability model
# - use leave-one-week-out (LOWO) approach again

# TODO
# Step 4: Create each player's hypothetical position circles
# - update all columns that involve a player's position (distance, angles, etc)

# TODO
# Step 5: Predict tackle probability at each point

# TODO
# Step 6: Find distance from optimal position for each player-frame observation

# TODO
# Step 7: Aggregate results for game/season level

# load in supplementary data
plays <- read_csv("data/plays.csv")
tackles <- read_csv("data/tackles.csv")




