library(tidyverse)

setwd("/Users/michaelegle/BDB2024")
source("code/data_manipulation/load_data.R")
source("code/util/add_play_information.R")
source("code/util/add_tackle_information.R")
source("code/util/standardize_tracking_data.R")

week1_standardized <- week1 %>% 
  add_tackle_information(tackles) %>% 
  standardize_tracking_data() %>% 
  add_play_information(plays)

# This model is on the condition that a player WILL have a chance to make a
# tackle at that frame. But how do we define that a player will have a chance
# at making a tackle?

# - for players who are involved in a play, take 

preliminary_participation_model_data <- week1_standardized %>% 
  filter(frame_of_interest == 1) %>% 
  filter(club == defensiveTeam)

# NOTE: some plays have a player listed as having a chance to make the tackle
# but they are never even close to the ballcarrier. Will have to look into these
# to see if the ballcarrier is incorrectly marked or if the PFF missed tackle
# column is just incorrect

# UPDATE: definitely just the data being incorrect