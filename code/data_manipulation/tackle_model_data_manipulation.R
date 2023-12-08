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


preliminary_model_data <- week1_standardized #%>% 
  # filter(will_have_chance_to_make_tackle == 1) %>%
  # TODO - need to filter down to points within the frames of interest
  # remember frames of interest are:
  # - after handoff and before the end of the play for designed run plays
  # - after the reception and before the end of the play for passes
  # - after the QB crosses the LOS and before the end of the play for undesigned 
  #   QB runs
  # this information can be added to the add_play_information() function
  # group_by(nflId, gameId, playId) %>% 
  # filter(frame_of_interest == 1) %>% 
  # slice_min(distance_to_ball_carrier, with_ties = F) %>% 
  # ungroup()

# NOTE: some plays have a player listed as having a chance to make the tackle
# but they are never even close to the ballcarrier. Will have to look into these
# to see if the ballcarrier is incorrectly marked or if the PFF missed tackle
# column is just incorrect

# UPDATE: definitely just the data being incorrect
