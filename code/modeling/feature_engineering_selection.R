# Use this scratch file to think of new features from the tracking data for
# the models

# Ideas:
# - does the defender have a clear path to the ballcarrier?
# - think about the players' location, speed, and direction as vectors
#   and look at the projection?
# - look at angles created by players and ballcarrier
# - ball carrier distance to sideline, distance to endzone
# - try some interaction terms between differences in speed/acceleration/distance
# - or maybe just raw speed/acceleration


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




