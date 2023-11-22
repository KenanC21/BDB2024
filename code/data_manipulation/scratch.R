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
animate_play(week1, game_id = 2022091102, play_id = 4102)

week1_standardized <- week1 %>% 
  standardize_tracking_data() %>% 
  add_play_information(plays) %>% 
  add_tackle_information(tackles)




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
