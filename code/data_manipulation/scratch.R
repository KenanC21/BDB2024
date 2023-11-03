library(tidyverse)

# set your working directory to wherever you have your repository
setwd("/Users/michaelegle/BDB2024")
week1 <- read_csv("data/tracking_week_1.csv")
source("code/util/animate_play.R")

plays <- read_csv("data/plays.csv")

# change game_id and play_id to whichever play you'd like to visualize
animate_play(week1, game_id = 2022090800, play_id = 80)

# take note of the nullified by penalties column in the play dataframe