library(tidyverse)
library(dplyr)

source("code/final_scripts/final_results.R")

Top_15_pass = season_results_pass %>%
  group_by(position) %>% 
  filter(misalignment_rank < 16)
Top_15_pass = Top_15_pass %>% relocate(nflId)
Top_15_pass = Top_15_pass %>% relocate(misalignment_rank)

#load_roster from nflreadr to get 2021 headshots
roster = nflreadr::load_rosters(2022)
names(roster)[7] <- "displayName"
newdf = merge(Top_15_pass, roster, by = 'displayName')


#DF updated with headshot URL for Pass
Top_15_pass = Top_15_pass %>% arrange(displayName)
Top_15_pass = bind_cols(Top_15_pass,newdf %>% 
                           select(headshot_url))


#repeat for running plays results
Top_15_run = season_results_run %>%
  group_by(position) %>% 
  filter(misalignment_rank < 16)
Top_15_run = Top_15_run %>% relocate(nflId)
Top_15_run = Top_15_run %>% relocate(misalignment_rank)

#DF updated with headshot URL for Run
Top_15_run = Top_15_run %>% arrange(displayName)
Top_15_run = bind_cols(Top_15_run,newdf %>% 
                          select(headshot_url))

































