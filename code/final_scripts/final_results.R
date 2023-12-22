library(tidyverse)

path <- "/Users/michaelegle/BDB2024"

setwd(path)

optimal_positions <- readRDS("misc/optimal_positions.RDS")
players <- read_csv("data/players.csv")
plays <- read_csv("data/plays.csv") %>% 
  mutate(run_pass = ifelse(is.na(passResult), "run", "pass"))

optimal_positions %>% 
  ggplot(aes(x = distance_from_actual_position)) +
  geom_histogram()


season_results <- optimal_positions %>% 
  left_join(plays, by = c("gameId", "playId")) %>% 
  group_by(nflId, run_pass) %>% 
  summarize(avg_misalignment = mean(distance_from_actual_position),
            avg_tackle_prob_lost = mean(prob_difference),
            frames = n(),
            snaps = n_distinct(paste0(gameId, playId, sep = "_"))) %>% 
  ungroup() %>% 
  left_join(players %>% 
              select(nflId, position, displayName),
            by = "nflId")

mean(total_snaps$snaps >= 75)

season_results %>% 
  ggplot(aes(x = avg_misalignment, y = avg_tackle_prob_lost)) +
  geom_point() +
  facet_grid(. ~ run_pass)


# About 30 snaps involved for pass, 50 for run?
season_results %>% 
  ggplot(aes(x = snaps)) +
  geom_histogram() +
  facet_grid(. ~ run_pass)

season_results_pass <- season_results %>% 
  filter(run_pass == "pass") %>% 
  filter(snaps >= 30) %>% 
  group_by(position) %>% 
  arrange(avg_misalignment) %>% 
  mutate(misalignment_rank = 1:n()) %>% 
  arrange(avg_tackle_prob_lost) %>% 
  mutate(tackle_prob_lost_rank = 1:n()) %>% 
  ungroup()
  
season_results_run <- season_results %>% 
  filter(run_pass == "run") %>% 
  filter(snaps >= 50) %>% 
  group_by(position) %>% 
  arrange(avg_misalignment) %>% 
  mutate(misalignment_rank = 1:n()) %>% 
  arrange(avg_tackle_prob_lost) %>% 
  mutate(tackle_prob_lost_rank = 1:n()) %>% 
  ungroup()

