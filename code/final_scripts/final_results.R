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

season_results %>% 
  ggplot(aes(x = avg_misalignment, y = avg_tackle_prob_lost)) +
  geom_point() +
  facet_grid(. ~ run_pass)


# About 20 snaps involved for pass, 30 for run?
season_results %>% 
  ggplot(aes(x = snaps)) +
  geom_histogram() +
  facet_grid(. ~ run_pass)

season_results_pass <- season_results %>% 
  filter(run_pass == "pass") %>% 
  filter(snaps >= 25) %>% 
  group_by(position) %>% 
  arrange(avg_misalignment) %>% 
  mutate(misalignment_rank = 1:n()) %>% 
  arrange(desc(avg_tackle_prob_lost)) %>% 
  mutate(tackle_prob_lost_rank = 1:n()) %>% 
  ungroup()
  
season_results_run <- season_results %>% 
  filter(run_pass == "run") %>% 
  filter(snaps >= 30) %>% 
  group_by(position) %>% 
  arrange(avg_misalignment) %>% 
  mutate(misalignment_rank = 1:n()) %>% 
  arrange(desc(avg_tackle_prob_lost)) %>% 
  mutate(tackle_prob_lost_rank = 1:n()) %>% 
  ungroup()



# Aggregate at play level then season -------------------------------------

weighted_results <- optimal_positions %>% 
  group_by(nflId, gameId, playId) %>% 
  # aggregating at play level first to avoid longer plays having higher weights
  summarize(avg_misalignment = mean(distance_from_actual_position),
            avg_tackle_prob_lost = mean(prob_difference)) %>% 
  ungroup() %>% 
  left_join(plays, by = c("gameId", "playId")) %>% 
  group_by(nflId, run_pass) %>% 
  summarize(avg_misalignment = mean(avg_misalignment),
            avg_tackle_prob_lost = mean(avg_tackle_prob_lost),
            snaps = n_distinct(paste0(gameId, playId, sep = "_"))) %>% 
  ungroup() %>% 
  left_join(players %>% 
              select(nflId, position, displayName),
            by = "nflId")

weighted_results_pass <- weighted_results %>% 
  filter(run_pass == "pass") %>% 
  filter(snaps >= 20) %>%
  group_by(position) %>% 
  arrange(avg_misalignment) %>% 
  mutate(misalignment_rank = 1:n()) %>% 
  arrange(desc(avg_tackle_prob_lost)) %>% 
  mutate(tackle_prob_lost_rank = 1:n()) %>% 
  ungroup()

weighted_results_run <- weighted_results %>% 
  filter(run_pass == "run") %>% 
  filter(snaps >= 30) %>% 
  group_by(position) %>% 
  arrange(avg_misalignment) %>% 
  mutate(misalignment_rank = 1:n()) %>% 
  arrange(desc(avg_tackle_prob_lost)) %>% 
  mutate(tackle_prob_lost_rank = 1:n()) %>% 
  ungroup()


# What about the first frame where the player has a chance to make a tackle? --------

first_chance <- optimal_positions %>% 
  group_by(nflId, gameId, playId) %>% 
  slice_min(order_by = frameId, n = 1, with_ties = F) %>% 
  ungroup()

first_chance_results <- first_chance %>% 
  left_join(plays, by = c("gameId", "playId")) %>% 
  group_by(nflId, run_pass) %>% 
  summarize(avg_misalignment = mean(distance_from_actual_position),
            avg_tackle_prob_lost = mean(prob_difference),
            snaps = n_distinct(paste0(gameId, playId, sep = "_"))) %>% 
  ungroup() %>% 
  left_join(players %>% 
              select(nflId, position, displayName),
            by = "nflId")

first_chance_tackle_results_pass <- first_chance_results %>% 
  filter(run_pass == "pass") %>% 
  filter(snaps >= 20) %>%
  group_by(position) %>% 
  arrange(avg_misalignment) %>% 
  mutate(misalignment_rank = 1:n()) %>% 
  arrange(desc(avg_tackle_prob_lost)) %>% 
  mutate(tackle_prob_lost_rank = 1:n()) %>% 
  ungroup()

first_chance_tackle_results_run <- first_chance_results %>% 
  filter(run_pass == "run") %>% 
  filter(snaps >= 30) %>% 
  group_by(position) %>% 
  arrange(avg_misalignment) %>% 
  mutate(misalignment_rank = 1:n()) %>% 
  arrange(desc(avg_tackle_prob_lost)) %>% 
  mutate(tackle_prob_lost_rank = 1:n()) %>% 
  ungroup()


# Look at tackle probability straight up ----------------------------------

first_chance %>% 
  left_join(plays, by = c("gameId", "playId")) %>% 
  group_by(nflId, run_pass) %>% 
  summarize(tackle_prob = mean(actual_tackle_prob),
            max_tackle_prob = mean(max_tackle_prob),
            snaps = n_distinct(paste0(gameId, playId, sep = "_"))) %>% 
  ungroup() %>% 
  left_join(players %>% 
              select(nflId, position, displayName),
            by = "nflId")

