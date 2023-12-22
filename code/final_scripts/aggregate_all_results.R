library(tidyverse)

path <- "/Users/michaelegle/BDB2024"

setwd(path)

all_files <- paste0("misc/pred_tackle_probs_at_all_points_week_", 1:9, ".RDS")

all_week_results <- pmap_dfr(.l = list(all_files),
                             .f = readRDS)


optimal_positions <- all_week_results %>%
  mutate(distance_from_actual_position = sqrt((x - true_x)^2 + (y - true_y)^2)) %>%
  group_by(nflId, gameId, playId, frameId, week) %>%
  arrange(distance_from_actual_position) %>%
  summarize(max_tackle_prob = max(tackle_prob),
            optimal_position_x = first(x[tackle_prob == max_tackle_prob]),
            optimal_position_y = first(y[tackle_prob == max_tackle_prob]),
            distance_from_actual_position = first(distance_from_actual_position[tackle_prob== max_tackle_prob]),
            actual_tackle_prob = tackle_prob[x == true_x & y == true_y],
            prob_difference = actual_tackle_prob - max_tackle_prob)

optimal_positions %>%
  group_by(week) %>%
  summarize(games = n_distinct(gameId),
            plays = n_distinct(paste(gameId, playId, sep = "_")),
            frames = n_distinct(paste(gameId, playId, frameId, sep = "_")))

saveRDS(optimal_positions, "misc/optimal_positions.RDS")

print(dim(optimal_positions))

print(optimal_positions %>% head(20))