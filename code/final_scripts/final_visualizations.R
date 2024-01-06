library(tidyverse)
source("code/util/load_tracking_data.R")
source("code/util/standardize_tracking_data.R")
source("code/util/animate_play.R")
source("code/util/add_play_information.R")

# Read in data ------------------------------------------------------------

participation_pred <- readRDS("misc/participation_model_pred.RDS") %>% 
  filter(week == 1)
plays <- read_csv("data/plays.csv")

tackle_probs <- readRDS("misc/pred_tackle_probs_at_all_points_week_1.RDS") %>% 
  filter(gameId == 2022090800) %>% 
  filter(playId == 101)

week1 <- load_tracking_data(1) %>% 
  standardize_tracking_data() %>% 
  add_play_information(plays)

# Create sample play animation --------------------------------------------

sample_play <- week1 %>% 
  filter(gameId == 2022090800) %>% 
  filter(playId == 101) %>% 
  left_join(participation_pred %>% 
              select(nflId, gameId, playId, frameId, pred_tackle_participation),
            by = c("gameId", "playId", "frameId", "nflId")) %>% 
  mutate(pred_tackle_participation = factor(replace_na(pred_tackle_participation, 0))) %>% 
  mutate(class = case_when(is.na(nflId) ~ "football",
                           club == possessionTeam ~ "offense",
                           club != possessionTeam & pred_tackle_participation == 0 ~ "defense, no chance",
                           club != possessionTeam & pred_tackle_participation == 1 ~ "defense, chance"))

chances <- sample_play %>% filter(pred_tackle_participation == 1)

animation <- sample_play %>% 
  animate_play_participation(2022090800, 101)

anim_save(animation = animation, filename = "viz/participation_example.gif")



# Highlight example of the hypothetical position circle -------------------

sample_play_subset <- sample_play %>% 
  filter(frameId %in% 26:32) %>% 
  filter(nflId %in% c(41239, 47939, 43335, 48512, 47857, NA))

sample_play_tackle_probs <- tackle_probs %>% 
  filter(nflId == 41239) %>%  # look at Aaron Donald
  filter(frameId %in% 26:32)



anim <- ggplot() +
  geom_rect(aes(xmin = 42, xmax = 45, ymin = 14, ymax = 24), fill = "#76b03f", colour = "#FFFFFF", linewidth = 0.5) +
  geom_rect(aes(xmin = 45, xmax = 48, ymin = 14, ymax = 24), fill = "#669933", colour = "#FFFFFF", linewidth = 0.5) +
  geom_raster(data = sample_play_tackle_probs,
              aes(x = x, y = y, fill = tackle_prob)) +
  scale_fill_viridis_c() +
  geom_point(data = sample_play_subset, aes(x = x, y = y, color = club, group = nflId), alpha = 0.7,
             size = 6.5,
             inherit.aes = F) +
  scale_color_manual(values = c("#002244", "#654321", "#e31837"), guide = "none") +
  geom_text(data = sample_play_subset, aes(x = x, y = y, label = jerseyNumber), color = "white",
            vjust = 0.36, size = 3.5,
            inherit.aes = F) +
  transition_time(frameId) +
  theme(rect = element_blank(), ##This removes all of the exterior lines from a typical ggplot
        line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        text = element_text(family = "Roboto")) +
  labs(fill = "Tackle Probability") +
  coord_fixed()

animation_2 <- animate(anim, nframes = 41, start_pause = 10, end_pause = 10, width = 1000)

anim_save(filename = "viz/tackle_probs_example.gif", animation = animation_2)


# sample_frame %>% 
#   ggplot(aes(x = x, y = y, fill = tackle_prob)) +
#   geom_raster() +
#   scale_fill_viridis_c() +
#   theme_minimal() +
#   geom_point(data = sample_play %>% 
#                filter(frameId == 31),
#              aes(x = x, y = y, color = club),
#              inherit.aes = F)





# Plot for run-pass splits for safeties and linebackers -------------------

source("code/final_scripts/final_results.R")

# 
season_results_rp_wider_2 <- season_results_rp %>% 
  pivot_wider(id_cols = c(displayName, nflId, position),
              values_from = c(avg_misalignment, snaps),
              names_from = run_pass) %>% 
  filter(!is.na(avg_misalignment_run) & !is.na(avg_misalignment_pass)) %>% 
  filter(snaps_run >= 20 & snaps_pass >= 20)

run_pass_misalignment_plot <- season_results_rp_wider_2 %>% 
  ggplot(aes(x = avg_misalignment_run, y = avg_misalignment_pass, color = position)) +
  geom_point(size = 2.5) +
  geom_point(size = 2.5, shape = 1, color = "black") +
  geom_text(data = season_results_rp_wider_2 %>% 
              filter(avg_misalignment_pass < 0.67 | avg_misalignment_run < 0.68),
            aes(x = avg_misalignment_run, y = avg_misalignment_pass, label = displayName),
            inherit.aes = F,
            nudge_x = 0.001,
            nudge_y = -0.0016,
            hjust = "left",
            size = 3) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Average Misalignment in Run/Pass Tackle Opportunities",
       subtitle = "Minimum of 20 Snaps on Run and Pass Plays",
       x = "Average Misalignment on Run Plays",
       y = "Average Misalignment on Pass Plays") +
  theme(text = element_text(family = "Roboto"))


ggsave(plot = run_pass_misalignment_plot, filename = "viz/run_pass_misalignment_plot.jpeg",
       width = 8, units = "in")


