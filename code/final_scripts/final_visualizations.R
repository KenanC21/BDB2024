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



# Create hypothetical positions frame plot --------------------------------

sample_frame <- tackle_probs %>% 
  filter(nflId == 41239) %>%  # look at Aaron Donald
  filter(frameId == 31)

sample_frame %>% 
  ggplot(aes(x = x, y = y, fill = tackle_prob)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_minimal() +
  geom_point(data = sample_play %>% 
               filter(frameId == 31),
             aes(x = x, y = y, color = club),
             inherit.aes = F)
