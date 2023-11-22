setwd("/Users/michaelegle/BDB2024")

tackles %>% 
  mutate(had_chance = pmax(tackle, assist, pff_missedTackle)) %>% 
  group_by(gameId, playId) %>% 
  summarize(players_with_chance = sum(had_chance)) %>% 
  ggplot(aes(x = players_with_chance)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "Distribution of # of Players With a Chance to Make a Tackle",
       subtitle = "Chance = Any Player Who Records Tackle, Assist, or Missed Tackle")


