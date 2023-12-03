add_tackle_information <- function(track, tackle)
{
  track_tackle <- track %>% 
    left_join(tackle %>% 
                mutate(will_make_tackle = pmax(tackle, assist),
                       will_have_chance_to_make_tackle = pmax(tackle, assist, pff_missedTackle)) %>% 
                select(gameId, playId, nflId, will_make_tackle,
                       will_have_chance_to_make_tackle,
                       pff_missedTackle),
              by = c("gameId", "playId", "nflId")) %>% 
    mutate(will_make_tackle = replace_na(will_make_tackle, 0),
           will_have_chance_to_make_tackle = replace_na(will_have_chance_to_make_tackle, 0))
  
  return(track_tackle)
}

