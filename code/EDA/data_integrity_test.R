# test each week's tracking data to look for missing frames, players etc

#' Check for missingness in tracking data
#'
#' @param week_num - week number to test
#'
#' @return - a data frame of plays and frames with missing data
#'
check_for_missing <- function(week_num)
{
  week <- read_csv(paste0("data/tracking_week_", week_num, ".csv"))
  
  grouped_by_frame <- week1 %>% 
    group_by(gameId, playId, frameId) %>% 
    count() %>% 
    filter(n != 23) %>% 
    select(-n)
  
  grouped_by_play <- week1 %>% 
    group_by(gameId, playId) %>% 
    summarize(min_frame = min(frameId),
              max_frame = max(frameId),
              frames_observed = n_distinct(frameId),
              frames_expected = max_frame - min_frame + 1) %>% 
    filter(frames_observed != frames_expected) %>% 
    select(gameId, playId)
  
  missing <- grouped_by_frame %>% 
    bind_rows(grouped_by_play)
  
  return(missing)
}

missing <- pmap_dfr(.l = list(seq(1, 9)),
                    .f = check_for_missing)

# it looks like there's no missing data at first glance, so that's good



