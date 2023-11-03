#' Standardize Tracking Data
#'
#' @param track - dataframe of tracking data
#'
#' @return - standardized tracking data such that the offense is always moving to the same side
#'
standardize_tracking_data <- function(track)
{
  track_standardized <- track %>% 
    mutate(x = ifelse(playDirection == "left", 120 - x, x),
           y = ifelse(playDirection == "left", 160/3 - y, y),
           dir = case_when(playDirection == "left" & dir <= 180 ~ dir + 180,
                           playDirection == "left" & dir > 180 ~ dir - 180,
                           T ~ dir),
           o = case_when(playDirection == "left" & o <= 180 ~ o + 180,
                         playDirection == "left" & o > 180 ~ o - 180,
                         T ~ o))
  
  return(track_standardized)
}