library(tidyverse)
#' Load tracking data function
#'
#' @param week - week of NFL season for which to read tracking data
#'
#' @return - dataframe of the raw tracking data
#'
load_tracking_data <- function(week)
{
  return(read_csv(paste0("data/tracking_week_", week, ".csv")) %>% mutate(week = week))
}