library(sf)
library(future)
library(furrr)


# NOTE: This is for the final model to be trained on the leave one week out
# (LOWO) data for the final data aggregation

#' Create Position Radius
#'
#' @param week - The data for player observations. We'll use their location
#'               to create a hypothetical set of positions inside a circle
#'               with a radius of one yard.
#'               This should already be filtered down to the players who have
#'               a "chance" at making a tackle as determined by the participation
#'               model in the given frame
#'
#' @return - data frame of a certain number of points within a 1 yard radius
#'
create_position_radius <- function(week)
{
  
  # Gather all information needed to cycle through the player-frame observations
  nflId_list <- week %>% pull(nflId)
  gameId_list <- week %>% pull(gameId)
  playId_list <- week %>% pull(playId)
  frameId_list <- week %>% pull(frameId)
  week_list <- week %>% pull(week)
  x_list <- week %>% pull(x)
  y_list <- week %>% pull(y)
  
  
  future::plan("multisession", workers = availableCores())
  # Run the helper method below on all of the observations
  all_circles_df <- furrr::future_pmap_dfr(.l = list(nflId = nflId_list,
                                                     gameId = gameId_list,
                                                     playId = playId_list,
                                                     frameId = frameId_list,
                                                     week = week_list,
                                                     x = x_list,
                                                     y = y_list),
                                           .f = create_position_radius_helper,
                                           .progress = T)
  
  return(all_circles_df)
}

#' Create hypothetical positioning dataframe for a player-frame observation
#'
#' @param nflId - player ID of the player
#' @param gameId - game ID of the play
#' @param playId - play ID of the play
#' @param frameId - frame ID of the coordinates for the player
#' @param x - X coordinate of the player
#' @param y - Y coordinate of the player
#' @param week 
#'
#' @return - dataframe of hypothetical X-Y values for the player-frame observation
#'
create_position_radius_helper <- function(nflId, gameId, playId, frameId, week, x, y)
{
  # using a unit circle, we're going to create some borders for the circle
  true_x <- x
  true_y <- y
  
  angles <- seq(0, 2 * pi, pi / 10)
  x_values <- cos(angles)
  y_values <- sin(angles)
  y_values[21] <- 0

  # connect all the points
  circle_points <- list(cbind(x_values + x, y_values + y))
  
  # create a circle shape based on the points
  circle <- st_polygon(circle_points)
  
  # create a convex hull based on the circle
  convex_hull <- st_convex_hull(circle)
  
  # create a grid of points laid over the convex hull
  convex_hull_points <- st_make_grid(convex_hull, what = "centers",
                                     # 9 by 9 grid, this is arbitrary but I found
                                     # that this is the best medium for not having too
                                     # many points and having a good circular shape
                                     n = c(9, 9))
  
  # take out the points on the grid that are outside the circle
  convex_hull_points_filtered <- convex_hull_points[convex_hull]
  # pull all the points from the convex hull
  all_points <- map(.x = convex_hull_points_filtered, .f = st_coordinates)
  # and make them a dataframe
  all_points_df <- do.call(rbind, all_points) %>%
    as.data.frame() %>% 
    # need these identifiers to match this data back to the data later
    mutate(nflId = nflId,
           gameId = gameId,
           playId = playId,
           frameId = frameId,
           week = week) %>% 
    rename(x = X, y = Y) %>% 
    mutate(hypothetical_position_id = 1:n(),
           true_x = true_x,
           true_y = true_y)
  
  return(all_points_df)
}
