library(sf)

# NOTE: This is for the final model to be trained on the leave one week out
# (LOWO) data for the final data aggregation

#' Create Position Radius
#'
#' @param week - The week for which we are to create the position radii.
#'               This should already be filtered down to the players who have
#'               a "chance" at making a tackle as determined by the participation
#'               model in the given frame
#'
#' @return - data frame of a certain number of points within a 1 yard radius
#'
create_position_radius <- function(week)
{
  # TODO
  
  # Probably going to have to cycle through this one using pmap
  # also may need to alter the helper function to have a spot for player,
  # play, frame, and game IDs
  
  nflId_list <- week %>% pull(nflId)
  gameId_list <- week %>% pull(gameId)
  playId_list <- week %>% pull(playId)
  frameId_list <- week %>% pull(frameId)
  x_list <- week %>% pull(x)
  y_list <- week %>% pull(y)
  
  all_circles_df <- pmap_dfr(.l = list(nflId = nflId_list,
                                       gameId = gameId_list,
                                       playId = playId_list,
                                       frameId = frameId_list,
                                       x = x_list,
                                       y = y_list),
                             .f = create_position_radius_helper)
  
  return(all_circles_df)
}

test <- week1 %>% 
  head()

test_outcome <- create_position_radius(test)

test_outcome %>% 
  ggplot(aes(x = x, y = y, color = factor(frameId))) +
  geom_point() +
  coord_fixed()

#' Create X coordinates for hypothetical circle
#'
#' @param x - original X coordinate
#'
#' @return - vector of X values in hypothetical circle
#'
create_position_radius_helper <- function(nflId, gameId, playId, frameId, x, y)
{
  # using a unit circle, we're going to create some borders for the circle
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
                                     n = c(13, 13))
  
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
           frameId = frameId) %>% 
    rename(x = X, y = Y)
  
  return(all_points_df)
}


angles <- seq(0, 2 * pi, pi / 10)
x_values <- cos(angles)
y_values <- sin(angles)
y_values[21] <- 0



circle_points <- list(cbind(x_values + 80, y_values + 40))

circle <- st_polygon(circle_points)

convex_hull <- st_convex_hull(circle)

convex_hull_points <- st_make_grid(convex_hull, what = "centers",
                                   n = c(13, 13))

convex_hull_points <- convex_hull_points[convex_hull]
all_points <- map(.x = convex_hull_points, .f = st_coordinates)

all_points_df <- do.call(rbind, all_points) %>%
  as.data.frame()

all_points_df %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_point()

