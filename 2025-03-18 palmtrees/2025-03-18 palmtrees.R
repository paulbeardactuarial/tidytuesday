library(tidyverse)
library(ggtext)
library(ggfx)
library(ragg)
library(colorspace)

tt_output <- tidytuesdayR::tt_load("2025-03-18")


palmtrees <- tt_output$palmtrees

palmtrees |> glimpse()


palmtrees |> 
  summarise(
    range = range(max_stem_height_m),
    max = max(max_stem_height_m),
    count = n(),
    .by = acc_genus
  ) |> arrange(desc(range), count)
    sort = T) 

plot_data <-
palmtrees |>
  filter(acc_genus == "Eremospatha") |> 
  mutate(spec_name = fct_reorder(.f = spec_name, .x = max_stem_height_m) |> fct_rev()) 


plot_data |> 
  ggplot() +
  aes(x = as.numeric(spec_name) * 10, y = max_stem_height_m) |> 
  geom_col(width = 1, fill = "#8B5E3C") |> 
  reduce2(
    .x = as.numeric(plot_data$spec_name) * 10, 
    .y = plot_data$max_stem_height_m, 
    .f = function(plot, x, y) {
      list(plot, geom_palm_leaf(x = x, y = y, fill = "forestgreen"))
    },
    .init = _)
  
  


tt_output$palmtrees$max_stem_height_m





# ========== function to construct the a dataframe of x and y co-orinates for a single palm leaf shape ==========

palm_leaf_polygon_construct <- function(
  length_scale = 6,
  curve_ratio = 0.667,
  points = 200,
  bottom_left_corner = c(0, 0) # c(x, y)
) {
  

  theta <- seq(0, pi, length.out = points)  # Only half ellipse (curved side)
  x_curve <- length_scale * curve_ratio * cos(theta)  # Scale the x-coordinate to make it more oval
  y_curve <- sin(theta)     # y-coordinate remains the same
  
  # Define the flat part of the polygon
  x_flat <- c(-2, 2)  # Flat side of the shape (bottom part), scaled horizontally
  y_flat <- rep(0, 2)  # Flat y-coordinates
  
  # Combine the coordinates of the flat and curved sides
  x_coords <- c(x_flat, x_curve)
  y_coords <- c(y_flat, y_curve)
  
  # Create a data frame of the coordinates
  # we are cutting off the right-most part and replacing with triangle to get leaf effect
  points <- 
    data.frame(x = x_coords, y = y_coords) |> 
    filter(x <= 0) |> 
    add_row(x = length_scale * (1-curve_ratio), y = 0, .after = 1) |> 
    mutate(x = x - min(x) + bottom_left_corner[1],
           y = y - min(y) + bottom_left_corner[2])
  
  return(points)
}

# ========== function to change dataframe of x and y co-ordinates to rotate a polygon ==========

rotate_coords <- function(
    xy_coord, # dataframe of x and y values
    theta # rotation in degrees
    ) {
  xy_coord <- select(xy_coord, x, y)
  theta_rad <- theta * pi / 180 # Convert degrees to radians
  rotation_matrix <- matrix(c(cos(theta_rad), -sin(theta_rad), sin(theta_rad), cos(theta_rad)), nrow = 2)
  rotated_coords <- as.data.frame(t(rotation_matrix %*% t(xy_coord))) |> setNames(c("x", "y"))
  return(rotated_coords)
}



# ========== geom_palm_leaf() function places a palm leaves polygon at x, y site ==========

 geom_palm_leaf <-
   function(
    x = 0,
    y = 0,
    scale = 1,
    length_scale = 6,
    curve_ratio = 0.667,
    points = 200,
    ...
   ) {
     
points <- palm_leaf_polygon_construct(length_scale = length_scale, curve_ratio = curve_ratio, points = points) |> 
  mutate(
    x = .data$x * scale,
    y = .data$y * scale
  )

shift <- function(data, x, y) {data |> mutate(x = x + .env$x, y = y + .env$y)}

list(
  geom_polygon(data = points |>  rotate_coords(0) |> shift(x, y), aes(x, y), ...),
  geom_polygon(data = points |>  rotate_coords(45) |> shift(x, y), aes(x, y), ...),
  geom_polygon(data = points |>  rotate_coords(-45) |> shift(x, y), aes(x, y), ...),
  geom_polygon(data = points |>  rotate_coords(0) |> mutate(x = -x) |> shift(x, y), aes(x, y), ...),
  geom_polygon(data = points |>  rotate_coords(45) |> mutate(x = -x) |> shift(x, y), aes(x, y), ...),
  geom_polygon(data = points |>  rotate_coords(-45) |> mutate(x = -x) |> shift(x, y), aes(x, y), ...)
)
}

 
 ggplot() +
   geom_palm_leaf(fill = "forestgreen") +
   geom_palm_leaf(x = 50, y = 0, fill = "forestgreen") +
   coord_fixed()
   
