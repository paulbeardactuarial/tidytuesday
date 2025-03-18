
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



# ========== geom_palm_leaf() function creates a list. The list describes a complete palm leaves polygon at the site of x, y ==========

# this function is called at the top of each geom_col() to make it look like a palm tree

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


