library(tidyverse)
library(ggtext)
library(ggfx)

tt_output <- tidytuesdayR::tt_load("2025-03-11")

pixar_films <- tt_output$pixar_films
public_response <- tt_output$public_response


sysfonts::font_add_google("Quattrocento")
showtext::showtext_auto()

# -------------- custom function for extracting multiple patterns over a vector -----------


str_extract_patterns <-
  function(str_vector, patterns) {
    
    pattern_extracts <- map(patterns, function(x) str_extract(str_vector, x))
    
    matches_per_row <- pattern_extracts |>
      map(function(x) !is.na(x)) |>
      bind_cols(.name_repair = "unique_quiet") |>
      mutate(across(everything(), as.numeric)) |>
      rowSums()
    
    multi_matched_strings <- str_vector[matches_per_row > 1]
    if (length(multi_matched_strings) > 0) {
      cli::cli_abort(glue::glue("The following strings in `str_vector` returned multiple pattern matches: ", glue::glue_collapse(multi_matched_strings, sep = ", \n")))
    }
    
    extraced_patterns <- pattern_extracts |>
      list_transpose() |> 
      map(function(x) {x |> sort(na.last = TRUE) |> head(1)}) |> 
      unlist()
    
    return(extraced_patterns)
    
  }


# -------------- data construction -----------

franchise_levels <- c("Cars", "Monsters",  "Incredibles", "Toy Story")
rating_vars <- c("rotten_tomatoes", "critics_choice", "metacritic")

data <- inner_join(pixar_films, public_response, by = "film") |> 
  mutate(
    sequel_no = 
      case_when(
        stringr::str_sub(film, start = -1, -1) == 2 ~ 2,
        stringr::str_sub(film, start = -1, -1) == 3 ~ 3,
        stringr::str_sub(film, start = -1, -1) == 4 ~ 4,
        film == "Monsters University" ~ 2,
        !is.na(film) ~ 1,
        .default = NA
      )
  ) |>  
  mutate(
    franchise = 
      str_extract_patterns(
        patterns = franchise_levels, 
        str_vector = film
        ) |> 
      factor(levels = franchise_levels)
  ) |> 
  filter(!is.na(franchise)) |> 
  arrange(franchise, desc(sequel_no)) |> 
  mutate(x_dim = row_number() * 1.5 + as.numeric(franchise)) 
  
min_ratings <- summarise(data, across(all_of(rating_vars), \(x) min(x, na.rm = T)))

# -------------- create plotting function -----------

y_var <- "metacritic" # decided to use metacritic, though can use any of the ratings available

plot_font <- "Quattrocento"
bck_col <- "#A6D5FB"
og_col <- "#F4EB8D"
seq_col <- "#FCE8E1"

# subtitle 
st <- "Pixar consistently produce films that score well with critics, but they have never released a <span style='color: #EBD7D2; font-weight: bold'>sequel</span> that has rated better than the <span style='color: #E6DA71; font-weight: bold'>original</span> in the franchise"


plot_data <- data 
  

# data for rectangles to make franchises clearer
franchise_dim_data <-
  summarise(
    plot_data, 
    xmin = min(x_dim), 
    xmax = max(x_dim), 
    xmid = xmin + 0.5 * (xmax - xmin), 
    ymin = min(get(y_var)), 
    ymax = max(get(y_var)), 
    ymid = ymin + 0.5 * (ymax - ymin), 
    .by = franchise) 

# main plot...
plot_data |>
  filter(!is.na(get(y_var))) |>
  ggplot() +
  # rectangles
  geom_rect(
      data = franchise_dim_data, aes(
        xmin = xmin - 1,
        xmax = xmax + 1,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = bck_col
    ) +
  # film points and film names
  with_outer_glow(
    geom_point(
      data = plot_data |> filter(sequel_no == 1),
      aes(x = x_dim, y = get(y_var)),
      color = og_col,
      size = 10
    ),
    sigma = 5
  ) +
  with_outer_glow(
    geom_point(
      aes(x = x_dim, y = get(y_var)),
      color = og_col,
      size = 3
    ),
    sigma = 3,
    expand = 0
  ) +
  with_outer_glow(
    geom_point(
      aes(x = x_dim, y = get(y_var), alpha = if_else(sequel_no == 1, 0, 1)),
      color = seq_col,
      size = 10
    ),
    sigma = 2,
    expand = 0
  ) +
  geom_text(
    aes(x = x_dim, y = get(y_var) + 4, label = glue::glue("{film} ({lubridate::year(release_date)})")),
    color = "black",
    alpha = 0.5,
    size = 5,
    hjust = 0,
    family = plot_font,
    fontface = "bold"
  ) +
  # scales
  scale_x_continuous(
    breaks = franchise_dim_data$xmid,
    labels = franchise_levels,
    name = ""
  ) +
  scale_y_continuous(
    name = glue::glue("{y_var} score (%)") |> str_replace_all("_", " ") |> tools::toTitleCase(),
    breaks = (5 + c(0:5)) * 10,
    lim = c(35, 110)
  ) +
  # flip it!
  coord_flip() +
  # subtitle and axis text
  geom_text(
    data = franchise_dim_data,
    aes(
      x = xmid,
      y = 35,
      label = franchise
    ),
    color = "white",
    family = plot_font, size = 10, hjust = 0, fontface = "bold", 
  ) +
  geom_textbox(
    data = data.frame(),
    aes(
      x = max(franchise_dim_data$xmax) + 3,  
      y = -Inf,    
      label = st
    ),
    fontface = "bold",
    width = unit(1, "npc"),
    hjust = 0.01,
    halign = 0,
    box.color = NA,
    family = plot_font,
    size = 6.2, 
    lineheight = 0.85
  )  +
  # final pieces... labs and theme
  labs(
    title = "Pixar Film Ratings and their Sequels",
    caption = 
      "Data Source: {pixarfilms}
      Creator: Paul Beard"
    
  )  +
  theme(
    plot.title = element_text(family = plot_font, size = 32, face = "bold"),
    legend.position = "none",
    axis.line.x = element_line(),
    panel.grid = element_blank(),
    axis.text.x = element_text(family = plot_font, size = 18, face = "bold"),
    axis.title.x = element_text(family = plot_font, size = 18, face = "bold"),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.caption = element_text(
      family = plot_font,
      size = 12
    ) 
  ) 
ggsave(
  filename = "2025-03-11 pixar/2025-03-11 pixar.png",
  height = 6.04,
  width = 5.62,
  bg = "white",
  units = "in",
  dpi = 175
)


