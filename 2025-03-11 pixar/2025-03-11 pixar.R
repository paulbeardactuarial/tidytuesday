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
  filter(!is.na(franchise)) 
  
min_ratings <- summarise(data, across(all_of(rating_vars), \(x) min(x, na.rm = T)))

# -------------- create plotting function -----------

plot_data <- data |> 
  arrange(franchise, desc(sequel_no)) |> 
  mutate(x_dim = row_number() * 0.75 + as.numeric(franchise)) 
  
franchise_dim_data <-
  summarise(plot_data, xmin = min(x_dim), xmax = max(x_dim), xmid = xmin + 0.5 * (xmax - xmin), .by = franchise) 

plot_font <- "Quattrocento"

y_var <- "metacritic"

plot_data |>
  filter(!is.na(get(y_var))) |>
  ggplot() +

  # geom_rect(
  #   data = franchise_dim_data, aes(
  #     xmin = xmin - 1,
  #     xmax = xmax + 1,
  #     ymin = -Inf,
  #     ymax = Inf
  #   ),
  #   fill = "#a6acb8"
  # ) +
  
  with_outer_glow(
    geom_point(
      data = plot_data |> filter(sequel_no == 1),
      aes(x = x_dim, y = get(y_var)),
      color = "#f4ef98",
      size = 10
    ),
    sigma = 5
  ) +
  with_outer_glow(
    geom_point(
      aes(x = x_dim, y = get(y_var)),
      color = "#f4ef98",
      size = 3
    ),
    sigma = 3,
    expand = 0
  ) +
  with_outer_glow(
    geom_point(
      aes(x = x_dim, y = get(y_var), alpha = if_else(sequel_no == 1, 0, 1)),
      color = "white",
      size = 10
    ),
    sigma = 2,
    expand = 0
  ) +
  geom_text(
    aes(x = x_dim, y = get(y_var) + 4, label = glue::glue("{film} ({lubridate::year(release_date)})")),
    color = "black",
    alpha = 0.5,
    size = 4,
    hjust = 0,
    family = plot_font,
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = franchise_dim_data$xmid,
    labels = franchise_levels,
    name = ""
  ) +
  scale_y_continuous(
    name = glue::glue("{y_var} score (%)") |> str_replace_all("_", " ") |> tools::toTitleCase(),
    breaks = (5 + c(0:5)) * 10,
    lim = c(50, 120)
  ) +
  coord_flip() +
  labs(
    title = "Pixar Film Ratings and their Sequels",
    subtitle = glue::glue(str_wrap(st_df$st_col), 10),
    caption = 
      "Data Source: {pixarfilms}
      Creator: Paul Beard"
    
  )  +
  # geom_richtext(
  #   data = st_df,
  #   aes(x = x, y = y, label = st_col),
  #   hjust = 0,
  #   fill = NA, label.color = NA
  # ) +
  theme(
    plot.title = element_text(family = plot_font, size = 24, face = "bold"),
    plot.subtitle = ggtext::element_markdown(
      family = plot_font, size = 16, face = "bold"
      ),
    legend.position = "none",
    axis.line.x = element_line(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(family = plot_font, size = 14, face = "bold"),
    axis.title.x = element_text(family = plot_font, size = 14, face = "bold"),
    axis.text.y = element_text(family = plot_font, size = 14, face = "bold", hjust = 1),
    axis.ticks.y = element_blank(),
    plot.caption = element_text(
      family = plot_font,
      size = 10
    ),
    # plot.background = element_rect(fill = grid::linearGradient(c("#c8cce7", "#558fd1")),
    #                                colour = "#efeae6", linewidth = 10) +
    #   annotation_custom(
    #     grob = grid::rasterGrob(
    #       grid::linearGradient(
    #         c("#c8cce7", "#558fd1")
    #       )
    #     ), 
    #     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    #   )
    
    plot.background = element_rect(
      fill = "#a6acb8"
      #fill = grid::linearGradient(c("blue","#a6acb8"))
      )
  ) +
  with_blur(
  geom_vline(
    data = franchise_dim_data,
    aes(
      xintercept = xmax + 1.75 / 2,
    ), color = "white"
  ),
  sigma = 2.5
  )

ggsave(
  filename = "2025-03-11 pixar/2025-03-11 pixar.png",
  height = 6.04,
  width = 5.62,
  bg = "white",
  units = "in",
  dpi = 150
)


ggsave(
  filename = "2025-03-11 pixar/2025-03-11 pixar.png",
  dpi = 400, width = 6.8, height = 10, bg = "#ffffff"
)

st_df <- 
  data.frame(
    x = 23, y = 40,
  st_col = "
  Pixar have never had a <span style='color: white'>sequel</span> rated better than the <span style='color: yellow;'>original</span> in the franchise ",
  st_black = 
  "<span style='color: white;'>Pixar have never had a </span><span style='color: black;'>sequel</span><span style='color: white;'> rated better than the </span><span style='color: black;'>original</span><span style='color: white;'> in the franchise</span>",
  st_yellow = 
    "<span style='opacity:0;'>Pixar have never had a </span><span style='color: yellow;'>sequel</span><span style='opacity:0;'> rated better than the </span><span style='color: yellow;'>original</span><span style='opacity:0;'> in the franchise</span>"
  )

st <- glue::glue("Pixar have never had a sequel rated better than the original in the franchise")
