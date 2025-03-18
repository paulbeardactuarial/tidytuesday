library(tidyverse)
library(ggtext)
library(ggfx)
library(colorspace)

tt_output <- tidytuesdayR::tt_load("2025-03-18")
palmtrees <- tt_output$palmtrees

# read in the code that is required to write the function `geom_palm_leaf()`
source("./2025-03-18 palmtrees/palm_tree_leaf_functions.R")

# font spec
plot_font <- "Baloo 2"
sysfonts::font_add_google(plot_font)
showtext::showtext_auto()

# filter the data for plot
plot_data <-
  palmtrees |>
  filter(acc_genus == "Eremospatha") |>
  mutate(
    spec_name = str_remove(spec_name, "Eremospatha") |> tools::toTitleCase(),
    spec_name = fct_reorder(.f = spec_name, .x = max_stem_height_m) |> fct_rev()
  )

# plot it
plot_data |>
  ggplot() +
  aes(x = as.numeric(spec_name) * 10, y = max_stem_height_m) |>
  geom_col(width = 1, fill = "#8B5E3C") |>
  # the custom function `geom_palm_leaf()` is employed at the tip of each bar, to look like leaves are added to a trunk.
  # function can only do one point at a time, so perform iteratively with reduce2()
  reduce2(
    .x = as.numeric(plot_data$spec_name) * 10,
    .y = plot_data$max_stem_height_m,
    .f = function(plot, x, y) {
      list(plot, geom_palm_leaf(x = x, y = y, fill = "forestgreen"))
    },
    .init = _
  ) +
  theme_void() +
  ggfx::with_blur(
    annotate(
      geom = "point",
      x = 90,
      y = 120,
      color = "yellow",
      size = 20
    ),
    sigma = 20
  ) +
  scale_y_continuous(name = "Max. Stem Height (m)") +
  scale_x_continuous(name = "Species", breaks = 1:nrow(plot_data) * 10, labels = levels(plot_data$spec_name)) +
  labs(
    title = "Palm Tree Heights",
    subtitle = "The maximum height of every species of palm tree within the genus **Eremospatha**",
    caption =
      "Data Source: {palmtrees}
      Creator: Paul Beard"
  ) +
  theme(
    plot.margin = margin(c(5, 5, 5, 5)),
    plot.title = element_text(family = plot_font, size = 32, face = "bold"),
    plot.subtitle = element_textbox(family = plot_font, size = 16, width = unit(0.8, "npc"), margin = margin(c(7, 0, 7, 0))),
    legend.position = "none",
    axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))),
    panel.grid = element_blank(),
    axis.text.x = element_text(family = plot_font, size = 18, angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_text(family = plot_font, size = 18, face = "bold"),
    axis.text.y = element_text(family = plot_font, size = 18, margin = margin(r = 10)),
    axis.title.y = element_text(family = plot_font, size = 18, face = "bold", angle = 90, margin = margin(r = 5)),
    plot.background = element_rect(fill = "#D1C28B"),
    plot.caption = element_text(
      family = plot_font,
      size = 12,
      hjust = 1,
      margin = margin(c(5, 5, 5, 5))
    )
  )


ggsave(
  filename = "2025-03-18 palmtrees/2025-03-18 palmtrees.png",
  height = 6.04,
  width = 5.62,
  units = "in",
  dpi = 150
)
