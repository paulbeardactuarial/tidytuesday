
library(tidyverse)
library(ggiraph)
library(ggrepel)
library(extrafont)
library(ggtext) 

# Check available fonts
fonts()

# load in data
# note `word_emoji_map` was constructed in `word_emoji_map_create.R` using API connection to LLM
tt_output <- tidytuesdayR::tt_load("2025-03-25")
word_emoji_map <- readRDS(glue::glue("./2025-03-25 report_words/Data/word_emoji_map.rds"))

# colors
amazon_colors <- c("#FF9900", "#131A22", "#232F3E")

# create data object
emoji_count <-
  tt_output$report_words_clean |> 
  inner_join(word_emoji_map) |> 
  count(emoji, sort = TRUE)  |> 
  left_join(
    word_emoji_map |>  summarise(words = paste(word, collapse = ", "), .by = emoji)
  ) |> 
  mutate(
    proportion = n/sum(n)
  )


# # font spec
plot_font <- "Roboto"
sysfonts::font_add_google(plot_font)
showtext::showtext_auto()


# create ggplot object
p <-
  emoji_count |>
  head(25) |>
  arrange(proportion) |>
  mutate(words = stringr::str_replace_all(words, ", ", "<br>")) |>
  ggplot() +
  geom_textbox(
    inherit.aes = FALSE,
    data = data.frame(),
    mapping = aes(
      x = -Inf,
      y = 1,
      label = glue::glue(
        "'Word Cloud' showing the relative frequency of themes in Amazon Annual Reports 2005 - 2023",
        "<br>",
        "<br>",
        "Instead of displaying words, the cloud consolidates words into themes. These were assigned as <span style='font-style:italic;color:#FF9900'>*\"the emoji that best corresponds to the word\"*</span> according to ChatGPT")
    ),
    fill = NA,
    color = "#232F3E",
    box.colour = NA,
    vjust = 1,
    hjust = 0,
    size = 4,
    box.padding = unit(c(0, 0, 0, 0), "pt"),
    width = unit(1, "npc"),
    family = plot_font
  ) +
  aes(
    x = 0,
    y = 0,
    label = emoji,
    size = proportion,
    data_id = emoji,
    tooltip = glue::glue(
     # "<b>emoji: {emoji}</b>",
      "<span style='font-weight:bold;font-size:32px'>{emoji}</span>",
      "<br>",
      "<b>proportion: {scales::percent(proportion, accuracy = 0.01)}</b>",
      "<br>",
      "<br>",
      "{words}"
    )
  ) +
  geom_text_repel_interactive(
    max.overlaps = Inf,
    seed = 2,
    box.padding = 1,
    segment.color = NA,
    family = "Arial",
    nudge_x = 0,
    nudge_y = 0,
    xlim = c(-1, 1),
    ylim = c(-3.5, -0.5),
  ) +
  theme_void() +
  scale_size(range = c(3, 20)) +
  labs(
    title = "Amazon Cloud",
    caption =
      "Data Source: Amazon Annual Reports
      Creator: Paul Beard"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 24, face = "bold", color = "#131A22", family = plot_font),
    plot.subtitle = element_markdown(size = 14, face = "plain", family = plot_font, align_widths = unit(0.9, "npc"), color = "#131A22"),
    plot.caption = element_text(size = 8, face = "plain", color = "#131A22", family = plot_font, margin = margin(t = 10, b = 20))
  ) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-3.5, 1))


# make ggplot interactive 
girafe(ggobj  = p, 
                options = 
                  list(
                    width_svg = 7, height_svg = 6,
                    opts_sizing(rescale = FALSE),
                    opts_tooltip(css = "background-color:#FF9900; color:black; padding: 10px; border-radius: 10px;"),
                    opts_hover(css = "opacity:0.5")
                    )
                )
