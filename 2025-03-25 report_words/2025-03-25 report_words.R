
library(tidyverse)
library(ggiraph)
library(ggrepel)
library(extrafont)
library(ggtext) 

# Check/add available fonts
fonts()
systemfonts::register_font(
  name = "Roboto",
  plain = systemfonts::match_fonts("Roboto")
)

# set font
plot_font <- "Roboto"

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
      x = -2,
      y = 1,
      label = glue::glue(
        "'Word Cloud' showing the relative frequency of themes in Amazon Annual Reports 2005 - 2023",
        "<br>",
        "<br>",
        "Instead of displaying words, the cloud consolidates words into themes. These were assigned by ChatGPT under the prompt <span style='font-style:italic;color:#FF9900'>*\"...the emoji that best corresponds to the word\"*</span>")
    ),
    fill = NA,
    color = "#232F3E",
    box.colour = NA,
    vjust = 1,
    hjust = 0,
    size = 4.5,
    box.padding = unit(c(0, 0, 0, 0), "pt"),
    width = unit(0.95, "npc"),
    family = plot_font
  ) +
  aes(
    x = 0,
    y = 0,
    label = emoji,
    size = proportion,
    data_id = emoji,
    tooltip = glue::glue(
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
    seed = 9,
    box.padding = 1,
    segment.color = NA,
    family = plot_font,
    nudge_x = 0,
    nudge_y = 0,
    xlim = c(-2, 2),
    ylim = c(-5.5, -1.5),
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
    plot.title = element_text(size = 24, face = "bold", color = "#131A22", family = plot_font, margin = margin(l = 20, r = 10)),
    plot.caption = element_text(size = 8, face = "plain", color = "#131A22", family = plot_font, margin = margin(t = 10, b = 20))
  ) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-5.5, 1)) 


# make ggplot interactive 
girafe(ggobj  = p, 
                options = 
                  list(
                    width_svg = 4, height_svg = 6,
                    opts_sizing(rescale = FALSE),
                    opts_tooltip(css = "background-color:#FF9900; color:black; padding: 10px; border-radius: 10px;"),
                    opts_hover(css = "opacity:0.5")
                    )
                )

