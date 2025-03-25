
library(tidyverse)
library(ggiraph)
library(ggrepel)

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


# font spec
plot_font <- "Noto Color Emoji"
sysfonts::font_add_google(plot_font)
showtext::showtext_auto()


# create ggplot object
p <-
ggplot(data = emoji_count |> head(30) |> arrange(proportion) |> mutate(words = stringr::str_replace_all(words, ", ", "<br>"))) +
  aes(x = 0,
      y = 0, 
      label = emoji, 
      size = proportion,
      data_id = emoji,
      tooltip = glue::glue(
        "<b>emoji: {emoji}</b>",
        "<br>",
        "<b>proportion: {scales::percent(proportion, accuracy = 0.01)}</b>",
        "<br>",
        "<br>",
        "{words}"
        )
      ) +
  geom_text_repel_interactive(
    max.overlaps = Inf,
    seed = 1,
    box.padding = 1,
    segment.color = NA
  ) +
  coord_cartesian(
    xlim = c(-0.5, 0.5),
    ylim = c(-0.5, 0.5)
  ) +
  theme_void() +
  scale_size(range = c(3, 20)) +
  labs(
    title = "Emoji Cloud of Amazon Annual Reports",
    subtitle = "vibes",
    caption =
      "Data Source: Amazon Annual Reports
      Creator: Paul Beard"
  ) +
  theme(
    legend.position = "none",
    plot.title =  element_text(family = "Arial", size = 32, face = "bold")
  )

# make ggplot interactive 
girafe(ggobj  = p, 
                options = 
                  list(
                    width_svg = 6, height_svg = 4,
                    opts_sizing(rescale = FALSE),
                    opts_tooltip(css = "background-color:#FF9900; color:black; padding: 10px; border-radius: 10px;"),
                    opts_hover(css = "fill:black; opacity: 1; font-weight: bold; font-size: 24", nearest_distance = 200)
                    )
                )


  