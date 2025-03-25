
library(tidyverse)
library(ggiraph)

tt_output <- tidytuesdayR::tt_load("2025-03-25")
word_emoji_map <- readRDS(glue::glue("./2025-03-25 report_words/Data/word_emoji_map.rds"))

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

emoji_count |> View()
           


emoji_count_by_year <-
  tt_output$report_words_clean |> 
  count(word, year, sort = TRUE) |> 
  left_join(word_emoji_map) |> 
  arrange(desc(n)) |> 
  count(emoji, year, wt = n, sort = TRUE) |> 
  filter(!is.na(emoji)) 

ggplot(data = emoji_count |> head(20)) +
  aes(fill = n, label = emoji, area = n) +
  geom_treemap( layout = "squarified") +
  geom_treemap_text(fontface = "bold", color = "black", grow = F, reflow = T, place = "centre", layout = "squarified", size = 40) +
  theme(
    legend.position = "none"
  )

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
  ggiraph::geom_text_repel_interactive(
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
    plot.title =  element_text(family = plot_font, size = 32, face = "bold")
  )


girafe(ggobj  = p, 
                options = 
                  list(
                    width_svg = 6, height_svg = 4,
                    opts_sizing(rescale = FALSE),
                    opts_tooltip(css = "background-color:#FF9900; color:black; padding: 10px; border-radius: 10px;"),
                    opts_hover(css = "fill:black; opacity: 1; font-weight: bold; font-size: 24", nearest_distance = 200)
                    )
                )


  