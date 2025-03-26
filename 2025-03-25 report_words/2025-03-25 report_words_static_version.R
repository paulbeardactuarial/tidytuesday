
library(tidyverse)
library(ggiraph)
library(ggrepel)
library(extrafont)
library(ggtext) 

# load in data
# note `word_emoji_map` was constructed in `word_emoji_map_create.R` using API connection to LLM
tt_output <- tidytuesdayR::tt_load("2025-03-25")
word_emoji_map <- readRDS(glue::glue("./2025-03-25 report_words/Data/word_emoji_map.rds"))

# colors
amazon_colors <- c("#FF9900", "#146eb4", "#232F3E", "#232F3E") 
text_color <- "#131A22"

# create data object
emoji_count <-
  tt_output$report_words_clean |> 
  left_join(word_emoji_map) |> 
  count(emoji, sort = TRUE)  |> 
  left_join(
    word_emoji_map |>  summarise(words = paste(word, collapse = ", "), .by = emoji)
  ) |> 
  mutate(
    proportion = n/sum(n)
  ) |> 
  filter(!is.na(emoji))


# add points
points <- 40  # Number of points
cloud_range <- 2.25

plot_data <- 
  emoji_count |>
  arrange(desc(proportion)) |>
  head(points) |>
  mutate(color = sample(amazon_colors, points, replace = T))

# create ggplot object
plot_data |> 
  ggplot() +
  geom_textbox(
    inherit.aes = FALSE,
    data = data.frame(),
    mapping = aes(
      x = -2,
      y = 1,
      label = glue::glue(
        "'Word Cloud' showing the relative frequency of themes in Amazon Annual Reports 2005 to 2023",
        "<br>",
        "<br>",
        "Instead of displaying words, the cloud consolidates words into themes. These were assigned by ChatGPT under the prompt <span style='font-style:italic;color:#FF9900'>*\"...the emoji that best corresponds to the word\"*</span>")
    ),
    fill = NA,
    color = text_color,
    box.colour = NA,
    vjust = 1,
    hjust = 0,
    size = 3.5,
    box.padding = unit(c(0, 0, 0, 0), "pt"),
    width = unit(0.85, "npc")
  ) +
  aes(
    x = 0,
    y = -3,
    label = emoji,
    size = proportion,
    color = color
  ) +
  scale_color_manual(values = amazon_colors |> setNames(amazon_colors)) +
  ggwordcloud::geom_text_wordcloud(
      xlim = c(-0.6, 0.6)*cloud_range,
      ylim = -4.5 + c(-1.4, 1.4)*cloud_range,
      shape = "circle",
      family = "Segoe UI Black"
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
    plot.title = element_text(size = 18, face = "bold", color = text_color, margin = margin(l = 20, r = 10)),
    plot.caption = element_text(size = 7, face = "plain", color = text_color, margin = margin(t = 10, b = 20, r = 5)),
    plot.margin = margin(10,2,2,2)
  ) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-6.75, 1)) 


# TO SAVE export the plot manually... 

# this is a necessity because emojis seem to change font (html variant?) when saving with ggsave() and not sure how to fix
