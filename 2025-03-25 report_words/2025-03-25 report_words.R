
library(tidyverse)
library(treemapify)
library(waffle)

word_emoji_map <- readRDS(glue::glue("./2025-03-25 report_words/Data/word_emoji_map.rds"))

emoji_count <-
  tt_output$report_words_clean |> 
  count(word, sort = TRUE) |> 
  left_join(word_emoji_map) |> 
  arrange(desc(n)) |> 
  count(emoji, wt = n, sort = TRUE) |> 
  filter(!is.na(emoji)) |> 
  mutate(dummy = rev(emoji))


ggplot(data = emoji_count) +
  geom_treemap(aes(area = n)) +
  geom_treemap_text(aes(area = n, label = ggtext::element_markdown(emoji)))

