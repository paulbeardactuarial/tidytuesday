
library(tidyverse)
library(ggtext)

tt_output <- tidytuesdayR::tt_load("2025-03-04")

data <- tt_output$longbeach

# =============== Helper functions ===============

yday_remove_feb_29 <- function(date) {
    case_when(
      leap_year(date) & yday(date) > 60 ~ yday(date) - 1,
      leap_year(date) & yday(date) == 60 ~ NA, 
      .default = yday(date)
    )
}

date_to_dd_mm <- function(date) {
  as.Date(date, origin = "2025-01-01") |> format("%d-%b")
}


# =============== Fonts ===============

sysfonts::font_add_google("Chewy")
showtext_auto()
showtext_opts(dpi = 300)


# =============== Data for month boundaries ===============

month_day_map <-
  tibble::tibble(
    date = seq.Date(as.Date("2025-01-01"), as.Date("2025-12-31"), by = 1)
  ) |>
  mutate(
    yday = yday(date),
    month = month(date)
  ) |>
  mutate(
    start_yday = min(yday) - 1,
    end_yday = max(yday) ,
    month_text = month(date, label = TRUE),
    .by = month
  ) |>
  mutate(
    mid_yday = (start_yday + end_yday) / 2
  ) |>
  slice_head(by = month)


# =============== Data for cat birthdays ===============

cat_birthday_data <-
  data |>
  mutate(dob_na = is.na(dob)) |>
  mutate(dob_yday = yday_remove_feb_29(dob)) |>
  filter(!is.na(dob_yday) & animal_type %in% c("cat")) |>
  count(dob_yday)



# =============== Plot params ===============

hollow_middle_y <- 50
margin_y <- 10
bkg_col <- "#fdf5e2"
month_col <- c("#829891", "#FFAC1C")
plot_font <- "Chewy"

cat_image_df <- data.frame(
  x = 0,
  y = 0,
  img = "./2025-03-04 longbeach/grumpy_bday_cat.png"
)

most_freq_yday <- cat_birthday_data |> slice_max(n) 
least_freq_yday <- cat_birthday_data |> slice_min(n)


# =============== Create plot! ===============

cat_birthday_data |>
  ggplot() +
  geom_rect(
    data = month_day_map,
    aes(
      xmin = start_yday,
      xmax = end_yday,
      ymin = -Inf,
      ymax = most_freq_yday$n + hollow_middle_y,
      fill = factor(month %% 2),
      alpha = 0.5
    )
  ) +
  scale_fill_manual(values = month_col) +
  geom_text(
    data = month_day_map,
    aes(
      label = month_text,
      x = mid_yday,
      y = most_freq_yday$n + hollow_middle_y,
      angle = (1 - mid_yday / max(cat_birthday_data$dob_yday)) * 360,
      vjust = 1.5
    ),
    family = plot_font,
    color = bkg_col,
    size = 2.5
  ) +
  geom_col(
    aes(
      x = dob_yday,
      y = n + hollow_middle_y
    ),
    width = 1,
    just = 1,
    fill = "grey30",
    color = "grey30"
  ) +
  annotate(
    geom = "rect",
    ymin = 0,
    ymax = hollow_middle_y,
    xmin = 0,
    xmax = max(cat_birthday_data$dob_yday),
    fill = bkg_col
  ) +
  ggimage::geom_image(data = cat_image_df, aes(image = img, x = x, y = y), size = 0.22) +
  labs(
    title = "Cat Birthday Frequencies",
    subtitle = glue::glue(
      "The frequency of birthdays attributed to the cats taken
      in by the City of Long Beach Animal Care Services"),
    caption = glue::glue(
      "Data Source: City of Long Beach Animal Care Services
      Creator: Paul Beard"
    )
  ) +
  coord_polar() +
  geom_hline(yintercept = mean(cat_birthday_data$n) + hollow_middle_y, color = bkg_col, linetype = "dashed") +
  annotate(geom = "text", x = 0, y = mean(cat_birthday_data$n) + hollow_middle_y, label = "mean frequency", size = 1.2, color = bkg_col, vjust = -0.5) +
  annotate(
    geom = "segment",
    x = most_freq_yday$dob_yday + 42,
    y = most_freq_yday$n + hollow_middle_y + 25,
    xend = most_freq_yday$dob_yday,
    yend = most_freq_yday$n + hollow_middle_y,
    arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
    color = "#FFAC1C", 
    linewidth = 0.8
  ) +
  annotate(
    geom = "label",
    x = most_freq_yday$dob_yday + 42,
    y = most_freq_yday$n + hollow_middle_y + 25,
    label = str_wrap(glue::glue("{date_to_dd_mm(most_freq_yday$dob_yday)} was the most common date of birth, shared by {most_freq_yday$n} cats"), width = 25),
    size = 1.8,
    color = "#FFAC1C",
    vjust = 1.1,
    alpha = 0,
    family = plot_font,
    label.size = 0
  ) +
  annotate(
    geom = "segment",
    x = least_freq_yday$dob_yday - 8,
    y = most_freq_yday$n + hollow_middle_y + 25,
    xend = least_freq_yday$dob_yday,
    yend = least_freq_yday$n + hollow_middle_y,
    arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
    color = "#FFAC1C", 
    linewidth = 0.8
  ) +
  annotate(
    geom = "label",
    x = least_freq_yday$dob_yday - 8,
    y = most_freq_yday$n + hollow_middle_y + 25,
    label = str_wrap(glue::glue("{date_to_dd_mm(least_freq_yday$dob_yday)} was the least common date of birth, shared by {least_freq_yday$n} cats"), width = 25),
    size = 1.8,
    color = "#FFAC1C",
    vjust = -0.1,
    alpha = 0,
    family = plot_font,
    label.size = 0
  )   +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bkg_col, colour = bkg_col),
    panel.background = element_rect(fill = bkg_col, colour = bkg_col),
    plot.title = element_text(
      family = plot_font,
      size = 14,
      margin = margin(t = 5, l = 3, unit = "pt"),
      color = "grey30"
    ),
    plot.subtitle = element_text(
      family = plot_font,
      size = 8,
      vjust = -1,
      margin = margin(t = 0, l = 3, unit = "pt"),
      color = "grey30"
    ),
    plot.caption = element_text(
      family = plot_font,
      size = 3,
      margin = margin(t = 5, l = 3, unit = "pt"),
      color = "grey30",
      hjust = 0
    )
  ) 

# =============== Save ===============


ggsave(
  filename = "2025-03-04 longbeach/2025-03-04 longbeach.png",
  height = 4.75,
  width = 3.94,
  bg = bg_col,
  units = "in",
  dpi = 200
)




