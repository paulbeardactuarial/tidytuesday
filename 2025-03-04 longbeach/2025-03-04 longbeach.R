library(tidyverse)

tt_output <- tidytuesdayR::tt_load("2025-03-04")

data <- tt_output$longbeach


yday_remove_feb_29 <- function(date) {
    case_when(
      leap_year(date) & yday(date) > 60 ~ yday(date) - 1,
      leap_year(date) & yday(date) == 60 ~ NA, 
      .default = yday(date)
    )
}


# Load fonts --------------------------------------------------------------

sysfonts::font_add_google("Chewy")
showtext_auto()
showtext_opts(dpi = 300)



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



cat_birthday_data <-
  data |>
  mutate(dob_na = is.na(dob)) |>
  mutate(dob_yday = yday_remove_feb_29(dob)) |>
  filter(!is.na(dob_yday) & animal_type %in% c("cat")) |>
  count(dob_yday)


hollow_middle_y <- 50
bkg_col <- "#fdf5e2"
month_col <- c("#829891", "#FFAC1C")
plot_font <- "Chewy"

cat_image_df <- data.frame(
  x = 0,
  y = 0,
  img = "./2025-03-04 longbeach/grumpy_bday_cat.png"
)

cat_birthday_data |>
  #filter(dob_yday %in% month_day_map$yday) |> 
  ggplot() +
  geom_rect(
    data = month_day_map,
    aes(
      xmin = start_yday,
      xmax = end_yday,
      ymin = -Inf,
      ymax = Inf,
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
      y = max(cat_birthday_data$n) + hollow_middle_y,
      angle = (1 - mid_yday / max(cat_birthday_data$dob_yday)) * 360,
      vjust = 1.5
    ),
    family = plot_font,
    color = bkg_col,
    size = 2
  ) +
  geom_col(
    aes(
      x = dob_yday,
      y = n + hollow_middle_y
    ),
    width = 1,
    just = 1,
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
  ggimage::geom_image(data = cat_image_df, aes(image = img, x = x, y = y), size = 0.25) +
  labs(
    title = "Happy Birthday Mr. Tibbles",
    #subtitle = str_wrap("Distribution of birthdays for the cats taken in by the City of Long Beach Animal Care Services", width = 50)
  ) +
  coord_polar(
    start = 0
    ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bkg_col, colour = bkg_col),
    panel.background = element_rect(fill = bkg_col, colour = bkg_col),
    plot.title = element_text(
      family = plot_font,
      size = 8,
      margin = margin(t = 10, l = 3, unit = "pt"),
      color = "grey30"
    ),
    plot.subtitle = element_text(
      family = plot_font,
      size = 5,
      vjust = -1,
      margin = margin(t = 6, l = 3, unit = "pt"),
      color = "grey30"
    )
    )
