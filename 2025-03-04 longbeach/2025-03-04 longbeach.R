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

sysfonts::font_add_google("Bangers")
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
bkg_col <- "cornsilk"
plot_font <- "Bangers"

cat_image_df <- data.frame(
  x = 0,
  y = 0,
  img = "./2025-03-04 longbeach/grumpy_bday_cat.png"
)

cat_birthday_data |>
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
    )
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
  coord_polar(start = 0) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bkg_col, colour = bkg_col),
    panel.background = element_rect(fill = bkg_col, colour = bkg_col)
    )

  facet_wrap(vars(animal_type))
  count(dob_na, animal_type) |> 
  

  ggplot(aes(animal_type))