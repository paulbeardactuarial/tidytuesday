library(tidyverse)

tt_output <- tidytuesdayR::tt_load("2025-03-04")

data <- tt_output$longbeach



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
  filter(
    !is.na(dob) & animal_type %in% c("cat") & !(month(dob) == 2 & day(dob) == 29)
  ) |>
  mutate(
    dob_month = month(dob),
    dob_day = day(dob),
    dob_yday = yday(dob)
  ) |>
  count(dob_yday)


hollow_middle_y <- 50

cat_birthday_data |>
  ggplot() +
  geom_rect(data = month_day_map, aes(xmin = start_yday, xmax = end_yday, ymin = -Inf, ymax = Inf, fill = factor(month %% 2), alpha = 0.2)) +
  geom_text(data = month_day_map, aes(label = month_text, x = mid_yday, y = max(cat_birthday_data$n) + hollow_middle_y, angle = (1-mid_yday/365)*360)) +
  geom_col(aes(x = dob_yday, y = n + hollow_middle_y)) +
  geom_rect(aes(ymin = 0, ymax = hollow_middle_y, xmin = 0, xmax = max(dob_yday)), fill = "white") +
  coord_polar(start = 0) +
  theme_void() +
  theme(legend.position = "none")

  facet_wrap(vars(animal_type))
  count(dob_na, animal_type) |> 
  

  ggplot(aes(animal_type))