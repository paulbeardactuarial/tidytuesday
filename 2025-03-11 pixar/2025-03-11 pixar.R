library(tidyverse)
library(ggtext)

tt_output <- tidytuesdayR::tt_load("2025-03-11")

pixar_films <- tt_output$pixar_films
public_response <- tt_output$public_response


sysfonts::font_add_google("Quattrocento")


# -------------- custom function for extracting multiple patterns over a vector -----------


str_extract_patterns <-
  function(str_vector, patterns) {
    
    pattern_extracts <- map(patterns, function(x) str_extract(str_vector, x))
    
    matches_per_row <- pattern_extracts |>
      map(function(x) !is.na(x)) |>
      bind_cols(.name_repair = "unique_quiet") |>
      mutate(across(everything(), as.numeric)) |>
      rowSums()
    
    multi_matched_strings <- str_vector[matches_per_row > 1]
    if (length(multi_matched_strings) > 0) {
      cli::cli_abort(glue::glue("The following strings in `str_vector` returned multiple matches: ", glue::glue_collapse(multi_matched_strings, sep = ", \n")))
    }
    
    extraced_patterns <- pattern_extracts |>
      list_transpose() |> 
      map(function(x) {x |> sort(na.last = TRUE) |> head(1)}) |> 
      unlist()
    
    return(extraced_patterns)
    
  }


# -------------- data construction -----------

data <- inner_join(pixar_films, public_response, by = "film") |> 
  mutate(
    sequel_no = 
      case_when(
        stringr::str_sub(film, start = -1, -1) == 2 ~ 2,
        stringr::str_sub(film, start = -1, -1) == 3 ~ 3,
        stringr::str_sub(film, start = -1, -1) == 4 ~ 4,
        film == "Monsters University" ~ 2,
        !is.na(film) ~ 1,
        .default = NA
      )
  ) |>  
  mutate(
    franchise = 
      str_extract_patterns(
        patterns = c("Toy Story", "Cars", "Incredibles", "Monsters"), 
        str_vector = film
        )
  ) |> 
  filter(!is.na(franchise)) 
  


# -------------- create shiny app -----------

data |> 
  ggplot(aes(x = franchise, y = rotten_tomatoes, color = sequel_no)) + 
  geom_point(aes(h_just = 1), size = 15)








data |> 
  ggplot(aes(x = rotten_tomatoes)) + 
  geom_histogram(bins = 5)

