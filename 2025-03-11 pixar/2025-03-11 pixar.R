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
      cli::cli_abort(glue::glue("The following strings in `str_vector` returned multiple pattern matches: ", glue::glue_collapse(multi_matched_strings, sep = ", \n")))
    }
    
    extraced_patterns <- pattern_extracts |>
      list_transpose() |> 
      map(function(x) {x |> sort(na.last = TRUE) |> head(1)}) |> 
      unlist()
    
    return(extraced_patterns)
    
  }


# -------------- data construction -----------

franchise_levels <- c("Toy Story", "Cars", "Incredibles", "Monsters")
rating_vars <- c("rotten_tomatoes", "critics_choice", "metacritic")

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
        patterns = franchise_levels, 
        str_vector = film
        ) |> 
      factor(levels = franchise_levels)
  ) |> 
  filter(!is.na(franchise)) 
  


# -------------- create shiny app -----------

plot_data <- data |> 
  arrange(franchise, sequel_no) |> 
  mutate(x_dim = row_number() * 1.5 + as.numeric(franchise)) 
  
franchise_dim_data <-
  summarise(plot_data, xmin = min(x_dim), xmax = max(x_dim), xmid = xmin + 0.5 * (xmax - xmin), .by = franchise) 


plot_pixar <- function(plot_data, y_var = "rotten_tomatoes") {

plot_data |> 
  ggplot() + 
  geom_point(aes(x = x_dim, y = !!rlang::sym(y_var), color = sequel_no), size = 10) +
  geom_rect(
    data = franchise_dim_data,
    aes(
      xmin = xmin - 1,
      xmax = xmax + 1,
      ymin = -Inf,
      ymax = Inf,
      fill = franchise
    ),
    alpha = 0.2
  ) +
  scale_x_continuous(
    breaks = franchise_dim_data$xmid,
    labels = franchise_levels,
    name = ""
      ) +
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),  
    axis.line.x = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank()
  )

}



ui <- fluidPage(
  titlePanel("Critic Ratings of Pixar Films"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("y_var", "Select Y Variable:", 
                  choices = rating_vars, 
                  selected = "rotten_tomatoes") # Default choice
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$text <- renderText(input$y_var)
  
  output$plot <- renderPlot({

    plot_pixar(plot_data, input$y_var)

  })
}

shinyApp(ui = ui, server = server)
