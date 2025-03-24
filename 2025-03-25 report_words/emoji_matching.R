# this script will create a dataframe called `word_emoji_map`, which contains columns:
# "word"
# "emoji"

# the words are the top 1000 from the tidytuesday data set
# the emojis are the emoji assigned to each word by ChatGPT

# this script will not work for you unless you have set up an API with ChatGPT and changed your environment variable accordingly

library(tidyverse)
library(ellmer)

tt_output <- tidytuesdayR::tt_load("2025-03-25")

word_vector <-
  tt_output$report_words_clean |>
  count(word, sort = TRUE) |>
  pull(word) |>
  head(1000)


initial_prompt <-
  glue::glue(
    "You are a classification LLM. You will receive a JSON file. The file will contain a list of words.
    It is important that you return only an edited version of the JSON file. Add 'emoji' to each item, which can only ever be a single emoji symbol and nothing else. It should be the emoji that best corresponds to the word. You may use the same emoji for different words.
    No explanations. Return only the data in a structured JSON format. Your final JSON code must begin with ``` and end with ```.
    "
  )

# function to convert vector of strings into line-by-line text of strings
glue_chr_vector <- function(vec) {
  glue::glue(paste0("\"", vec, collapse = "\"\n"), "\"")
}

# function to split vector into list of vectors (for chunking)
split_vector <- function(vec, max_length_per_vec) {
  split(vec, ceiling(seq_along(vec) / max_length_per_vec))
}

# function to convert character vector into JSON file
glue_to_json <- function(vec) {
  glue::glue('[\n{glue::glue_collapse(glue::glue(\'  {{ "word": "{vec}" }}\'), ",\n")}\n]')
}

# function for cleaning LLMs
json_list_to_df <- function(output_list) {
  output_list |>
    purrr::map(function(x) {
      has_ticks <- stringr::str_detect(x, "```")
      if (has_ticks) {
        x <- x |>
          stringr::str_extract_all("(?s)```(.*?)```") |>
          pluck(1) |>
          tail(1)
      }
      x |>
        stringr::str_remove_all("\`") |>
        stringr::str_remove("json") |>
        jsonlite::fromJSON()
    }) |>
    dplyr::bind_rows()
}


# set max chunk size... this might need refning based on what API seems to accept
max_chunk_size <- 50
sleep_time_between_chunks <- 0
chat_function <- chat_openai
model <- "gpt-4o-2024-08-06"
output_name <- "output_openai_gpt_4o"

# get our prompts of cod_vector for processing
list_x <- word_vector |> split_vector(max_chunk_size)
prompts_list <- list_x |> purrr::map(glue_to_json)
vectors <- seq_along(prompts_list)

# create output vector
llm_output <- vector("list", length = length(prompts_list))

# loop through and collect results!
for (i in vectors) {
  llm_chat <- do.call(chat_function, list(model = model, system_prompt = initial_prompt))

  llm_output[[i]] <- llm_chat$chat(prompts_list[[i]], echo = FALSE)

  cli::cli_alert_info(glue::glue("completed {i} of {length(vectors)}"))

  saveRDS(llm_output, glue::glue("./2025-03-25 report_words/Data/{output_name}.rds"))

  Sys.sleep(sleep_time_between_chunks)
}


word_emoji_map <- llm_output |> json_list_to_df()

saveRDS(word_emoji_map, glue::glue("./2025-03-25 report_words/Data/word_emoji_map.rds"))
