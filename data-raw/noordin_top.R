library(tidyverse)

target_url <- "https://sites.google.com/site/ucinetsoftware/datasets/covert-networks/noordintop/Noordin%20Top%20CSV.zip?attredirects=0&d=1"

download.file(target_url, destfile = "datasets/noordin_top.zip", mode = "wb")

unzip("datasets/noordin_top.zip", exdir = "datasets")

file_names <- list.files("datasets/CSV", full.names = TRUE)

relations <- file_names %>% 
  str_extract("(?<=^datasets/CSV/NOORDINTOP_).*?(?=\\.csv)") %>% 
  str_replace("\\&", "_") %>% 
  str_replace("TRAININGEVENTS", "TRAINING_EVENTS") %>% 
  str_to_lower()

noordin_top <- file_names %>%
  map(read_csv) %>% 
  map(~ gather(.x, var, val, -X1)) %>% 
  map(~ filter(.x, val != 0)) %>% 
  map(~ rename(.x, .ego = X1, .alter = var)) %>% 
  map2_df(relations, ~ mutate(.x, relation = .y)) %>% 
  select(-val) %>% 
  igraph::graph_from_data_frame(directed = FALSE)

# where be the attributes?
