library(tidyverse)

target_url <- "http://vlado.fmf.uni-lj.si/pub/networks/data/GBM/davis/DeepSouth.paj"

download.file(target_url, destfile = "datasets/DeepSouth.paj")

raw <- read_lines("datasets/DeepSouth.paj")

vert_attrs <- raw[17:79] %>%
  as_tibble() %>% 
  filter(nchar(value) > 0L) %>% 
  separate(value, c(".vrt_id", "name", "coords"), sep = "\"") %>% 
  select(-coords) %>% 
  mutate_all(str_trim) %>% 
  mutate(.vrt_id = as.integer(.vrt_id)) %>% 
  # mutate(event_date = str_extract(name, "\\d{2}/\\d{2}")) %>% 
  mutate(event_date = str_extract(name, "(?<=^E\\d{2}:)\\d{2}/\\d{2}($=?)")) %>%
  mutate(name = str_remove(name, "(?<=^E\\d{2}):\\d{2}/\\d{2}$"))

event_names <- vert_attrs1 %>% 
  filter(!is.na(event_date)) %>% 
  pull(name)

actor_names <- vert_attrs1 %>% 
  filter(is.na(event_date)) %>% 
  pull(name)

affil_matrix <- raw[83:117] %>%
  as_tibble() %>% 
  filter(nchar(value) > 0L) %>% 
  separate(value, event_names, sep = "\\s+") %>% 
  as.matrix() %>% 
  `rownames<-`(actor_names) %>% 
  `storage.mode<-`("integer")

library(tidygraph)
library(igraph)

southern_women <- affil_matrix %>%
  graph_from_incidence_matrix() %>% 
  as_tbl_graph() %>% 
  mutate(event_date = vert_attrs$event_date) %E>% 
  mutate(event_date = .N()$event_date[to]) %>% 
  mutate(event_month = event_date %>% 
           str_extract("^\\d{2}(?=/\\d{2}$)") %>% 
           as.integer()
        ) %>% 
  mutate(event_day = event_date %>% 
           str_extract("(?<=^\\d{2}/)\\d{2}$") %>% 
           as.integer()
        ) %N>% 
  mutate(type = !type) %>% 
  select(-event_date) %>% 
  as_igraph() %>% 
  snatools:::as_bridge_net() %>% 
  snatools:::as_igraph.bridge_net()

usethis::use_data(southern_women)

