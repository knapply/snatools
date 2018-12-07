# Sampson.net
library(tidyverse)
target_url <- "http://vlado.fmf.uni-lj.si/pub/networks/data/esna/Sampson.zip"
temp_file <- tempfile(fileext = ".zip", tmpdir = "datasets")
download.file(target_url, destfile = temp_file, mode = "wb")
unzip(temp_file, exdir = "datasets")
# Pajek longitudinal network data file, 25 vertices (novices), 
# 322 valued arcs (affect relations; 3 - most liked peer, 2 - second
# most liked, 1 - third most liked, -1 - third least liked, -2, 
# second least liked, -3 - least liked), no edges, no loops, five 
# different moments (time 1 thru 5). 
raw <- read_lines("datasets/Sampson.paj")

all_vert_names <- raw[3:27] %>%
  as_tibble() %>% 
  mutate(.vrt_id = value %>% 
           str_extract("^\\s{0,1}\\d+") %>% 
           str_trim() %>% 
           as.integer()
        ) %>% 
  mutate(name = value %>% 
           str_extract('(?<=").*?(?=")') %>% 
           str_trim()
        ) %>% 
  mutate(time = value %>% 
           str_extract_all("\\d") %>% 
           map(as.integer) %>% 
           map(`length<-`, 5L)
          ) %>% 
  select(-value, -.vrt_id, -time)

all_vert_cloisterville <- raw[411:435] %>%
  as_tibble() %>% 
  rename(cloisterville = value) %>% 
  mutate(cloisterville = as.logical(as.integer(cloisterville)))

# vert_t4_name <- raw[355:372] %>%
#   as_tibble() %>% 
#   mutate(.vrt_id = value %>% 
#            str_extract("^\\s{0,1}\\d+") %>% 
#            str_trim() %>% 
#            as.integer()
#         ) %>% 
#   mutate(name = value %>% 
#            str_extract('(?<=").*?(?=")') %>% 
#            str_trim()
#         ) %>% 
#   select(-value, -.vrt_id)

# vert_t4_faction <- raw[460:477] %>% 
#   as_tibble() %>% 
#   rename(faction = value) %>% 
#   mutate(faction = as.integer(faction)) %>% 
#   mutate(faction = case_when(
#     faction == 1L ~ "Young Turks",
#     faction == 2L ~ "Loyal Opposition",
#     faction == 3L ~ "Outcasts",
#     faction == 4L ~ "Interstitial"
#   ))

# vert_t4 <- bind_cols(vert_t4_name, vert_t4_faction)
# vert_t4_dict <- vert_t4$name %>% 
#   set_names(seq_along(.))

all_vert <- bind_cols(all_vert_names, all_vert_cloisterville)
all_vert_dict <- all_vert$name %>% 
  set_names(seq_along(.))

all_vert_edges <- raw[29:350] %>%
  as_tibble() %>% 
  filter(nchar(value) > 0L) %>% 
  mutate(value = str_trim(value)) %>% 
  separate(value, c(".ego", ".alter", "affect", "time"), sep = "\\s+") %>% 
  mutate(affect = as.integer(affect),
         time = time %>% 
           str_extract_all("\\d") %>% 
           map(`length<-`, 5L) %>% 
           map(as.integer)
        ) %>% 
  unnest() %>% 
  drop_na(time) %>% 
  mutate_at(vars(.ego, .alter), ~ recode(., !!!all_vert_dict))
  
# t4_edges <- raw[374:406] %>%
#   as_tibble() %>%
#   mutate(value = str_trim(value)) %>%
#   separate(value, c(".ego", ".alter", "relation2"), sep = "\\s+") %>%
#   mutate_all(as.integer) %>%
#   mutate(relation2 = if_else(relation2 == 1L, "most liked", "most disliked"),
#          time = 4L) %>%
#   mutate_at(vars(.ego, .alter), ~ recode(., !!!vert_t4_dict))

verts <- full_join(all_vert, vert_t4, by = "name")
edges <- bind_rows(all_vert_edges, t4_edges)

crisis_in_cloister <- igraph::graph_from_data_frame(all_vert_edges, 
                                                    vertices = all_vert)

usethis::use_data(crisis_in_cloister)