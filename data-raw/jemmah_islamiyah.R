library(tidyverse)

target_url <- "https://sites.google.com/site/ucinetsoftware/datasets/covert-networks/jemaahislamiyahkoschade/Jemaah%20Islamiyah%20Koschade%20CSV.zip?attredirects=0&d=1"

download.file(target_url, destfile = "datasets/jemmah_islamiyah.zip", mode = "wb")

unzip("datasets/jemmah_islamiyah.zip", exdir = "datasets/JI")

jemmah_islamiyah <- read_csv("datasets/JI/CSV/JI_KOSCHADE.csv") %>% 
  as.data.frame() %>% 
  `rownames<-`(.$X1) %>% 
  select(-X1) %>% 
  as.matrix() %>% 
  igraph::graph_from_adjacency_matrix(mode = "undirected", 
                                      weighted = TRUE) %>% 
  snatools:::as_bridge_net() %>% 
  snatools:::as_igraph.bridge_net()


usethis::use_data(jemmah_islamiyah)
