library(igraph)
library(ggraph)
library(tidyverse)

init <- "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/padgett.dat" %>%
  snatools::read_ucinet() %>%
  snatools::as_igraph()

florence <- init %>% 
  map_df(igraph::as_data_frame, .id = "type") %>%
  select(from, to, type) %>% 
  graph_from_data_frame(directed = FALSE)

devtools::use_data(florence, overwrite = TRUE)
