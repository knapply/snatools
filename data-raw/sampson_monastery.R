library(tidyverse)
library(snatools)

names_dict <- c(JOHN_1 = "John Bosco", 
                GREG_2 = "Gregory", 
                BASIL_3 = "Basil",
                PETER_4 = "Peter", 
                BONAVEN_5 = "Bonaventure", 
                BERTH_6 = "Berthold", 
                MARK_7 = "Mark", 
                VICTOR_8 = "Victor", 
                AMBROSE_9 = "Ambrose",
                ROMUL_10 = "Romauld",
                LOUIS_11 = "Louis",
                WINF_12 = "Winfrid",
                AMAND_13 = "Amand", 
                HUGH_14 = "Hugh",
                BONI_15 = "Boniface",
                ALBERT_16 = "Albert",
                ELIAS_17 = "Elias",
                SIMP_18 = "Simplicius")

relation_dict <- c(SAMPLK1 = "liking",
                   SAMPLK2 = "liking",
                   SAMPLK3 = "liking",
                   SAMPDLK = "disliking",
                   SAMPES = "esteem", 
                   SAMPDES = "disesteem",
                   SAMPIN = "positive influence", 
                   SAMPNIN = "negative influence",
                   SAMPPR = "praise",
                   SAMNPR = "blame")

data(samplk, package = "ergm")

vertices <- list(samplk1, samplk2, samplk3) %>% 
  map_df(vrt_get_attrs) %>% 
  distinct() %>% 
  rename(name = vertex.names) %>% 
  select(name, everything()) %>% 
  mutate(status = case_when(
    row_number() %in% c(2, 3, 17, 18) ~ "Expelled",
    row_number() %in% c(1, 7, 14, 15, 16) ~ "Left Voluntarily",
    TRUE ~ "Remained"
    )) %>% 
  rename(faction = group) %>% 
  mutate(faction = str_replace(faction, "Turks", "Young Turks"))

dat <- "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/sampson.dat" %>% 
  read_ucinet()

dat$edge_attributes <- dat$edge_attributes %>% 
  mutate(time = case_when(
    level == "SAMPLK1" ~ 1L,
    level == "SAMPLK2" ~ 2L,
    TRUE ~ 3L
  )) %>% 
  rename(relation = level) %>% 
  mutate(relation = recode(relation, !!!relation_dict)) %>% 
  mutate(positive_relation = relation %in% c("liking", "esteem",
                                             "positive influence", "praise"))

dat$vertex_attributes <- dat$vertex_attributes %>% 
  mutate_at(vars(from, to), ~ recode(., !!!names_dict)) %>%
  left_join(vertices, by = "name")

sampson_monastery <- dat

devtools::use_data(sampson_monastery, overwrite = TRUE)
