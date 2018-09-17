library(snatools)
library(tidyverse)

padgw <- tribble(
  ~name,          ~net_wealth,      ~priorates,
  "ACCIAIUOL",             10,              53,
  "ALBIZZI",               36,              65,
  "RIDOLFI",               27,              38,
  "STROZZI",              146,              74,
  "BARBADORI",             55,               0,
  "BISCHERI",              44,              12,
  "CASTELLAN",             20,              22,
  "GUADAGNI",               8,              21,
  "LAMBERTES",             42,               0,
  "MEDICI",               103,              53,
  "PAZZI",                 48,               0,
  "PERUZZI",               49,              42,
  "SALVIATI",              10,              35,
  "TORNABUON",             48,               0,
  "GINORI",                32,               0,
  "PUCCI",                  3,               0
) %>% 
  mutate(name = str_to_title(name)) %>% 
  mutate_if(is.numeric, as.integer)

relation_dict <- c(PADGB = "business",
                   PADGM = "marriage")

dat <- "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/padgett.dat" %>%
  read_ucinet()

dat$edge_attributes <- dat$edge_attributes %>% 
  rename(relation = level) %>% 
  mutate(relation = recode(relation, !!!relation_dict))

dat$vertex_attributes <- dat$vertex_attributes %>%
  mutate(name = str_to_title(name))

dat$vertex_attributes <- dat$vertex_attributes %>% 
  left_join(padgw, by = "name")

padgett_florence <- dat

devtools::use_data(padgett_florence, overwrite = TRUE)
