monastery <- "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/sampson.dat" %>% 
  snatools::read_ucinet() %>%
  snatools::as_igraph()

devtools::use_data(monastery, overwrite = TRUE)
