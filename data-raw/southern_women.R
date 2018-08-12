southern_women <- "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/davis.dat" %>%
  snatools::read_ucinet() %>%
  snatools::as_igraph()

devtools::use_data(southern_women, overwrite = TRUE)