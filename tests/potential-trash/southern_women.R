davis_southern_women <- "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/davis.dat" %>%
  snatools::read_ucinet() 

devtools::use_data(davis_southern_women, overwrite = TRUE)