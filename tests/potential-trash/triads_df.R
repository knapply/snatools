library(tidyverse)

triads_df <- tibble::tribble(
  ~state,         ~representation,     ~description,
  "003",          "A,B,C",              "empty",
  "012",          "A->B, C",            "single directed edge",
  "102",          "A<->B, C",           "mutual connection between two vertices",
  "021D",         "A<-B->C",            "out-star",
  "021U",         "A->B<-C",            "in-star",
  "021C",         "A->B->C",            "directed line",
  "111D",         "A<->B<-C",            NA,
  "111U",         "A<->B->C",            NA,
  "030T",         "A->B<-C, A->C",       NA,
  "030C",         "A<-B<-C, A->C",       NA,
  "201",          "A<->B<->C",           NA,
  "120D",         "A<-B->C, A<->C",      NA,
  "120U",         "A->B<-C, A<->C",      NA,
  "120C",         "A->B->C, A<->C",      NA,
  "210",          "A->B<->C, A<->C",     NA,
  "300",          "A<->B<->C, A<->C",   "complete graph"
) %>% 
  dplyr::mutate_all(forcats::as_factor)

devtools::use_data(triads_df, overwrite = TRUE)
