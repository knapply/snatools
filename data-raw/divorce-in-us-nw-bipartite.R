target_url <- "http://vlado.fmf.uni-lj.si/pub/networks/data/2mode/divorce.net"
nw <- network::read.paj(target_url) 

save(divorce_nw, "data/divorce_nw.rda")

readr::write_rds(divorce_nw, "data/divorce_nw.rds")