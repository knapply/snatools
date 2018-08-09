# sna_as_edgelist ====
my_version <- function(x) {
  outl <- lapply(x$mel, `[[`, "outl")
  outl <- unlist(outl)
  inl <- lapply(x$mel, `[[`, "inl")
  inl <- unlist(inl)
  cbind(outl, inl)
}

# sna_get_vert_attrs
sna_get_vert_attrs.network <- function(nw) {
  out <- lapply(nw$val, `[`)
  out <- do.call(rbind, out)
  out <- apply(out, 2, as.list) 
  out <- lapply(out, unlist)
  out$na <- NULL
  names(out)[names(out) == "vertex.names"] <- "name"
  
  out[order(names(out))]
}

alt1 <- function(nw) {
  out <- lapply(nw$val, `[`)
  out <- do.call(rbind, out)
  out <- apply(out, 2, c)
  out <- lapply(out, unlist)
  out$na <- NULL
  names(out)[names(out) == "vertex.names"] <- "name"
  
  out[order(names(out))]
}

alt2 <- function(nw) { # winner
  out <- lapply(nw$val, `[`)
  out <- do.call(rbind, out)
  out_names <- colnames(out)
  out <- lapply(seq_len(ncol(out)), function(x) unlist(out[, x]))
  names(out) <- out_names
  out$na <- NULL
  names(out)[names(out) == "vertex.names"] <- "name"
  
  out[order(names(out))]
}


bench::mark(
  sna_get_vert_attrs.network(nw),
  alt1(nw),
  alt2(nw)
) %>% ggplot2::autoplot()

nw <- snatools:::build_test_graph("nw", n = 100)


data("emon", package = "network")
emon$Cheyenne

nw <- emon$Cheyenne
