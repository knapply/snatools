net_is_directed <- function(.net) {
  UseMethod("net_is_directed")
}

#' @importFrom igraph is_directed
net_is_directed.igraph <- function(.net) {
  is_directed(.net)
}

net_is_directed.network <- function(.net) {
  isTRUE(.net[["gal"]][["directed"]])
}

net_is_weighted <- function(.net) {
  "weight" %in% edge_get_attr_names(.net)
}

