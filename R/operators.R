#' Confirm `bridge_net`s, `igraph`s, and `network`s are `identical()`.
#'
#' @param lhs A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param rhs A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' 
#' @return `logical` indicating whether `lhs` and `rhs` are `identical()`, excluding
#' non-structural network attributes.
#' 
#' @export
`%==%` <- function(lhs, rhs) {
  if (!class(lhs)[[1]] %in% c("bridge_net", "igraph", "network", "tbl_graph")) {
    # return(lhs_name)
    terminate(patch("`%s` is not a `bridge_net`, `igraph`, `network`, or `tbl_graph`.",
                    deparse(substitute(lhs))))
  }
  if (!class(rhs)[[1]] %in% c("bridge_net", "igraph", "network", "tbl_graph")) {
    terminate(patch("`%s` is not a `bridge_net`, `igraph`, `network`, or `tbl_graph`.",
                    deparse(substitute(rhs))))
  }
  lhs <- as_bridge_net(lhs)
  lhs[["net_attrs"]] <- NULL
  
  rhs <- as_bridge_net(rhs)
  rhs[["net_attrs"]] <- NULL
  
  identical(lhs, rhs)
}
