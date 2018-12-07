#' Subset by Edges
#' 
#' @template graph-param
#' @param .predicate A single predicate function describing how `x`'s edges should be 
#' subset.
#' @param drop_isolates `logical` (Default: `FALSE`) indicating whether isolated vertices shuold be dropped.
#' 
#' @template bknapp-author
#' 
#' @examples 
#' library(snatools)
#' 
#' florence %>% 
#'   edg_subset(relation == "business")
#' 
#' crisis_in_cloister %>% 
#'   edg_subset(time == 1)
#'   
#' crisis_in_cloister %>% 
#'   as_network() %>% 
#'   edg_subset(time == 3 & affect < 0)
#'   
#' crisis_in_cloister %>% 
#'   edg_subset(time == 3 & affect < 0, drop_isolates = TRUE)
#' 
#' @importFrom rlang !! enquo eval_tidy quo
#' @export
edg_subset <- function(x, .predicate, .drop_isolates = FALSE) {
  validate_args(x, validate_graph = TRUE)
  bn <- as_bridge_net(x)
  bn[["edges"]] <- eval_tidy(quo(bn[["edges"]][!!enquo(.predicate), , drop = FALSE]),
                             data = bn[["edges"]])
  
  bn[["edges"]] <- na.omit(bn[["edges"]])
  
  if (.drop_isolates) {
    .remaining <- unique(c(bn[["edges"]][[".ego"]], bn[["edges"]][[".alter"]]))
    bn[["vertices"]] <- bn[["vertices"]][bn[["vertices"]][[".vrt_id"]] %in% 
                                           .remaining, , drop = FALSE]
    bn[["edges"]][[".ego"]] <- match(bn[["edges"]][[".ego"]],
                                     bn[["vertices"]][[".vrt_id"]])
    bn[["edges"]][[".alter"]] <- match(bn[["edges"]][[".alter"]],
                                       bn[["vertices"]][[".vrt_id"]])
    bn[["edges"]] <- bn[["edges"]][!is.na(bn[["edges"]][[".ego"]]), , drop = FALSE]
    bn[["edges"]] <- bn[["edges"]][!is.na(bn[["edges"]][[".alter"]]), , drop = FALSE]
  }
  bn[["metadata"]] <- get_metadata(bn)
  
  switch(class(x)[[1L]],
         igraph = as_igraph(bn),
         network = as_network(bn))
}


#' Subset by Vertices
#' 
#' @template graph-param
#' @param .predicate A single predicate function describing how `x`'s edges should be 
#' subset.
#' 
#' @template bknapp-author
#' 
#' @examples 
#' library(snatools)
#' 
#' crisis_in_cloister %>% 
#'   vrt_subset(cloisterville)
#'   
#' crisis_in_cloister %>% 
#'   as_network() %>% 
#'   vrt_subset(!cloisterville)
#' 
#' @importFrom rlang !! enquo eval_tidy quo
#' @export
vrt_subset <- function(x, .predicate) {
  validate_args(x, validate_graph = TRUE)
  bn <- as_bridge_net(x)
  bn[["vertices"]] <- eval_tidy(quo(bn[["vertices"]][!!enquo(.predicate), , drop = FALSE]),
                             data = bn[["vertices"]])
  
  bn[["edges"]] <- bn[["edges"]][bn[["edges"]][[".ego"]] %in% 
                                 bn[["vertices"]][[".vrt_id"]], , drop = FALSE]
  bn[["edges"]] <- bn[["edges"]][bn[["edges"]][[".alter"]] %in% 
                                 bn[["vertices"]][[".vrt_id"]], , drop = FALSE]
  
  .remaining <- unique(c(bn[["edges"]][[".ego"]], bn[["edges"]][[".alter"]]))
  bn[["vertices"]] <- bn[["vertices"]][bn[["vertices"]][[".vrt_id"]] %in% 
                                         .remaining, , drop = FALSE]
  bn[["edges"]][[".ego"]] <- match(bn[["edges"]][[".ego"]],
                                   bn[["vertices"]][[".vrt_id"]])
  bn[["edges"]][[".alter"]] <- match(bn[["edges"]][[".alter"]],
                                     bn[["vertices"]][[".vrt_id"]])
  bn[["edges"]] <- bn[["edges"]][!is.na(bn[["edges"]][[".ego"]]), , drop = FALSE]
  bn[["edges"]] <- bn[["edges"]][!is.na(bn[["edges"]][[".alter"]]), , drop = FALSE]

  bn[["metadata"]] <- get_metadata(bn)
  
  switch(class(x)[[1]],
         igraph = as_igraph.bridge_net(bn),
         network = as_network.bridge_net(bn))
}



