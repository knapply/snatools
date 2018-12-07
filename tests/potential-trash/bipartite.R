#' Assign vertex attributes to the distinguish sets in `network` objects.
#' 
#' @param x A [`network::network`] object.
#' 
#' @return A [`network::network`] object with a new, `logical` vertex attribute named 
#' `is_actor`.
#' 
#' @examples 
#' library(snatools)
#' 
#' sw_matrix <- matrix(
#'   c(1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0,
#'     1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 
#'     1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 
#'     0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'     1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
#'     1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0,
#'     0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
#'     1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1,
#'     1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 
#'     0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
#'     0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 
#'     1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0),
#'   nrow = 14L, ncol = 18L,
#'   dimnames = list(
#'   c("E1", "E2", "E3", "E4", "E5", "E6", "E7",
#'     "E8", "E9", "E10", "E11", "E12", "E13", "E14"),
#'   c("EVELYN", "LAURA", "THERESA", "BRENDA", "CHARLOTTE", "FRANCES",
#'     "ELEANOR", "PEARL", "RUTH", "VERNE", "MYRNA", "KATHERINE", 
#'     "SYLVIA", "NORA", "HELEN", "DOROTHY", "OLIVIA", "FLORA"))
#'     )
#'     
#' (southern_women_nw <- sw_matrix %>% 
#'   t() %>% 
#'   network::as.network.matrix())
#'   
#' southern_women_nw %>% vrt_get_attrs()
#'   
#' (actors_clarified <- bip_clarify_actors(southern_women_nw))
#' 
#' actors_clarified %>% vrt_get_attrs()
#' 
bip_clarify_actors <- function(x) {
  if(class(x) != "network") {
    stop("`bip_clarify_actors()` is only applicable to `network` objects.")
  }
  if(!net_is_bipartite(x)) {
    stop("`bip_clarify_actors()` is only applicable to bipartite `network` objects.")
  }
  if(!"is_actor" %in% vrt_get_attr_names(x)) {
    actor_n <- x$gal$bipartite
    vrt_n <- x$gal$n
    is_actor_attr <- c(rep(TRUE, actor_n), rep(FALSE, vrt_n - actor_n))
    network::set.vertex.attribute(x, "is_actor", is_actor_attr) # modifies in-place
  }
  x
}

#' Swap vertex modes in bipartite graphs.
#' 
#' @param x A bipartite `igraph` ([`igraph::graph`]) or [`network::network`] object.
#' @param clarify_actors Whether to also run `bip_clarify_actors()` on [`network::network`] 
#' objects.
#' 
#' @examples 
#' library(snatools)
#' 
#' sw_matrix <- matrix(
#'   c(1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0,
#'     1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 
#'     1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 
#'     0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'     1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
#'     1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0,
#'     0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
#'     1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1,
#'     1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 
#'     0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
#'     0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 
#'     1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0),
#'   nrow = 14L, ncol = 18L,
#'   dimnames = list(
#'   c("E1", "E2", "E3", "E4", "E5", "E6", "E7",
#'     "E8", "E9", "E10", "E11", "E12", "E13", "E14"),
#'   c("EVELYN", "LAURA", "THERESA", "BRENDA", "CHARLOTTE", "FRANCES",
#'     "ELEANOR", "PEARL", "RUTH", "VERNE", "MYRNA", "KATHERINE", 
#'     "SYLVIA", "NORA", "HELEN", "DOROTHY", "OLIVIA", "FLORA"))
#'     )
#'     
#' (southern_women_ig <- sw_matrix %>% 
#'   t() %>% 
#'   igraph::graph_from_incidence_matrix())
#' 
#' southern_women_ig %>% vrt_get_attrs()
#' 
#' southern_women_ig %>% 
#'   bip_swap_modes() %>%
#'   vrt_get_attrs()
#' 
#' (southern_women_nw <- sw_matrix %>% 
#'   network::as.network.matrix() %>% 
#'   bip_clarify_actors())
#'   
#' southern_women_nw %>% vrt_get_attrs()
#'   
#' southern_women_nw %>%
#'   bip_swap_modes() %>%
#'   vrt_get_attrs()
#' 
#' @export
#' 
bip_swap_modes <- function(x, ...) {
  UseMethod("bip_swap_modes")
}

#' @rdname bip_swap_modes
#' 
#' @export
#' 
bip_swap_modes.igraph <- function(x) {
  if(!net_is_bipartite(x)) {
    stop("`bip_swap_modes()` is only applicable to bipartite graphs.")
  }
  igraph::V(x)$type <- ifelse(igraph::V(x)$type, FALSE, TRUE)
  
  x
}

#' @rdname bip_swap_modes
#' 
#' @export
#' 
bip_swap_modes.network <- function(x, clarify_actors = TRUE) {
  if(!net_is_bipartite(x)) {
    stop("`bip_swap_modes()` is only applicable to bipartite graphs.")
  }
  net_attrs <- net_get_attrs(x)
  
  two_mode_matrix <- t(network::as.matrix.network.adjacency(x))
  new_actor_n <- nrow(two_mode_matrix)
  old_actors <- seq_len(x$gal$bipartite)
  new_actors <- (x$gal$bipartite + 1):x$gal$n
  
  vrt_attrs <- vrt_get_attrs(x)
  old_actor_attrs <- lapply(vrt_attrs, `[`, old_actors)
  new_actor_attrs <- lapply(vrt_attrs, `[`, new_actors)

  new_vrt_attrs <- Map(c, new_actor_attrs, old_actor_attrs)
  
  out <- network::as.network.matrix(two_mode_matrix, 
                                    matrix.type = "adjacency",
                                    directed = x$gal$directed, 
                                    hyper = x$gal$hyper,
                                    loops = x$gal$loops,
                                    multiple = x$gal$multiple, 
                                    bipartite = new_actor_n)
  
  if(length(new_vrt_attrs)){
    for(v_attr in names(new_vrt_attrs)){
      network::set.vertex.attribute(out, v_attr, new_vrt_attrs[[v_attr]]) # assigns invisibly
    }
  }
  if(length(net_attrs)){
    for(g_attr in names(net_attrs)) {
      if(!g_attr %in% names(x$gal)) {
        network::set.network.attribute(out, g_attr, net_attrs[[g_attr]]) # assigns invisibly
      }
    }
  }
  if(clarify_actors) {
    is_actor_attr <- c(rep(TRUE, length(new_actors)), rep(FALSE, length(old_actors)))
    network::set.vertex.attribute(out, "is_actor", value = is_actor_attr) # assigns invisibly
  }
  
  out
}
