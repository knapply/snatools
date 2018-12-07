# simple graph data ====
el <- matrix(c(1, 2,
               2, 3,
               3, 4,
               1, 4,
               4, 5), ncol = 2L, byrow = TRUE)
ig_attrs <- data.frame(lab = letters[seq_len(nrow(el))],
                       foo = LETTERS[seq_len(nrow(el))],
                       stringsAsFactors = FALSE)
nw_attrs <- ig_attrs

if (!identical(ig_attrs[[1]], nw_attrs[[1]]) || !identical(ig_attrs[[2]], nw_attrs[[2]])) {
  stop("prepared igraph attributes don't match prepared network attributes.",
       call. = FALSE)
}

ig <- igraph::graph_from_edgelist(el)
igraph::edge_attr(ig) <- ig_attrs

nw <- network::as.network.matrix(el, matrix.type = "edgelist")
network::set.edge.attribute(nw, attrname = names(nw_attrs), value = nw_attrs)

tg <- tidygraph::as_tbl_graph(ig)

if (as_bridge_net(ig) %==% as_bridge_net(nw)) {
  bn <- as_bridge_net(ig)
  } else {
    stop("ig and nw don't match... something went wrong with as_bridge_net()",
         call. = FALSE)
}

edge_df <- igraph::as_data_frame(ig)
rownames(edge_df) <- NULL
colnames(edge_df)[[1]] <- ".ego"
colnames(edge_df)[[2]] <- ".alter"
edge_df$.ego <- as.integer(edge_df$.ego)
edge_df$.alter <- as.integer(edge_df$.alter)

#_ ====
# bipartite graph data ====
n_actors <- 6
n_events <- 3
affil_matrix <- matrix(c(1, 0, 1 ,0, 1, 0,
                         0, 1, 0, 1, 0, 1,
                         1, 1, 1, 1, 1, 1),
                       nrow = n_events, byrow = TRUE)

ig_bip_verts <- data.frame(name = c(paste0("actor", seq_len(n_actors)),
                                    paste0("event", seq_len(n_events))),
                           type = c(rep(TRUE, ncol(affil_matrix)),
                                    rep(FALSE, nrow(affil_matrix))),
                           stringsAsFactors = FALSE)


rownames(affil_matrix) <- seq_len(n_events)
colnames(affil_matrix) <- seq(n_events + 1, n_actors + n_events)
ig_bip_attrs <- data.frame(lab = letters[seq_len(sum(affil_matrix))],
                           foo = LETTERS[seq_len(sum(affil_matrix))],
                           stringsAsFactors = FALSE)

ig_bip <- igraph::graph_from_incidence_matrix(affil_matrix,directed = FALSE)
ig_bip <- prep_bipartite_igraph(ig_bip)

igraph::vertex_attr(ig_bip) <- ig_bip_verts
igraph::edge_attr(ig_bip) <- ig_bip_attrs

el_bip <- igraph::as_edgelist(ig_bip, names = FALSE)
nw_bip_attrs <- igraph::as_data_frame(ig_bip)
nw_bip_attrs$from <- NULL
nw_bip_attrs$to <- NULL

nw_bip_verts <- ig_bip_verts
names(nw_bip_verts)[names(nw_bip_verts) == "name"] <- "vertex.names"
names(nw_bip_verts)[names(nw_bip_verts) == "type"] <- ".actor"

nw_bip <- network::network.initialize(n_actors + n_events, directed = FALSE, 
                                      bipartite = n_actors)
network::add.edges(nw_bip, tail = el_bip[, 1], head = el_bip[, 2])
network::set.vertex.attribute(nw_bip, names(nw_bip_verts), nw_bip_verts)
network::set.edge.attribute(nw_bip, names(nw_bip_attrs), nw_bip_attrs)

# if (as_bridge_net(ig_bip) %==% as_bridge_net(nw_bip)) {
if (identical(as_bridge_net(ig_bip), as_bridge_net(nw_bip))) {
  bn_bip <- as_bridge_net(ig_bip)
  } else {
    stop("ig_bip and nw_bip don't match... something went wrong with as_bridge_net()
         the only way you can manually match bipartite graph seems to be building an 
         igraph then running prep_bipartite_igraph() and extracting the edge list to add
         to an empty network.",
         call. = FALSE)
}

tg_bip <- tidygraph::as_tbl_graph(ig_bip)

bip_edge_df <- igraph::as_data_frame(ig_bip)
rownames(bip_edge_df) <- NULL
colnames(bip_edge_df)[[1]] <- ".ego"
colnames(bip_edge_df)[[2]] <- ".alter"


context("test full conversions")

test_that("round trip conversions work consistently", {
  expect_true(ig %==% as_igraph(as_network(ig)))
  expect_true(ig_bip %==% as_igraph(as_network(ig_bip)))
  expect_true(nw %==% as_network(as_igraph(nw)))
  expect_true(nw_bip %==% as_network(as_igraph(nw_bip)))
  expect_true(bn %==% as_bridge_net(as_network(as_igraph(bn))))
  expect_true(bn_bip %==% as_bridge_net(as_network(as_igraph(bn_bip))))
  expect_true(tg %==% tidygraph::as_tbl_graph(as_igraph(as_network(tg))))
  expect_true(tg_bip %==% tidygraph::as_tbl_graph(as_igraph(as_network(tg_bip))))
})




