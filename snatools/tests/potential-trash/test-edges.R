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


context("edges, extraction, deletion, filtering")
#_ ====
# Extract Edges/Attributes ====
test_that("edges and their attributes can be extracted consistently", {
#_ edg_attr_names() ====
#** igraph ====
  expect_identical(edg_attr_names(ig)[[1]], "lab")
  expect_identical(edg_attr_names(ig)[[2]], "foo")
  
  expect_identical(edg_attr_names(ig_bip)[[1]], "lab")
  expect_identical(edg_attr_names(ig_bip)[[2]], "foo")
#** network ====
  expect_identical(edg_attr_names(nw)[[1]], "lab")
  expect_identical(edg_attr_names(nw)[[2]], "foo")
  
  expect_identical(edg_attr_names(nw_bip)[[1]], "lab")
  expect_identical(edg_attr_names(nw_bip)[[2]], "foo")
#** bridge_net ====
  expect_identical(edg_attr_names(bn)[[1]], "lab")
  expect_identical(edg_attr_names(bn)[[2]], "foo")
  
  expect_identical(edg_attr_names(bn_bip)[[1]], "lab")
  expect_identical(edg_attr_names(bn_bip)[[2]], "foo")
#** tbl_graph ====
  expect_identical(edg_attr_names(tg)[[1]], "lab")
  expect_identical(edg_attr_names(tg)[[2]], "foo")
  
  expect_identical(edg_attr_names(tg_bip)[[1]], "lab")
  expect_identical(edg_attr_names(tg_bip)[[2]], "foo")
  
#_ edg_get_attr() ====
#** igraph ====
  expect_identical(edg_get_attr(ig, "lab"), ig_attrs$lab)
  expect_identical(edg_get_attr(ig, "foo"), ig_attrs$foo)
  
  expect_identical(edg_get_attr(ig_bip, "lab"), ig_bip_attrs$lab)
  expect_identical(edg_get_attr(ig_bip, "foo"), ig_bip_attrs$foo)
#** network ====
  expect_identical(edg_get_attr(nw, "lab"), nw_attrs$lab)
  expect_identical(edg_get_attr(nw, "foo"), nw_attrs$foo)
  
  expect_identical(edg_get_attr(nw_bip, "lab"), nw_bip_attrs$lab)
  expect_identical(edg_get_attr(nw_bip, "foo"), nw_bip_attrs$foo)
#** bridge_net ====
  expect_identical(edg_get_attr(bn, "lab"), ig_attrs$lab)
  expect_identical(edg_get_attr(bn, "foo"), ig_attrs$foo)
  
  expect_identical(edg_get_attr(bn_bip, "lab"), nw_bip_attrs$lab)
  expect_identical(edg_get_attr(bn_bip, "lab"), ig_bip_attrs$lab)
  expect_identical(edg_get_attr(bn_bip, "foo"), nw_bip_attrs$foo)
  expect_identical(edg_get_attr(bn_bip, "foo"), ig_bip_attrs$foo)
#** tbl_graph ====
  expect_identical(edg_get_attr(tg, "lab"), ig_attrs$lab)
  expect_identical(edg_get_attr(tg, "foo"), ig_attrs$foo)
  
  expect_identical(edg_get_attr(tg_bip, "lab"), ig_bip_attrs$lab)
  expect_identical(edg_get_attr(tg_bip, "foo"), ig_bip_attrs$foo)

#_ edg_to_df() ====
#** igraph ====
  expect_identical(edg_to_df(ig, include_dyad = FALSE, leave_raw = TRUE), ig_attrs)
  
  expect_identical(edg_to_df(ig_bip, include_dyad = FALSE, leave_raw = TRUE), ig_bip_attrs)
#** network ====
  expect_identical(edg_to_df(nw, include_dyad = FALSE, leave_raw = TRUE), nw_attrs)
  
  expect_identical(edg_to_df(nw_bip, include_dyad = FALSE, leave_raw = TRUE), nw_bip_attrs)
#** bridge_net ====
  expect_identical(edg_to_df(bn), as_edge_data_frame(edge_df))
  
  expect_identical(edg_to_df(bn_bip), as_edge_data_frame(bip_edge_df))
#** tbl_graph ====
  expect_identical(edg_to_df(tg, include_dyad = FALSE, leave_raw = TRUE), ig_attrs)
  
  expect_identical(edg_to_df(tg_bip, include_dyad = FALSE, leave_raw = TRUE), ig_bip_attrs)
})

# _ ====
# Delete Edge Attributes ====
test_that("edge attributes can be deleted consistently", {
#_ edg_delete_attr() ====
#** igraph ====
  expect_identical(edg_to_df(edg_delete_attr(ig, "foo"), 
                             include_dyad = FALSE, leave_raw = TRUE), 
                   ig_attrs[, "lab", drop = FALSE])
  
  expect_identical(edg_to_df(edg_delete_attr(ig_bip, "foo"), 
                             include_dyad = FALSE, leave_raw = TRUE), 
                   ig_bip_attrs[, "lab", drop = FALSE])
#** network ====
  expect_identical(edg_to_df(edg_delete_attr(nw, "foo"), 
                             include_dyad = FALSE, leave_raw = TRUE), 
                   nw_attrs[, "lab", drop = FALSE])
#** bridge_net ====
  expect_identical(edg_to_df(edg_delete_attr(bn, "foo")), 
                   edg_to_df(edg_delete_attr(ig, "foo")))
  expect_identical(edg_to_df(edg_delete_attr(bn, "foo")), 
                   edg_to_df(edg_delete_attr(nw, "foo")))
  
  expect_identical(edg_to_df(edg_delete_attr(bn_bip, "lab")), 
                   edg_to_df(edg_delete_attr(ig_bip, "lab")))
  # shouldn't delete .actor attribute
  # expect_identical(edg_to_df(edg_delete_attr(bn_bip, ".actor"))$.name, 
                   # edg_to_df(edg_delete_attr(nw_bip, ".actor"))$.name)
#** tbl_graph ====
  expect_identical(edg_to_df(edg_delete_attr(tg, "foo"), 
                             include_dyad = FALSE, leave_raw = TRUE), 
                   ig_attrs[, "lab", drop = FALSE])
  
  expect_identical(edg_to_df(edg_delete_attr(tg_bip, "foo"), 
                             include_dyad = FALSE, leave_raw = TRUE), 
                   ig_bip_attrs[, "lab", drop = FALSE])
})


# _ ====
# Filter Edges ====
test_that("vertices can be filtered consistently", {
#_ edg_filter() ====
#** igraph ====
  expect_identical(edg_to_df(edg_filter(ig, foo %in% c("A", "B")), 
                             include_dyad = FALSE, leave_raw = TRUE),
                   ig_attrs[ig_attrs$foo %in% c("A", "B"), c("lab", "foo")])
  expect_identical(edg_to_df(edg_filter(ig, foo %in% c("A", "B")),
                             include_dyad = FALSE),
                   as_edge_data_frame(ig_attrs[ig_attrs$foo %in% c("A", "B"), ]))
  
  expect_identical(edg_to_df(edg_filter(ig_bip, lab %in% c(letters[1:5])), 
                             include_dyad = FALSE, leave_raw = TRUE),
                   ig_bip_attrs[ig_bip_attrs$lab %in% c(letters[1:5]), c("lab", "foo")])
  expect_identical(edg_to_df(edg_filter(ig_bip, lab %in% c(letters[1:5])),
                             include_dyad = FALSE),
                   as_edge_data_frame(ig_bip_attrs[ig_bip_attrs$lab %in% 
                                                     c(letters[1:5]), ]))
#** network ====
  expect_identical(edg_to_df(edg_filter(nw, foo %in% c("A", "B")), 
                             include_dyad = FALSE, leave_raw = TRUE),
                   nw_attrs[nw_attrs$foo %in% c("A", "B"), ])
  expect_identical(edg_to_df(edg_filter(nw, foo %in% c("A", "B")),
                             include_dyad = FALSE),
                   as_edge_data_frame(nw_attrs[nw_attrs$foo %in% c("A", "B"), ]))
  
  expect_identical(edg_to_df(edg_filter(nw_bip, lab %in% c(letters[1:5])), 
                             include_dyad = FALSE, leave_raw = TRUE),
                   nw_bip_attrs[nw_bip_attrs$lab %in% c(letters[1:5]), c("lab", "foo")])
  expect_identical(edg_to_df(edg_filter(nw_bip, lab %in% c(letters[1:5])),
                             include_dyad = FALSE),
                   as_edge_data_frame(nw_bip_attrs[nw_bip_attrs$lab %in% 
                                                     c(letters[1:5]), ]))
#** bridge_net ====
  expect_identical(edg_to_df(edg_filter(bn, foo %in% c("A", "B")),
                             include_dyad = FALSE),
                   as_edge_data_frame(nw_attrs[nw_attrs$foo %in% c("A", "B"), ]))
  expect_identical(edg_to_df(edg_filter(bn_bip, lab %in% c("a", "b")),
                             include_dyad = FALSE),
                   as_edge_data_frame(ig_bip_attrs[ig_bip_attrs$lab %in% c("a", "b"), ]))
#_ edg_filter_se() ====
#** igraph ====
  expect_identical(
    edg_to_df(edg_filter_se(ig, "foo", `%in%`, c("A", "B")), 
              include_dyad = FALSE, leave_raw = TRUE),
    ig_attrs[ig_attrs$foo %in% c("A", "B"), ]
    )
  expect_identical(
    edg_to_df(edg_filter_se(ig, "foo", `%in%`, c("A", "B")),
              include_dyad = FALSE),
    as_edge_data_frame(ig_attrs[ig_attrs$foo %in% c("A", "B"), ])
    )
  
  expect_identical(
    edg_to_df(edg_filter_se(ig_bip, "foo", `%in%`, c("A", "B")), 
              include_dyad = FALSE, leave_raw = TRUE),
    ig_bip_attrs[ig_bip_attrs$foo %in% c("A", "B"), ]
    )
  expect_identical(
    edg_to_df(edg_filter_se(ig_bip, "foo", `%in%`, c("A", "B")),
              include_dyad = FALSE),
    as_edge_data_frame(ig_bip_attrs[ig_bip_attrs$foo %in% c("A", "B"), ])
    )
#** network ====
  expect_identical(
    edg_to_df(edg_filter_se(nw, "foo", `%in%`, c("A", "B")), 
              include_dyad = FALSE, leave_raw = TRUE),
    nw_attrs[nw_attrs$foo %in% c("A", "B"), ]
    )
  expect_identical(
    edg_to_df(edg_filter_se(nw, "foo", `%in%`, c("A", "B")),
              include_dyad = FALSE),
    as_edge_data_frame(nw_attrs[nw_attrs$foo %in% c("A", "B"), ])
    )
  
  expect_identical(
    edg_to_df(edg_filter_se(nw_bip, "foo", `%in%`, c("A", "B")), 
              include_dyad = FALSE, leave_raw = TRUE),
    nw_bip_attrs[nw_bip_attrs$foo %in% c("A", "B"), ]
    )
  expect_identical(
    edg_to_df(edg_filter_se(nw_bip, "foo", `%in%`, c("A", "B")),
              include_dyad = FALSE),
    as_edge_data_frame(nw_bip_attrs[nw_bip_attrs$foo %in% c("A", "B"), ])
    )
#** bridge_net ====
  expect_identical(
    edg_to_df(edg_filter_se(bn, "foo", `%in%`, c("A", "B")),
              include_dyad = FALSE),
    as_edge_data_frame(nw_attrs[nw_attrs$foo %in% c("A", "B"), ])
    )
  expect_identical(
    edg_to_df(edg_filter_se(bn_bip, "foo", `%in%`, c("A", "B")),
              include_dyad = FALSE),
    as_edge_data_frame(ig_bip_attrs[ig_bip_attrs$foo %in% c("A", "B"), ])
    )
})

#_ ====
# Convert `edges_data_frame`s ====
test_that("`edges_data_frame`s convert to `data.frame`s consistently", {
#** igraph ====
  expect_identical(as.data.frame.edge_data_frame(edg_to_df(ig)), edge_df)
  
  expect_identical(as.data.frame.edge_data_frame(edg_to_df(ig_bip)), bip_edge_df)
#** network ====
  expect_identical(as.data.frame.edge_data_frame(edg_to_df(nw)), edge_df)
  
  expect_identical(as.data.frame.edge_data_frame(edg_to_df(nw_bip)), bip_edge_df)
#** bridge_net ====
  expect_identical(as.data.frame.edge_data_frame(edg_to_df(bn)), edge_df)
  
  expect_identical(as.data.frame.edge_data_frame(edg_to_df(bn_bip)), bip_edge_df)
#** tbl_graph ====
  expect_identical(as.data.frame.edge_data_frame(edg_to_df(tg)), edge_df)
  
  expect_identical(as.data.frame.edge_data_frame(edg_to_df(tg_bip)), bip_edge_df)
})
  
#_ ====
# Print `edges_data_frame`s ====
test_that("`edges_data_frame`s print without error", {
#** igraph ====
  expect_output(print.edge_data_frame(edg_to_df(ig)))
  
  expect_output(print.edge_data_frame(edg_to_df(ig_bip)))
#** network ====
  expect_output(print.edge_data_frame(edg_to_df(nw)))
  
  expect_output(print.edge_data_frame(edg_to_df(nw_bip)))
#** bridge_net ====
  expect_output(print.edge_data_frame(edg_to_df(bn)))
  
  expect_output(print.edge_data_frame(edg_to_df(bn_bip)))
#** tbl_graph ====
  expect_output(print.edge_data_frame(edg_to_df(tg)))
  
  expect_output(print.edge_data_frame(edg_to_df(tg_bip)))
})

#_ ====
# `as_edge_data_frame`  ====
test_that("`as_edge_data_frame` fails on lists and matrices", {
  expect_error(as_edge_data_frame(as.list(ig_attrs)))
  expect_error(as_edge_data_frame(as.matrix(ig_attrs)))
})

#_ ====
# Validating Edge Attributes ====
test_that("validating edges attributes works consistently", {
  expect_true(is_valid_edg_attr(ig, "lab"))
  expect_true(is_valid_edg_attr(nw, "foo"))
  expect_true(is_valid_edg_attr(bn, "lab"))
  expect_true(is_valid_edg_attr(tg, "foo"))
  
  expect_false(is_valid_edg_attr(ig, "names"))
  expect_false(is_valid_edg_attr(nw, "edges.name"))
  expect_false(is_valid_edg_attr(bn, ".names"))
  expect_false(is_valid_edg_attr(tg, "names"))
  
  expect_null(validate_edg_attr(ig, "foo"))
  expect_null(validate_edg_attr(nw, "lab"))
  expect_null(validate_edg_attr(bn, "foo"))
  expect_null(validate_edg_attr(tg, "lab"))
  
  expect_error(validate_edg_attr(ig, "bad"))
  expect_error(validate_edg_attr(nw, "bad"))
  expect_error(validate_edg_attr(bn, "bad"))
  expect_error(validate_edg_attr(tg, "bad"))
})

