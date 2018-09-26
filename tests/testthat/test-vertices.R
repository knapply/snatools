# simple graph data ====
el <- matrix(c(1, 2,
               2, 3,
               3, 4,
               1, 4,
               4, 5), ncol = 2L, byrow = TRUE)
ig_attrs <- data.frame(name = letters[seq_along(unique(c(el)))],
                       foo = LETTERS[seq_along(unique(c(el)))],
                       stringsAsFactors = FALSE)
nw_attrs <- ig_attrs
names(nw_attrs)[names(nw_attrs) == "name"] <- "vertex.names"

if (!identical(ig_attrs[[1]], nw_attrs[[1]]) || !identical(ig_attrs[[2]], nw_attrs[[2]])) {
  stop("prepared igraph attributes don't match prepared network attributes.",
       call. = FALSE)
}

ig <- igraph::graph_from_edgelist(el)
igraph::vertex_attr(ig) <- ig_attrs

nw <- network::as.network.matrix(el, matrix.type = "edgelist")
network::set.vertex.attribute(nw, attrname = names(nw_attrs), value = nw_attrs)

tg <- tidygraph::as_tbl_graph(ig)

if (as_bridge_net(ig) %==% as_bridge_net(nw)) {
  bn <- as_bridge_net(ig)
  } else {
    stop("ig and nw don't match... something went wrong with as_bridge_net()",
         call. = FALSE)
}

vert_df <- data.frame(.name = ig_attrs$name,
                      foo = ig_attrs$foo, stringsAsFactors = FALSE)

#_ ====
# bipartite graph data ====
n_actors <- 6
n_events <- 3
affil_matrix <- matrix(c(1, 0, 1 ,0, 1, 0,
                         0, 1, 0, 1, 0, 1,
                         1, 1, 1, 1, 1, 1),
                       nrow = n_events, byrow = TRUE)
rownames(affil_matrix) <- seq_len(n_events)
colnames(affil_matrix) <- seq(n_events + 1, n_actors + n_events)
ig_bip_attrs <- data.frame(name = c(paste0("actor", seq_len(n_actors)),
                                    paste0("event", seq_len(n_events))),
                           type = c(rep(TRUE, ncol(affil_matrix)),
                                    rep(FALSE, nrow(affil_matrix))),
                           stringsAsFactors = FALSE)

ig_bip <- igraph::graph_from_incidence_matrix(affil_matrix,directed = FALSE)
ig_bip <- prep_bipartite_igraph(ig_bip)

igraph::vertex_attr(ig_bip) <- ig_bip_attrs

el_bip <- igraph::as_edgelist(ig_bip, names = FALSE)

nw_bip_attrs <- ig_bip_attrs
names(nw_bip_attrs)[names(nw_bip_attrs) == "name"] <- "vertex.names"
names(nw_bip_attrs)[names(nw_bip_attrs) == "type"] <- ".actor"

nw_bip <- network::network.initialize(n_actors + n_events, directed = FALSE, 
                                      bipartite = n_actors)
network::add.edges(nw_bip, tail = el_bip[, 1], head = el_bip[, 2])
network::set.vertex.attribute(nw_bip, names(nw_bip_attrs), nw_bip_attrs)

# if (as_bridge_net(ig_bip) %==% as_bridge_net(nw_bip)) {
if (identical(as_bridge_net(ig_bip), as_bridge_net(nw_bip))) {
  bn_bip <- as_bridge_net(ig_bip)
  } else {
    stop("ig_bip and nw_bip don't match... something went wrong with as_bridge_net()",
         "the only way you can manually match bipartite graph seems to be building an 
          igraph then running prep_bipartite_igraph() and extracting the edge list to add 
          to an empty network.",
         call. = FALSE)
}

tg_bip <- tidygraph::as_tbl_graph(ig_bip)

bip_vert_df <- data.frame(.name = ig_bip_attrs$name,
                          .actor = ig_bip_attrs$type, 
                          stringsAsFactors = FALSE)

context("vertices, extraction, deletion, filtering")
#_ ====
# Extract Vertices/Attributes ====
test_that("vertices and their attributes can be extracted consistently", {
#_ vrt_attr_names() ====
#** igraph ====
  expect_identical(vrt_attr_names(ig)[[1]], "name")
  expect_identical(vrt_attr_names(ig)[[2]], "foo")
  
  expect_identical(vrt_attr_names(ig_bip)[[1]], "name")
  expect_identical(vrt_attr_names(ig_bip)[[2]], "type")
#** network ====
  expect_identical(vrt_attr_names(nw)[[1]], "vertex.names")
  expect_identical(vrt_attr_names(nw)[[2]], "foo")
  
  expect_identical(vrt_attr_names(nw_bip)[[1]], "vertex.names")
  expect_identical(vrt_attr_names(nw_bip)[[2]], ".actor")
#** bridge_net ====
  expect_identical(vrt_attr_names(bn)[[1]], ".name")
  expect_identical(vrt_attr_names(bn)[[2]], "foo")
  
  expect_identical(vrt_attr_names(bn_bip)[[1]], ".name")
  expect_identical(vrt_attr_names(bn_bip)[[2]], ".actor")
#** tbl_graph ====
  expect_identical(vrt_attr_names(tg)[[1]], "name")
  expect_identical(vrt_attr_names(tg)[[2]], "foo")
  
  expect_identical(vrt_attr_names(tg_bip)[[1]], "name")
  expect_identical(vrt_attr_names(tg_bip)[[2]], "type")
  
#_ vrt_get_attr() ====
#** igraph ====
  expect_identical(vrt_get_attr(ig, "name"), ig_attrs$name)
  expect_identical(vrt_get_attr(ig, "foo"), ig_attrs$foo)
  
  expect_identical(vrt_get_attr(ig_bip, "name"), ig_bip_attrs$name)
  expect_identical(vrt_get_attr(ig_bip, "type"), ig_bip_attrs$type)
#** network ====
  expect_identical(vrt_get_attr(nw, "vertex.names"), nw_attrs$vertex.names)
  expect_identical(vrt_get_attr(nw, "foo"), nw_attrs$foo)
  
  expect_identical(vrt_get_attr(nw_bip, "vertex.names"), nw_bip_attrs$vertex.names)
  expect_identical(vrt_get_attr(nw_bip, ".actor"), nw_bip_attrs$.actor)
#** bridge_net ====
  expect_identical(vrt_get_attr(bn, ".name"), ig_attrs$name)
  expect_identical(vrt_get_attr(bn, "foo"), ig_attrs$foo)
  
  expect_identical(vrt_get_attr(bn_bip, ".name"), nw_bip_attrs$vertex.names)
  expect_identical(vrt_get_attr(bn_bip, ".name"), ig_bip_attrs$name)
  expect_identical(vrt_get_attr(bn_bip, ".actor"), nw_bip_attrs$.actor)
  expect_identical(vrt_get_attr(bn_bip, ".actor"), ig_bip_attrs$type)
#** tbl_graph ====
  expect_identical(vrt_get_attr(tg, "name"), ig_attrs$name)
  expect_identical(vrt_get_attr(tg, "foo"), ig_attrs$foo)
  
  expect_identical(vrt_get_attr(tg_bip, "name"), ig_bip_attrs$name)
  expect_identical(vrt_get_attr(tg_bip, "type"), ig_bip_attrs$type)

#_ vrt_to_df() ====
#** igraph ====
  expect_identical(vrt_to_df(ig, leave_raw = TRUE), ig_attrs)
  
  expect_identical(vrt_to_df(ig_bip, leave_raw = TRUE), ig_bip_attrs)
#** network ====
  expect_identical(vrt_to_df(nw, leave_raw = TRUE),nw_attrs)
  
  expect_identical(vrt_to_df(nw_bip, leave_raw = TRUE), nw_bip_attrs)
#** bridge_net ====
  expect_identical(vrt_to_df(bn),
                   as_vertex_data_frame(vrt_to_df(ig, leave_raw = TRUE)))
  
  expect_identical(vrt_to_df(bn_bip),
                   as_vertex_data_frame(vrt_to_df(nw_bip, leave_raw = TRUE)))
#** tbl_graph ====
  expect_identical(vrt_to_df(tg, leave_raw = TRUE), ig_attrs)
  
  expect_identical(vrt_to_df(tg_bip, leave_raw = TRUE), ig_bip_attrs)
})

# _ ====
# Delete Vertex Attributes ====
test_that("vertex attributes can be deleted consistently", {
#_ vrt_delete_attr() ====
#** igraph ====
  expect_identical(vrt_to_df(vrt_delete_attr(ig, "foo"), leave_raw = TRUE), 
                   ig_attrs[, "name", drop = FALSE])
  
  expect_identical(vrt_to_df(vrt_delete_attr(ig_bip, "type"), leave_raw = TRUE), 
                   ig_bip_attrs[, "name", drop = FALSE])
#** network ====
  expect_identical(vrt_to_df(vrt_delete_attr(nw, "foo"), leave_raw = TRUE), 
                   nw_attrs[, "vertex.names", drop = FALSE])
#** bridge_net ====
  expect_identical(vrt_to_df(vrt_delete_attr(bn, "foo")), 
                   vrt_to_df(vrt_delete_attr(ig, "foo")))
  expect_identical(vrt_to_df(vrt_delete_attr(bn, "foo")), 
                   vrt_to_df(vrt_delete_attr(nw, "foo")))
  
  expect_identical(vrt_to_df(vrt_delete_attr(bn_bip, ".actor")), 
                   vrt_to_df(vrt_delete_attr(ig_bip, "type")))
  # shouldn't delete .actor attribute
  # expect_identical(vrt_to_df(vrt_delete_attr(bn_bip, ".actor"))$.name, 
                   # vrt_to_df(vrt_delete_attr(nw_bip, ".actor"))$.name)
#** tbl_graph ====
  expect_identical(vrt_to_df(vrt_delete_attr(tg, "foo"), leave_raw = TRUE), 
                   ig_attrs[, "name", drop = FALSE])
})

ig_attrs_dt <- data.table::as.data.table(ig_attrs)


# _ ====
# Filter Vertices ====
test_that("vertices can be filtered consistently", {
#_ vrt_filter() ====
#** igraph ====
  expect_identical(vrt_to_df(vrt_filter(ig, foo %in% c("A", "B")), leave_raw = TRUE),
                   ig_attrs[ig_attrs$foo %in% c("A", "B"), ])
  expect_identical(vrt_to_df(vrt_filter(ig, foo %in% c("A", "B"))),
                   as_vertex_data_frame(ig_attrs[ig_attrs$foo %in% c("A", "B"), ]))
#** network ====
  expect_identical(vrt_to_df(vrt_filter(nw, foo %in% c("A", "B")), leave_raw = TRUE),
                   nw_attrs[nw_attrs$foo %in% c("A", "B"), ])
  expect_identical(vrt_to_df(vrt_filter(nw, foo %in% c("A", "B"))),
                   as_vertex_data_frame(nw_attrs[nw_attrs$foo %in% c("A", "B"), ]))
#** bridge_net ====
  expect_identical(vrt_to_df(vrt_filter(ig, foo %in% c("A", "B"))),
                   as_vertex_data_frame(nw_attrs[nw_attrs$foo %in% c("A", "B"), ]))
#_ vrt_filter_se() ====
#** igraph ====
  expect_identical(
    vrt_to_df(vrt_filter_se(ig, "foo", `%in%`, c("A", "B")), leave_raw = TRUE),
    ig_attrs[ig_attrs$foo %in% c("A", "B"), ]
    )
  expect_identical(
    vrt_to_df(vrt_filter_se(ig, "foo", `%in%`, c("A", "B"))),
    as_vertex_data_frame(ig_attrs[ig_attrs$foo %in% c("A", "B"), ])
    )
#** network ====
  expect_identical(
    vrt_to_df(vrt_filter_se(nw, "foo", `%in%`, c("A", "B")), leave_raw = TRUE),
    nw_attrs[nw_attrs$foo %in% c("A", "B"), ]
    )
  expect_identical(
    vrt_to_df(vrt_filter_se(nw, "foo", `%in%`, c("A", "B"))),
    as_vertex_data_frame(nw_attrs[nw_attrs$foo %in% c("A", "B"), ])
    )
#** bridge_net ====
  expect_identical(
    vrt_to_df(vrt_filter_se(ig, "foo", `%in%`, c("A", "B"))),
    as_vertex_data_frame(nw_attrs[nw_attrs$foo %in% c("A", "B"), ])
    )
})

#_ ====
# Convert `vertex_data_frame`s ====
test_that("`vertex_data_frame`s convert to `data.frame`s consistently", {
#** igraph ====
  expect_identical(as.data.frame.vertex_data_frame(vrt_to_df(ig)), vert_df)
  
  expect_identical(as.data.frame.vertex_data_frame(vrt_to_df(ig_bip)), bip_vert_df)
#** network ====
  expect_identical(as.data.frame.vertex_data_frame(vrt_to_df(nw)), vert_df)
  
  expect_identical(as.data.frame.vertex_data_frame(vrt_to_df(nw_bip)), bip_vert_df)
#** bridge_net ====
  expect_identical(as.data.frame.vertex_data_frame(vrt_to_df(bn)), vert_df)
  
  expect_identical(as.data.frame.vertex_data_frame(vrt_to_df(bn_bip)), bip_vert_df)
#** tbl_graph ====
  expect_identical(as.data.frame.vertex_data_frame(vrt_to_df(tg)), vert_df)
  
  expect_identical(as.data.frame.vertex_data_frame(vrt_to_df(tg_bip)), bip_vert_df)
})
  
#_ ====
# Print `vertex_data_frame`s ====
test_that("`vertex_data_frame`s print without error", {
#** igraph ====
  expect_output(print.vertex_data_frame(vrt_to_df(ig)))
  
  expect_output(print.vertex_data_frame(vrt_to_df(ig_bip)))
#** network ====
  expect_output(print.vertex_data_frame(vrt_to_df(nw)))
  
  expect_output(print.vertex_data_frame(vrt_to_df(nw_bip)))
#** bridge_net ====
  expect_output(print.vertex_data_frame(vrt_to_df(bn)))
  
  expect_output(print.vertex_data_frame(vrt_to_df(bn_bip)))
#** tbl_graph ====
  expect_output(print.vertex_data_frame(vrt_to_df(tg)))
  
  expect_output(print.vertex_data_frame(vrt_to_df(tg_bip)))
})

#_ ====
# `as_vertex_data_frame`  ====
test_that("`as_vertex_data_frame` fails on lists and matrices", {
  expect_error(as_vertex_data_frame(as.list(ig_attrs)))
  expect_error(as_vertex_data_frame(as.matrix(ig_attrs)))
})

#_ ====
# Validating Vertex Attributes ====
test_that("validating vertex attributes works consistently", {
  expect_true(is_valid_vrt_attr(ig, "name"))
  expect_true(is_valid_vrt_attr(nw, "vertex.names"))
  expect_true(is_valid_vrt_attr(bn, ".name"))
  expect_true(is_valid_vrt_attr(tg, "name"))
  
  expect_false(is_valid_vrt_attr(ig, "names"))
  expect_false(is_valid_vrt_attr(nw, "vertex.name"))
  expect_false(is_valid_vrt_attr(bn, ".names"))
  expect_false(is_valid_vrt_attr(tg, "names"))
  
  expect_null(validate_vrt_attr(ig, "foo"))
  expect_null(validate_vrt_attr(nw, "foo"))
  expect_null(validate_vrt_attr(bn, "foo"))
  expect_null(validate_vrt_attr(tg, "foo"))
  
  expect_error(validate_vrt_attr(ig, "bad"))
  expect_error(validate_vrt_attr(nw, "bad"))
  expect_error(validate_vrt_attr(bn, "bad"))
  expect_error(validate_vrt_attr(tg, "bad"))
})

