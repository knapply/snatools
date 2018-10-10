context("vertex/edge data frame construction works")

# simple graph data ====
el <- matrix(c(1, 2,
               2, 3,
               # 4, 3, # lower value in .ego column aren't testable... ?
                       # igraph sorts undirected rows at some point
               3, 4,
               1, 4,
               4, 5,
               1, 1, # loop edges
               2, 2,
               2, 3, # parallel edges
               4, 5), 
             ncol = 2L, byrow = TRUE)
storage.mode(el) <- "integer"

ig_attrs <- data.frame(name = paste0("v_", seq_along(unique(c(el)))),
                       lab = letters[seq_along(unique(c(el)))],
                       foo = LETTERS[seq_along(unique(c(el)))],
                       stringsAsFactors = FALSE)

edge_attrs <- data.frame(elab = letters[seq_len(nrow(el))],
                         efoo = LETTERS[seq_len(nrow(el))],
                         stringsAsFactors = FALSE)

names_el <- matrix(ig_attrs$name[el], ncol = 2L)
lab_attr_el <- matrix(ig_attrs$lab[el], ncol = 2L)
foo_attr_el <- matrix(ig_attrs$foo[el], ncol = 2L)

ig_dir <- igraph::graph_from_edgelist(el)
ig_undir <- igraph::graph_from_edgelist(el, directed = FALSE)
igraph::vertex_attr(ig_dir) <- ig_attrs
igraph::edge_attr(ig_dir) <- edge_attrs
igraph::vertex_attr(ig_undir) <- ig_attrs
igraph::edge_attr(ig_undir) <- edge_attrs

tg_dir <- tidygraph::as_tbl_graph(ig_dir)
tg_undir <- tidygraph::as_tbl_graph(ig_undir)

nw_dir <- network::as.network.matrix(el, matrix.type = "edgelist")
nw_undir <- network::as.network.matrix(el, matrix.type = "edgelist", directed = FALSE)

nw_attrs <- ig_attrs
names(nw_attrs)[names(nw_attrs) == "name"] <- "vertex.names"

network::set.vertex.attribute(nw_dir, attrname = names(nw_attrs), value = nw_attrs)
network::set.edge.attribute(nw_dir, names(edge_attrs), value = edge_attrs)
network::set.vertex.attribute(nw_undir, attrname = names(nw_attrs), value = nw_attrs)
network::set.edge.attribute(nw_undir, names(edge_attrs), value = edge_attrs)

vrt_control_tibble <- tibble::tibble(.vrt_id = seq_len(nrow(ig_attrs)),
                                     .vrt_name = ig_attrs$name,
                                     lab = ig_attrs$lab,
                                     foo = ig_attrs$foo)

edg_control_tibble <- tibble::tibble(.edg_id = seq_len(nrow(el)),
                                     .ego = el[, 1],
                                     .alter = el[, 2],
                                     elab = edge_attrs$elab,
                                     efoo = edge_attrs$efoo)

#* vertex data frames ====
test_that("vertex data frame construction works", {
# igraph
  # directed
  expect_identical(
    vrt_as_df(ig_dir), 
    vrt_control_tibble
    )
  # undirected
  expect_identical(
    vrt_as_df(ig_undir),
    vrt_control_tibble
    )
# tidygraph
  # directed
  expect_identical(
    vrt_as_df(tg_dir),
    vrt_control_tibble
    )
  # undirected
  expect_identical(
    vrt_as_df(tg_undir),
    vrt_control_tibble
    )
# network
  # directed
  expect_identical(
    vrt_as_df(nw_dir),
    vrt_control_tibble
    )
  # undirected
  expect_identical(
    vrt_as_df(nw_undir),
    vrt_control_tibble
    )
})

#* vertex data frames ====
test_that("edge data frame construction works", {
# igraph
  # directed
  expect_identical(
    edg_as_df(ig_dir), 
    edg_control_tibble
    )
  # undirected
  expect_identical(
    edg_as_df(ig_undir),
    edg_control_tibble
    )
# tidygraph
  # directed
  expect_identical(
    edg_as_df(tg_dir),
    edg_control_tibble
    )
  # undirected
  expect_identical(
    edg_as_df(tg_undir),
    edg_control_tibble
    )
# network
  # directed
  expect_identical(
    edg_as_df(nw_dir),
    edg_control_tibble
    )
  # undirected
  expect_identical(
    edg_as_df(nw_undir),
    edg_control_tibble
    )
})


# # bipartite graph data ====
# # 2-mode matrix
n_actors <- 6
n_events <- 3
affil_matrix <- matrix(c(1, 0, 1 ,0, 1, 0,
                         0, 1, 0, 1, 0, 1,
                         1, 1, 1, 1, 1, 1),
                       nrow = n_events, byrow = TRUE)
# vertex attributes
ig_bip_attrs <- data.frame(name = c(paste0("actor", seq_len(n_actors)),
                                    paste0("event", seq_len(n_events))),
                           # type = c(rep(TRUE, ncol(affil_matrix)),
                           #          rep(FALSE, nrow(affil_matrix))),
                           type = c(rep(FALSE, ncol(affil_matrix)),
                                    rep(TRUE, nrow(affil_matrix))),
                           foo = LETTERS[seq_len(ncol(affil_matrix) + nrow(affil_matrix))],
                           stringsAsFactors = FALSE)
# decorate matrix
rownames(affil_matrix) <- seq_len(n_events)
colnames(affil_matrix) <- seq(n_events + 1, n_actors + n_events)
# edge attributes
ig_bip_edges <- data.frame(lab = letters[seq_len(sum(affil_matrix))],
                           foo = LETTERS[seq_len(sum(affil_matrix))],
                           stringsAsFactors = FALSE)
# bipartite igraph
ig_bip <- igraph::graph_from_incidence_matrix(affil_matrix ,directed = FALSE)
# sort vertices (TRUE 'type' vertices come first)
ig_bip <- prep_bipartite_igraph(ig_bip)

# decorate igraph
igraph::vertex_attr(ig_bip) <- ig_bip_attrs
igraph::edge_attr(ig_bip) <- ig_bip_edges

ig_bip_attrs <- igraph::as_data_frame(ig_bip, "vertices")
rownames(ig_bip_attrs) <- NULL

# create tidygraph
tg_bip <- tidygraph::as_tbl_graph(ig_bip)

# get edgelist
el_bip <- igraph::as_edgelist(ig_bip, names = FALSE)

type_el_bip <- matrix(igraph::vertex_attr(ig_bip, name = "type")[el_bip],
                      ncol = 2)

# network vertex attributes
nw_bip_attrs <- ig_bip_attrs
names(nw_bip_attrs)[names(nw_bip_attrs) == "name"] <- "vertex.names"
nw_bip_attrs$type <- NULL

# network edges
nw_bip_edges <- ig_bip_edges
nw_bip_edges$from <- NULL
nw_bip_edges$to <- NULL

# create network
nw_bip <- network::network.initialize(n_actors + n_events, directed = FALSE,
                                      bipartite = n_actors)
network::add.edges(nw_bip, tail = el_bip[, 1], head = el_bip[, 2])
# decorate network
network::set.vertex.attribute(nw_bip, names(nw_bip_attrs), # breaks if list/df of length 1 !!
                              value = nw_bip_attrs)

network::set.edge.attribute(nw_bip, names(nw_bip_edges), nw_bip_edges)

ig_bip_vrt_control_tibble <- tibble::tibble(.vrt_id = seq_len(nrow(ig_bip_attrs)),
                                            .vrt_name = ig_bip_attrs$name,
                                            .actor = ig_bip_attrs$type,
                                            foo = ig_bip_attrs$foo)
nw_bip_vrt_control_tibble <- tibble::tibble(.vrt_id = seq_len(nrow(ig_bip_attrs)),
                                            .vrt_name = ig_bip_attrs$name,
                                            .actor = !ig_bip_attrs$type,
                                            foo = ig_bip_attrs$foo)
bip_edg_control_tibble <- tibble::tibble(.edg_id = seq_len(nrow(ig_bip_edges)),
                                         .ego = el_bip[, 1],
                                         .alter = el_bip[, 2],
                                         lab = ig_bip_edges$lab,
                                         foo = ig_bip_edges$foo)
bip_edg_control_tibble$.ego <- as.integer(bip_edg_control_tibble$.ego)
bip_edg_control_tibble$.alter <- as.integer(bip_edg_control_tibble$.alter)

# #* vertex data frames ====
test_that("bipartite vertex data frame construction works", {
# igraph
  # directed
  expect_identical(
    vrt_as_df(ig_bip),
    ig_bip_vrt_control_tibble
    )
  # undirected
  expect_identical(
    vrt_as_df(ig_bip),
    ig_bip_vrt_control_tibble
    )
# tidygraph
  # directed
  expect_identical(
    vrt_as_df(tg_bip),
    ig_bip_vrt_control_tibble
    )
# network
  expect_identical(
    vrt_as_df(nw_bip),
    nw_bip_vrt_control_tibble
    )
})
# 
# #* vertex data frames ====
test_that("bipartite edge data frame construction works", {
# igraph
  expect_identical(
    edg_as_df(ig_bip),
    bip_edg_control_tibble
    )
# tidygraph
  expect_identical(
    edg_as_df(tg_bip),
    bip_edg_control_tibble
    )
# network
  expect_identical(
    edg_as_df(nw_bip),
    bip_edg_control_tibble
    )
})


# delete optional vertex attributes
ig_dir <- igraph::delete_vertex_attr(ig_dir, "foo")
ig_dir <- igraph::delete_vertex_attr(ig_dir, "lab")
ig_undir <- igraph::delete_vertex_attr(ig_undir, "foo")
ig_undir <- igraph::delete_vertex_attr(ig_undir, "lab")

network::delete.vertex.attribute(nw_dir, "foo")
network::delete.vertex.attribute(nw_dir, "lab")
network::delete.vertex.attribute(nw_undir, "foo")
network::delete.vertex.attribute(nw_undir, "lab")

vrt_no_opts_control_tibble <- vrt_control_tibble[, c(".vrt_id", ".vrt_name")]

# optional vertex column handling ====
test_that("handling 0 optional vertex columns works as expected", {
# igraph
  # directed
  expect_identical(
    vrt_as_df(ig_dir),
    vrt_no_opts_control_tibble
  )
  # undirected
  expect_identical(
    vrt_as_df(ig_undir),
    vrt_no_opts_control_tibble
  )
# network
  # directed
  expect_identical(
    vrt_as_df(nw_dir),
    vrt_no_opts_control_tibble
  )
  # undirected
  expect_identical(
    vrt_as_df(nw_undir),
    vrt_no_opts_control_tibble
  )
})

# delete igraph vertex names
ig_dir <- igraph::delete_vertex_attr(ig_dir, "name")
ig_undir <- igraph::delete_vertex_attr(ig_undir, "name")

vrt_no_opts_control_tibble[[".vrt_name"]] <- seq_len(nrow(vrt_no_opts_control_tibble))


test_that("vrt_dfs from igraphs missing names work as expected", {
  # directed
  expect_identical(
    vrt_as_df(ig_dir),
    vrt_no_opts_control_tibble
  )
  # undirected
  expect_identical(
    vrt_as_df(ig_undir),
    vrt_no_opts_control_tibble
  )
})


# delete optional edge attributes

ig_dir <- igraph::delete_edge_attr(ig_dir, "efoo")
ig_dir <- igraph::delete_edge_attr(ig_dir, "elab")
ig_undir <- igraph::delete_edge_attr(ig_undir, "efoo")
ig_undir <- igraph::delete_edge_attr(ig_undir, "elab")

network::delete.edge.attribute(nw_dir, "efoo")
network::delete.edge.attribute(nw_dir, "elab")
network::delete.edge.attribute(nw_undir, "efoo")
network::delete.edge.attribute(nw_undir, "elab")

edg_no_opts_control_tibble <- edg_control_tibble[, c(".edg_id", ".ego", ".alter")]

# optional edge column handling ====
test_that("handling 0 optional edge columns works as expected", {
# igraph
  # directed
  expect_identical(
    edg_as_df(ig_dir),
    edg_no_opts_control_tibble
  )
  # undirected
  expect_identical(
    edg_as_df(ig_undir),
    edg_no_opts_control_tibble
  )
# network
  # directed
  expect_identical(
    edg_as_df(nw_dir),
    edg_no_opts_control_tibble
  )
  # undirected
  expect_identical(
    edg_as_df(nw_undir),
    edg_no_opts_control_tibble
  )
})

# networks w/ nested attributes ====
test_that("nested attributes and networkDynamic objects work", {
  iso_envir1 <- new.env()
  data("windsurfers", package = "networkDynamic", envir = iso_envir1)
  expect_s3_class(vrt_as_df(iso_envir1$windsurfers), "tbl_df")
  
  iso_envir2 <- new.env()
  data(nd_test_nets, package = "networkDynamic", envir = iso_envir2)
  for (i in seq_along(iso_envir2$nd_test_nets[1:10])) {
    # print(class(iso_envir2$nd_test_nets[[i]]))
    print(i)
    expect_s3_class(vrt_as_df(iso_envir2$nd_test_nets[[2]]), "tbl_df")
  }
})

# empty graphs ====
test_that("empty graphs works", {
# network
  nw_empty <- network::network.initialize(n = 0)
  
  expect_s3_class(vrt_as_df(nw_empty), "tbl_df")
  expect_true(is_empty(vrt_as_df(nw_empty)))
  expect_identical(nrow(vrt_as_df(nw_empty)), 0L)
  expect_identical(ncol(vrt_as_df(nw_empty)), 0L)
  
  expect_s3_class(edg_as_df(nw_empty), "tbl_df")
  expect_true(is_empty(edg_as_df(nw_empty)))
  expect_identical(nrow(edg_as_df(nw_empty)), 0L)
  expect_identical(ncol(edg_as_df(nw_empty)), 0L)

# igraph
  ig_empty <- igraph::make_empty_graph(n = 0)
  
  expect_s3_class(vrt_as_df(ig_empty), "tbl_df")
  expect_true(is_empty(vrt_as_df(ig_empty)))
  expect_identical(nrow(vrt_as_df(ig_empty)), 0L)
  expect_identical(ncol(vrt_as_df(ig_empty)), 0L)
  
  expect_s3_class(edg_as_df(ig_empty), "tbl_df")
  expect_true(is_empty(edg_as_df(ig_empty)))
  expect_identical(nrow(edg_as_df(ig_empty)), 0L)
  expect_identical(ncol(edg_as_df(ig_empty)), 0L)
  
})



