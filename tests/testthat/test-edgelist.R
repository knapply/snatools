context("edgelist construction works")

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
names_el <- `colnames<-`(matrix(ig_attrs$name[el], ncol = 2L),
                         c(".ego", ".alter"))
lab_attr_el <- `colnames<-`(matrix(ig_attrs$lab[el], ncol = 2L), 
                            c(".ego", ".alter"))
foo_attr_el <- `colnames<-`(matrix(ig_attrs$foo[el], ncol = 2L),
                            c(".ego", ".alter"))

ig_dir <- igraph::graph_from_edgelist(el)
ig_undir <- igraph::graph_from_edgelist(el, directed = FALSE)
igraph::vertex_attr(ig_dir) <- ig_attrs
igraph::vertex_attr(ig_undir) <- ig_attrs

tg_dir <- tidygraph::as_tbl_graph(ig_dir)
tg_undir <- tidygraph::as_tbl_graph(ig_undir)

nw_dir <- network::as.network.matrix(el, matrix.type = "edgelist")
nw_undir <- network::as.network.matrix(el, matrix.type = "edgelist", directed = FALSE)

nw_attrs <- ig_attrs
names(nw_attrs)[names(nw_attrs) == "name"] <- "vertex.names"

network::set.vertex.attribute(nw_dir, attrname = names(nw_attrs), value = nw_attrs)
network::set.vertex.attribute(nw_undir, attrname = names(nw_attrs), value = nw_attrs)

# helper functions ====
# remove non-matrix attribtues from `edgelist` class objects and the edgelists that
# {network} creates
strip_attrs <- function(x) {
  stopifnot(is.matrix(x))
  all_attrs <- attributes(x)
  for (attr_name in names(all_attrs)) {
    if (attr_name != "dim") {
      attr(x, attr_name) <- NULL
    }
  }
  x
}

# fill matrices with an attribute
fill_mat <- function(x, fill) {
  matrix(fill[x], ncol = 2L)
}

# do it all
strip_fill <- function(x, fill) {
  `colnames<-`(fill_mat(strip_attrs(x), fill), c(".ego", ".alter"))
}

# convert storage type from double to integer
ig_as_int_el <- function(x, use_names = FALSE) {
  `storage.mode<-`(igraph::as_edgelist(x, names = use_names), "integer")
}
ig_as_int_el_named <- function(x) {
  `colnames<-`(ig_as_int_el(x), c(".ego", ".alter"))
}
ig_as_named_el <- function(x) {
  `colnames<-`(igraph::as_edgelist(x, names = TRUE), c(".ego", ".alter"))
}
nw_as_el <- function(x) {
  `colnames<-`(strip_attrs(network::as.matrix.network.edgelist(x)), 
               c(".ego", ".alter"))
}
nw_as_el_named <- function(x) {
  `colnames<-`(nw_as_el(x), c(".ego", ".alter"))
}

#* fill w/ vertex indices ====
test_that("raw edgelists build properly", {
# igraph
  # directed
  expect_identical(
    get_el(ig_dir),
    ig_as_int_el(ig_dir)
    )
  expect_identical(
    rep_as_edgelist(ig_dir, use_names = FALSE),
    ig_as_int_el_named(ig_dir)
    )
  # undirected
  expect_identical(
    get_el(ig_undir),
    ig_as_int_el(ig_undir)
    )
  expect_identical(
    rep_as_edgelist(ig_undir, use_names = FALSE),
    ig_as_int_el_named(ig_undir)
    )
# tidygraph
  # directed
  expect_identical(
    get_el(tg_dir),
    ig_as_int_el(ig_dir)
    )
  expect_identical(
    rep_as_edgelist(tg_dir, use_names = FALSE),
    ig_as_int_el_named(ig_dir)
    )
  # undirected
  expect_identical(
    get_el(tg_undir),
    ig_as_int_el(ig_undir)
    )
  expect_identical(
    rep_as_edgelist(tg_undir, use_names = FALSE),
    ig_as_int_el_named(ig_undir)
    )
# network
  # directed
  expect_identical(
    get_el(nw_dir),
    strip_attrs(network::as.matrix.network.edgelist(nw_dir))
    )
  expect_identical(
    rep_as_edgelist(ig_dir, use_names = FALSE),
    nw_as_el_named(nw_dir)
    )
  # undirected
  expect_identical(
    get_el(nw_undir),
    strip_attrs(network::as.matrix.network.edgelist(nw_undir))
    )
  expect_identical(
    rep_as_edgelist(nw_undir, use_names = FALSE),
    nw_as_el_named(nw_undir)
    )
})

#* fill w/ vertex names ====
test_that("edgelist objects: use_names = TRUE", {
# igraph
  # directed
  expect_identical(
    rep_as_edgelist(ig_dir),
    ig_as_named_el(ig_dir)
    )
  # undirected
  expect_identical(
    rep_as_edgelist(ig_undir),
    ig_as_named_el(ig_undir)
    )
# tidygraph
  # directed
  expect_identical(
    rep_as_edgelist(tg_dir),
    ig_as_named_el(ig_dir)
    )
  # undirected
  expect_identical(
    rep_as_edgelist(tg_undir),
    ig_as_named_el(ig_undir)
    )
# network
  # directed
  expect_identical(
    rep_as_edgelist(nw_dir),
    strip_fill(nw_as_el(nw_dir), nw_attrs$vertex.names)
    )
  # undirected
  expect_identical(
    rep_as_edgelist(nw_undir),
    strip_fill(nw_as_el(nw_undir), nw_attrs$vertex.names)
    )
})

#* fill with vertex attrs ====
test_that("edgelist objects: vrt_attr = 'foo'", {
# igraph
  # directed
  expect_identical(
    rep_as_edgelist(ig_dir, vrt_attr = "foo"),
    foo_attr_el
    )
  # undirected
  expect_identical(
    rep_as_edgelist(ig_undir, vrt_attr = "foo"),
    foo_attr_el
    )
# tidygraph
  # directed
    expect_identical(
    rep_as_edgelist(tg_dir, vrt_attr = "foo"),
    foo_attr_el
    )
  # undirected
  expect_identical(
    rep_as_edgelist(tg_undir, vrt_attr = "foo"),
    foo_attr_el
    )
# network
  # directed
  expect_identical(
    rep_as_edgelist(nw_dir, vrt_attr = "foo"),
    foo_attr_el
    )
  # undirected
  expect_identical(
    rep_as_edgelist(nw_undir, vrt_attr = "foo"),
    foo_attr_el
    )
})


# bipartite graph data ====
# 2-mode matrix
n_actors <- 6
n_events <- 3
affil_matrix <- matrix(c(1, 0, 1 ,0, 1, 0,
                         0, 1, 0, 1, 0, 1,
                         1, 1, 1, 1, 1, 1),
                       nrow = n_events, byrow = TRUE)
# vertex attributes
ig_bip_attrs <- data.frame(name = c(paste0("actor", seq_len(n_actors)),
                                    paste0("event", seq_len(n_events))),
                           type = c(rep(TRUE, ncol(affil_matrix)),
                                    rep(FALSE, nrow(affil_matrix))),
                           stringsAsFactors = FALSE)
# decorate matrix
rownames(affil_matrix) <- seq_len(n_events)
colnames(affil_matrix) <- seq(n_events + 1, n_actors + n_events)
# edge attributes
ig_bip_edges <- data.frame(lab = letters[seq_len(sum(affil_matrix))],
                           foo = LETTERS[seq_len(sum(affil_matrix))],
                           stringsAsFactors = FALSE)
# bipartite igraph
ig_bip <- igraph::graph_from_incidence_matrix(affil_matrix, directed = FALSE)
# sort vertices (TRUE 'type' vertices come first)
ig_bip <- prep_bipartite_igraph(ig_bip)

ig_bip_attrs <- igraph::as_data_frame(ig_bip, "vertices")

# decorate igraph
igraph::vertex_attr(ig_bip) <- ig_bip_attrs
igraph::edge_attr(ig_bip) <- ig_bip_edges

# create tidygraph
tg_bip <- tidygraph::as_tbl_graph(ig_bip)

# get edgelist
el_bip <- igraph::as_edgelist(ig_bip, names = FALSE)

type_el_bip <- matrix(igraph::vertex_attr(ig_bip, name = "type")[el_bip],
                      ncol = 2)

# network vertex attributes
nw_bip_attrs <- ig_bip_attrs
names(nw_bip_attrs)[names(nw_bip_attrs) == "name"] <- "vertex.names"
# network edges
nw_bip_edges <- ig_bip_edges
nw_bip_edges$from <- NULL
nw_bip_edges$to <- NULL

# create network
nw_bip <- network::network.initialize(n_actors + n_events, directed = FALSE,
                                      bipartite = n_actors)
network::add.edges(nw_bip, tail = el_bip[, 1], head = el_bip[, 2])
# decorate network
network::set.vertex.attribute(nw_bip, names(nw_bip_attrs), nw_bip_attrs)
network::set.edge.attribute(nw_bip, names(nw_bip_attrs), nw_bip_attrs)


#* fill w/ vertex indices ====
test_that("raw bipartite edgelists build properly", {
# igraph
  expect_identical(
    get_el(ig_bip),
    ig_as_int_el(ig_bip)
    )
# tidygraph
  expect_identical(
    get_el(tg_bip),
    ig_as_int_el(ig_bip)
    )
# network
  expect_identical(
    get_el(nw_bip),
    strip_attrs(network::as.matrix.network.edgelist(nw_bip))
    )
})

# undirected networks from edgelists with unordered rows ====
# test_that("undirected edgelists with unsorted rows sort correctly", {
#   unsorted_el <- matrix(c(2, 1,
#                           3, 2,
#                           4, 5,
#                           4, 1,
#                           5, 2), ncol = 2, byrow = TRUE)
#   sorted_el <- matrix(c(1, 2,
#                         2, 3,
#                         4, 5,
#                         1, 4,
#                         2, 5), ncol = 2, byrow = TRUE)
#
#   nw_from_unsorted_el <- network::as.network.matrix(unsorted_el,
#                                                     matrix.type = "edgelist",
#                                                     directed = TRUE)
#
#   expect_identical(strip_attrs(rep_as_edgelist(nw_from_unsorted_el)),
#                    sorted_el)
# })
