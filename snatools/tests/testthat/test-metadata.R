context("metadata queries")

# data: parallel edges, loops ====
# directed graphs have isolates
el_mult <- matrix(c(1, 2,
                    2, 3,
                    3, 4,
                    1, 4,
                    4, 5,
                    1, 1, # loop edges
                    2, 2,
                    2, 3, # parallel edges
                    4, 5),
                  ncol = 2L, byrow = TRUE)

ig_dir_mult <- igraph::graph_from_edgelist(el_mult)
ig_dir_mult <- igraph::add_vertices(ig_dir_mult, 1)
ig_undir_mult <- igraph::graph_from_edgelist(el_mult, directed = FALSE)


tg_dir_mult <- tidygraph::as_tbl_graph(ig_dir_mult)
tg_undir_mult <- tidygraph::as_tbl_graph(ig_undir_mult)

nw_dir_mult <- network::as.network.matrix(el_mult, 
                                          matrix.type = "edgelist",
                                          multiple = TRUE)
network::add.vertices(nw_dir_mult, 1)
nw_undir_mult <- network::as.network.matrix(el_mult, 
                                            matrix.type = "edgelist", 
                                            directed = FALSE,
                                            multiple = TRUE)

# data: simple graphs ====
# directed graphs have isolates
el_simp <- unique.matrix(el_mult[el_mult[, 1] != el_mult[, 2], ])

ig_dir_simp <- igraph::graph_from_edgelist(el_simp)
ig_dir_simp <- igraph::add_vertices(ig_dir_simp, 1)
ig_undir_simp <- igraph::graph_from_edgelist(el_simp, directed = FALSE)

tg_dir_simp <- tidygraph::as_tbl_graph(ig_dir_simp)
tg_undir_simp <- tidygraph::as_tbl_graph(ig_undir_simp)

nw_dir_simp <- network::as.network.matrix(el_simp, 
                                          matrix.type = "edgelist",
                                          multiple = TRUE)
network::add.vertices(nw_dir_simp, 1)
nw_undir_simp <- network::as.network.matrix(el_simp, 
                                            matrix.type = "edgelist", 
                                            directed = FALSE,
                                            multiple = TRUE) # treachery

# net_is_directed() ====

test_that("net_is_directed is correct", {
# igraph
  # directed
  expect_true(
    net_is_directed(ig_dir_mult)
    )
  # undirected
  expect_false(
    net_is_directed(ig_undir_mult)
    )
# tidygraph
  # directed
  expect_true(
    net_is_directed(tg_dir_mult)
    )
  # undirected
  expect_false(
    net_is_directed(tg_undir_mult)
    )
# network
  # directed
  expect_true(
    net_is_directed(nw_dir_mult)
    )
  # undirected
  expect_false(
    net_is_directed(nw_undir_mult)
    )
})

# net_is_multiplex() ====
test_that("net_is_multiplex is correct", {
# igraph
  # directed
  expect_true(
    net_is_multiplex(ig_dir_mult)
  )
  expect_false(
    net_is_multiplex(ig_dir_simp)
  )
  # undirected
  expect_true(
    net_is_multiplex(ig_undir_mult)
  )
  expect_false(
    net_is_multiplex(ig_undir_simp)
  )
# tidygraph
  # directed
  expect_true(
    net_is_multiplex(tg_dir_mult)
  )
  expect_false(
    net_is_multiplex(tg_dir_simp)
  )
  # undirected
  expect_true(
    net_is_multiplex(tg_undir_mult)
  )
  expect_false(
    net_is_multiplex(tg_undir_simp)
  )
# network
  # directed
  expect_true(
    net_is_multiplex(nw_dir_mult)
  )
  expect_false(
    net_is_multiplex(nw_dir_simp)
  )
  # undirected
  expect_true(
    net_is_multiplex(nw_undir_mult)
  )
  expect_false(
    net_is_multiplex(nw_undir_simp)
  )
})


# net_has_loops() ====

test_that("net_has_loops is correct", {
# igraph
  # directed
  expect_true(
    net_has_loops(ig_dir_mult)
  )
  expect_false(
    net_has_loops(ig_dir_simp)
  )
  # undirected
  expect_true(
    net_has_loops(ig_undir_mult)
  )
  expect_false(
    net_has_loops(ig_undir_simp)
  )
# tidygraph
  # directed
  expect_true(
    net_has_loops(tg_dir_mult)
  )
  expect_false(
    net_has_loops(tg_dir_simp)
  )
  # undirected
  expect_true(
    net_has_loops(tg_undir_mult)
  )
  expect_false(
    net_has_loops(tg_undir_simp)
  )
# network
  # directed
  expect_true(
    net_has_loops(nw_dir_mult)
  )
  expect_false(
    net_has_loops(nw_dir_simp)
  )
  # undirected
  expect_true(
    net_has_loops(nw_undir_mult)
  )
  expect_false(
    net_has_loops(nw_undir_simp)
  )
})

# net_has_isolates() ====
# directed graphs should have the isolates

test_that("net_has_isolates is correct", {
# igraph
  # directed
  expect_true(
    net_has_isolates(ig_dir_mult)
  )
  expect_true(
    net_has_isolates(ig_dir_simp)
  )
  # undirected
  expect_false(
    net_has_isolates(ig_undir_mult)
  )
  expect_false(
    net_has_isolates(ig_undir_simp)
  )
# tidygraph
  # directed
  expect_true(
    net_has_isolates(tg_dir_mult)
  )
  expect_true(
    net_has_isolates(tg_dir_simp)
  )
  # undirected
  expect_false(
    net_has_isolates(tg_undir_mult)
  )
  expect_false(
    net_has_isolates(tg_undir_simp)
  )
# network
  # directed
  expect_true(
    net_has_isolates(nw_dir_mult)
  )
  expect_true(
    net_has_isolates(nw_dir_simp)
  )
  # undirected
  expect_false(
    net_has_isolates(nw_undir_mult)
  )
  expect_false(
    net_has_isolates(nw_undir_simp)
  )
})








# net_count_vertices() ====
# directed graphs should have 6
# undirected graphs should have 5

test_that("net_count_vertices is correct", {
# igraph
  # directed
  expect_equal(
    net_count_vertices(ig_dir_mult),
    6
  )
  expect_equal(
    net_count_vertices(ig_dir_simp),
    6
  )
  # undirected
  expect_equal(
    net_count_vertices(ig_undir_mult),
    5
  )
  expect_equal(
    net_count_vertices(ig_undir_simp),
    5
  )
# tidygraph
  # directed
  expect_equal(
    net_count_vertices(tg_dir_mult),
    6
  )
  expect_equal(
    net_count_vertices(tg_dir_simp),
    6
  )
  # undirected
  expect_equal(
    net_count_vertices(tg_undir_mult),
    5
  )
  expect_equal(
    net_count_vertices(tg_undir_simp),
    5
  )
# network
  # directed
  expect_equal(
    net_count_vertices(nw_dir_mult),
    6
  )
  expect_equal(
    net_count_vertices(nw_dir_simp),
    6
  )
  # undirected
  expect_equal(
    net_count_vertices(nw_undir_mult),
    5
  )
  expect_equal(
    net_count_vertices(nw_undir_simp),
    5
  )
})


# net_count_edges() ====
# multigraphs should have 9
# simple graphs should have 5

test_that("net_count_edges is correct", {
# igraph
  # directed
  expect_equal(
    net_count_edges(ig_dir_mult),
    9
  )
  expect_equal(
    net_count_edges(ig_dir_simp),
    5
  )
  # undirected
  expect_equal(
    net_count_edges(ig_undir_mult),
    9
  )
  expect_equal(
    net_count_edges(ig_undir_simp),
    5
  )
# tidygraph
  # directed
  expect_equal(
    net_count_edges(tg_dir_mult),
    9
  )
  expect_equal(
    net_count_edges(tg_dir_simp),
    5
  )
  # undirected
  expect_equal(
    net_count_vertices(tg_undir_mult),
    5
  )
  expect_equal(
    net_count_vertices(tg_undir_simp),
    5
  )
# network
  # directed
  expect_equal(
    net_count_edges(nw_dir_mult),
    9
  )
  expect_equal(
    net_count_edges(nw_dir_simp),
    5
  )
  # undirected
  expect_equal(
    net_count_edges(nw_undir_mult),
    9
  )
  expect_equal(
    net_count_edges(nw_undir_simp),
    5
  )
})


# bipartite graph data ====
n_actors <- 6
n_events <- 3
affil_matrix <- matrix(c(1, 0, 1 ,0, 1, 0,
                         0, 1, 0, 1, 0, 1,
                         1, 1, 1, 1, 1, 1),
                       nrow = n_events, byrow = TRUE)

ig_bip <- igraph::graph_from_incidence_matrix(affil_matrix)
tg_bip <- tidygraph::as_tbl_graph(ig_bip)
nw_bip <- network::as.network.matrix(t(affil_matrix), 
                                     bipartite = n_actors)

# net_is_bipartite ====
test_that("net_is_bipartite  works", {
# igraph
  expect_true(
    net_is_bipartite(ig_bip)
  )
  expect_false(
    net_is_bipartite(ig_dir_mult)
  )
# tidygraph
  expect_true(
    net_is_bipartite(tg_bip)
  )
  expect_false(
    net_is_bipartite(tg_dir_mult)
  )
# network
  expect_true(
    net_is_bipartite(nw_bip)
  )
  expect_false(
    net_is_bipartite(nw_dir_mult)
  )
})

# net_count_actors ====
# should be 6 for all bipartite graphs, errors for non-bipartites
test_that("net_count_actors works", {
# igraph
  expect_equal(
    net_count_actors(ig_bip),
    6
  )
  expect_error(
    net_count_actors(ig_dir_mult)
  )
# tidygraph
  expect_equal(
    net_count_actors(tg_bip),
    6
  )
  expect_error(
    net_count_actors(tg_dir_mult)
  )
# network
  expect_equal(
    net_count_actors(nw_bip),
    6
  )
  expect_error(
    net_count_actors(nw_dir_mult)
  )
})
