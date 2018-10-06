context("graph conversion")

test_that("network objects convert to igraph correctly", {
  iso_env <- new.env()
  
  data("emon", package = "network", envir = iso_env)
  
  nw_bip <- network::network(data.frame(event1 = c(1, 2, 1, 0),
                                        event2 = c(0, 0, 3, 0),
                                        event3 = c(1, 1, 0, 4),
                                        row.names = c("a", "b", "c", "d")), 
                             matrix.type = "bipartite", 
                             ignore.eval = FALSE, names.eval = "pies")
  
  expect_true(
    are_same_graphs(iso_env$emon$Cheyenne, as_igraph(iso_env$emon$Cheyenne))
  )
  expect_true(
    are_same_graphs(iso_env$emon$HurrFrederic, as_igraph(iso_env$emon$HurrFrederic))
  )
  expect_true(
    are_same_graphs(iso_env$emon$LakePomona, as_igraph(iso_env$emon$LakePomona))
  )
  expect_true(
   are_same_graphs(iso_env$emon$MtSi, as_igraph(iso_env$emon$MtSi))
  )
  expect_true(
    are_same_graphs(iso_env$emon$MtStHelens, as_igraph(iso_env$emon$MtStHelens))
  )
  expect_true(
    are_same_graphs(iso_env$emon$Texas, as_igraph(iso_env$emon$Texas))
  )
  expect_true(
    are_same_graphs(iso_env$emon$Wichita, as_igraph(iso_env$emon$Wichita))
  )
  
  expect_true(
    have_same_vert_attrs(nw_bip, as_igraph(nw_bip))
  )
  
 expect_true(
    have_same_vert_attrs(nw_bip, as.igraph(nw_bip))
  )
  
  expect_false(
    are_same_graphs(iso_env$emon$Wichita, as_igraph(iso_env$emon$Cheyenne))
  )
  
  expect_identical(iso_env$emon$Wichita, as_network(iso_env$emon$Wichita))
})

test_that("igraph objects convert to network correctly", {
  iso_env <- new.env()
  data("UKfaculty", package = "igraphdata", envir = iso_env)
  
  n_actors <- 6
  n_events <- 3
  affil_matrix <- matrix(c(1, 0, 1 ,0, 1, 0,
                           0, 1, 0, 1, 0, 1,
                           1, 1, 1, 1, 1, 1),
                         nrow = n_events, byrow = TRUE)
  # vertex attributes
  ig_bip_attrs <- data.frame(name = c(paste0("event", seq_len(n_events)),
                                      paste0("actor", seq_len(n_actors))),
                             type = c(rep(FALSE, nrow(affil_matrix)),
                                      rep(TRUE, ncol(affil_matrix))),
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
  # decorate igraph
  igraph::vertex_attr(ig_bip) <- ig_bip_attrs
  igraph::edge_attr(ig_bip) <- ig_bip_edges
  
  # ig_bip <- prep_bipartite_igraph(ig_bip)
  
  ig_bip_attrs <- igraph::as_data_frame(ig_bip, "vertices")
  
  # decorate igraph
  igraph::vertex_attr(ig_bip) <- ig_bip_attrs
  igraph::edge_attr(ig_bip) <- ig_bip_edges
  
  # create tidygraph
  tg_bip <- tidygraph::as_tbl_graph(ig_bip)
  
  expect_true(
    are_same_graphs(iso_env$UKfaculty, as_network(iso_env$UKfaculty))
  )
  expect_true(
    are_same_graphs(ig_bip, tg_bip)
  )
  expect_true(
    are_same_graphs(ig_bip, as_network(ig_bip))
  )
  
  expect_true(
    are_same_graphs(ig_bip, as.network(tg_bip))
  )
  
  expect_identical(ig_bip, as_igraph(ig_bip))
})


