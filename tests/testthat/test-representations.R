ig_undir <- snatools:::build_test_graph("ig", n = 10, direct = FALSE)
ig_dir <- snatools:::build_test_graph("ig", n = 10, direct = TRUE)

nw_undir <- snatools:::build_test_graph("nw", n = 10, direct = FALSE)
nw_dir <- snatools:::build_test_graph("nw", n = 10, direct = TRUE)

# edge lists ====
as_el <- function(x, use_names = FALSE) {
  if(class(x) == "igraph") {
    out <- igraph::as_edgelist(x, names = use_names)
    if(igraph::is_directed(x)) {
      colnames(out) <- c("from", "to")
    } else {
      colnames(out) <- c("vert1", "vert2")
    }
  }
  if(class(x) == "network") {
    out <- network::as.matrix.network.edgelist(x)
    attr(out, "n") <- NULL
    vrt_names <- attr(out, "vnames")
    attr(out, "vnames") <- NULL
    if(use_names) {
      out <- matrix(vrt_names[out], ncol = 2)
    }
    if(network::is.directed(x)) {
      colnames(out) <- c("from", "to")
      } else {
        temp <- out
        out[, 1] <- temp[, 2]
        out[, 2] <- temp[, 1]
        colnames(out) <- c("vert1", "vert2")
      }
    if(is.integer(out)) {
      out <- apply(out, 2, as.double)
    }
  }
  out
}

#* undirected edge lists ====
context("Undirected Edge Lists")
#** igraph undirected ====
test_that("`ig_undir %>% rep_edgelist()
          %==%
          igraph::as_edgelist(res$success, names = FALSE)`
          ", {
  expect_true(rep_edgelist(ig_undir) %==% as_el(ig_undir, use_names = FALSE))
})
#** network undirected ====
test_that("`nw_undir %>% rep_edgelist()
          %==%
          network::as.matrix.network.edgelist(nw_undir)`
          ", {
  expect_true(rep_edgelist(nw_undir) %==% as_el(nw_undir, use_names = FALSE))
})
#** igraph undirected w/ names ====
test_that("`ig_undir %>% rep_edgelist(use_names = TRUE)
          %==%
          igraph::as_edgelist(ig_undir, names = TRUE)`
          ", {
  expect_true(rep_edgelist(ig_undir, use_names = TRUE) %==% as_el(ig_undir, use_names = TRUE))
})
#** network undirected w/ names ====
test_that("`nw_undir %>% rep_edgelist(use_names = TRUE)
          %==%
          network::as.matrix.network.edgelist()` w/ names manually replaced
          ", {
  expect_true(rep_edgelist(nw_undir, use_names = TRUE) %==% as_el(nw_undir, use_names = TRUE))
})
#** igraph undirected w/ conversion ====
test_that("`ig_undir %>% rep_edgelist()
          %==%
          ig_undir %>% as_network() %>% rep_edgelist()`", {
  expect_true(rep_edgelist(ig_undir) %==% rep_edgelist(as_network(ig_undir)))
})
#** network undirected w/ conversion ====
test_that("nw_undir` %>% rep_edgelist()
          %==%
          nw_undir %>% as_igraph() %>% rep_edgelist()`", {
  expect_true(rep_edgelist(nw_undir) %==% rep_edgelist(as_igraph(nw_undir)))
})
#** igraph undirected w/ conversion + names
test_that("`ig_undir %>% rep_edgelist(use_names = TRUE)
          %==%
          igraph %>% as_network() %>% as_igraph() %>%  rep_edgelist(use_names = TRUE)`", {
  expect_true(rep_edgelist(ig_undir, use_names = TRUE) %==% rep_edgelist(as_igraph(as_network(ig_undir)), use_names = TRUE))
})
#** network undirected w/ round-tripe conversion + names ====
test_that("`nw_undir %>% rep_edgelist(use_names = TRUE)
          %==%
          network %>% as_igraph() %>% as_network() %>% rep_edgelist(use_names = TRUE)`", {
  expect_true(rep_edgelist(nw_undir, use_names = TRUE) %==% rep_edgelist(as_network(as_igraph(nw_undir)), use_names = TRUE))
})
#
#
#
#
#* directed edge lists ====
context("Directed Edge Lists")
#** igraph directed ====
test_that("`ig_dir %>% rep_edgelist()
          %==%
          igraph::as_edgelist(ig_dir, names = FALSE)`
          ", {
  expect_true(rep_edgelist(ig_dir) %==% as_el(ig_dir, use_names = FALSE))
})
#** network directed ====
test_that("`nw_dir %>% rep_edgelist()
          %==%
          network::as.matrix.network.edgelist(nw_dir)`
          ", {
  expect_true(rep_edgelist(nw_dir) %==% as_el(nw_dir, use_names = FALSE))
})
#** igraph directed w/ names ====
test_that("`ig_dir %>% rep_edgelist(use_names = TRUE)
          %==%
          igraph::as_edgelist(ig_dir, names = TRUE)`
          ", {
  expect_true(rep_edgelist(ig_dir, use_names = TRUE) %==% as_el(ig_dir, use_names = TRUE))
})
#** network directed w/ names ====
test_that("`nw_dir %>% rep_edgelist(use_names = TRUE)
          %==%
          network::as.matrix.network.edgelist(nw_dir)` w/ names manually replaced
          ", {
  expect_true(rep_edgelist(nw_dir, use_names = TRUE) %==% as_el(nw_dir, use_names = TRUE))
})
#** igraph directed w/ conversion ====
test_that("`ig_dir %>% rep_edgelist()
          %==%
          ig_dir %>% as_network() %>% rep_edgelist()`", {
  expect_true(rep_edgelist(ig_dir) %==% rep_edgelist(as_network(ig_dir)))
})
#** network directed w/ conversion ====
test_that("`nw_dir %>% rep_edgelist()
          %==%
          network %>% as_network() %>% rep_edgelist()`", {
  expect_true(rep_edgelist(nw_dir) %==% rep_edgelist(as_igraph(nw_dir)))
})
#** igraph directed w/ conversion + names ====
test_that("`ig_dir %>% rep_edgelist(use_names = TRUE)
          %==%
          ig_dir %>% as_network() %>% as_igraph() %>%  rep_edgelist(use_names = TRUE)`", {
  expect_true(rep_edgelist(ig_dir, use_names = TRUE) %==% rep_edgelist(as_igraph(as_network(ig_dir)), use_names = TRUE))
})
#** network directed w/ conversion + names ====
test_that("`nw_dir %>% rep_edgelist(use_names = TRUE)
          %==%
          network %>% as_igraph() %>% as_network() %>% rep_edgelist(use_names = TRUE)`", {
  expect_true(rep_edgelist(nw_dir, use_names = TRUE) %==% rep_edgelist(as_network(as_igraph(nw_dir)), use_names = TRUE))
})
#
#
#
#
# adjacency matrices ====

#* undirected adjacency matrices ====
#** igraph undirected ====
context("Undirected Adjacency Matrices")

test_that("`ig_undir %>% rep_adjacency_matrix()
          %==%
          igraph::as_adjacency_matrix(ig_undir, sparse = FALSE)`", {
  expect_true(rep_adjacency_matrix(ig_undir) %==% igraph::as_adjacency_matrix(ig_undir, sparse = FALSE))
})
#** network undirected ====
test_that("`nw_undir %>% rep_adjacency_matrix()
          %==%
          network::as.matrix.network.adjacency(nw_undir)`", {
  expect_true(rep_adjacency_matrix(nw_undir) %==% network::as.matrix.network.adjacency(nw_undir))
})
#** igraph undirected w/ conversion ====
test_that("`ig_undir %>% rep_adjacency_matrix()
          %==%
          ig_undir %>% as_network() %>% network::as.matrix.network.adjacency()`", {
  expect_true(rep_adjacency_matrix(ig_undir) %==% network::as.matrix.network.adjacency(as_network(ig_undir)))
})
#** network undirected w/ conversion ====
test_that("`nw_undir %>% rep_adjacency_matrix()
          %==%
          nw_undir %>% as_igraph() %>% igraph::as_adjacency_matrix(sparse = FALSE)`", {
  expect_true(rep_adjacency_matrix(nw_undir) %==% igraph::as_adjacency_matrix(as_igraph(nw_undir), sparse = FALSE))
})
#** igraph undirected w/ sparse ====
test_that("`ig_undir %>% rep_adjacency_matrix(sparse = TRUE)
          %==%
          igraph::as_adjacency_matrix(ig_undir, sparse = TRUE)`", {
  expect_true(rep_adjacency_matrix(ig_undir, sparse = TRUE) %==% igraph::as_adjacency_matrix(ig_undir, sparse = TRUE))
})
#
#
#
#
#* directed adjacency matrices ====
context("Directed Adjacency Matrices")
#** igraph directed ====
test_that("`ig_dir %>% rep_adjacency_matrix()
          %==%
          `igraph::as_adjacency_matrix(ig_dir, sparse = FALSE)`", {
  expect_true(rep_adjacency_matrix(ig_dir) %==% igraph::as_adjacency_matrix(ig_dir, sparse = FALSE))
})
#** network directed ====
test_that("`nw_dir %>% rep_adjacency_matrix()
          %==%
          network::as.matrix.network.adjacency(nw_dir)`", {
  expect_true(rep_adjacency_matrix(nw_dir) %==% network::as.matrix.network.adjacency(nw_dir))
})
#** igraph directed w/ conversion ====
test_that("`ig_dir %>% rep_adjacency_matrix()
          %==%
          ig_dir %>% as_network() %>% network::as.matrix.network.adjacency()`", {
  expect_true(rep_adjacency_matrix(ig_dir) %==% network::as.matrix.network.adjacency(as_network(ig_dir)))
})
#** network directed w/ conversion ====
test_that("`nw_dir %>% rep_adjacency_matrix()
          %==%
          nw_dir %>% as_igraph() %>% igraph::as_adjacency_matrix(sparse = FALSE)`", {
  expect_true(rep_adjacency_matrix(nw_dir) %==% igraph::as_adjacency_matrix(as_igraph(nw_dir), sparse = FALSE))
})
#** igraph directed w/ sparse ====
test_that("`ig_dir %>% rep_adjacency_matrix(sparse = TRUE)
          %==%
          igraph::as_adjacency_matrix(ig_dir, sparse = TRUE)`", {
  expect_true(rep_adjacency_matrix(ig_dir, sparse = TRUE) %==% igraph::as_adjacency_matrix(ig_dir, sparse = TRUE))
})
#
#
#
#
# Attribute Edge Lists ====
#* Undirected Attribute Edge Lists ====
context("Undirected Attribute Edge Lists")
#* igraph undirected w/ conversion ====
test_that("`ig_undir %>% rep_attr_el()
          %==%
          ig_undir %>% as_network() %>% rep_attr_adj_mat()`", {
  expect_true(rep_attr_el(ig_undir, vrt_attr = "node_character") %==% rep_attr_el(as_network(ig_undir), vrt_attr = "node_character"))
})
# * network undirected w/ conversion ====
test_that("`nw_undir %>% rep_attr_el()
          %==%
          nw_undir %>% as_igraph() %>%  rep_attr_el()`", {
  expect_true(rep_attr_el(nw_undir, vrt_attr = "node_character") %==% rep_attr_el(as_network(nw_undir), vrt_attr = "node_character"))
})
#
#
#
#
#* Directed Attribute Edge Lists ====
context("Directed Attribute Edge Lists")
#* igraph directed w/ conversion ====
test_that("`ig_dir %>% rep_attr_el()
          %==%
          ig_dir %>% as_network() %>%  rep_attr_el()`", {
  expect_true(rep_attr_el(ig_dir, vrt_attr = "node_character") %==% rep_attr_el(as_network(ig_dir), vrt_attr = "node_character"))
})
# * network directed w/ conversion ====
test_that("`nw_dir %>% rep_attr_el()
          %==%
          nw_dir %>% as_igraph() %>%  rep_attr_el()`", {
  expect_true(rep_attr_el(nw_dir, vrt_attr = "node_character") %==% rep_attr_el(as_network(nw_dir), vrt_attr = "node_character"))
})
#
#
#
#
# Attribute Adjacency Matrices ====
context("Undirected Attribute Adjacency Matrices")
#* igraph undirected w/ conversion ====
test_that("`ig_undir %>% rep_attr_adj_mat()
          %==%
          ig_undir %>% as_network() %>%  rep_attr_adj_mat()`", {
  expect_true(rep_attr_adj_mat(ig_undir, vrt_attr = "node_character") %==% rep_attr_adj_mat(as_network(ig_undir), vrt_attr = "node_character"))
})
# * network undirected w/ conversion ====
test_that("`nw_undir %>% rep_attr_adj_mat()
          %==%
          nw_undir %>% as_igraph() %>%  rep_attr_adj_mat()`", {
  expect_true(rep_attr_adj_mat(nw_undir, vrt_attr = "node_character") %==% rep_attr_adj_mat(as_network(nw_undir), vrt_attr = "node_character"))
})
#
#
#
#
# 
context("Directed Attribute Adjacency Matrices")
#* igraph directed w/ conversion ====
test_that("`ig_dir %>% rep_attr_adj_mat()
          %==%
          ig_dir %>% as_network() %>%  rep_attr_adj_mat()`", {
  expect_true(rep_attr_adj_mat(ig_dir, vrt_attr = "node_character") %==% rep_attr_adj_mat(as_network(ig_dir), vrt_attr = "node_character"))
})
# * network directed w/ conversion ====
test_that("`nw_dir %>% rep_attr_adj_mat()
          %==%
          `nw_dir %>% as_igraph() %>% rep_attr_adj_mat()`", {
  expect_true(rep_attr_adj_mat(nw_dir, vrt_attr = "node_character") %==% rep_attr_adj_mat(as_network(nw_dir), vrt_attr = "node_character"))
})
#
#
#
#
# 
context("Undirected `attr_el`s from `attr_adj_mats`")
#* igraph undirected w/ conversion ====
test_that("`ig_undir %>% rep_attr_adj_mat() %>% rep_attr_el()
          %==%
          ig_undir %>% as_network() %>%  rep_attr_adj_mat() %>% rep_attr_el(_`", {
  expect_true(rep_attr_el(rep_attr_adj_mat(ig_undir, vrt_attr = "node_character")) %==% rep_attr_el(rep_attr_adj_mat(as_network(ig_undir), vrt_attr = "node_character")))
})
#* network undirected w/ conversion ====
test_that("`nw_undir %>% rep_attr_adj_mat() %>% rep_attr_el()
          %==%
          nw_undir %>% as_igraph() %>%  rep_attr_adj_mat() %>% rep_attr_el()`", {
  expect_true(rep_attr_el(rep_attr_adj_mat(nw_undir, vrt_attr = "node_character")) %==% rep_attr_el(rep_attr_adj_mat(as_igraph(nw_undir), vrt_attr = "node_character")))
})
#
#
#
#
# 
context("Directed `attr_el`s from `attr_adj_mats`")
#* igraph undirected w/ conversion ====
test_that("`ig_undir %>% rep_attr_adj_mat() %>% rep_attr_el()
          %==%
          igraph %>% as_network() %>%  rep_attr_adj_mat() %>% rep_attr_el()`", {
  expect_true(rep_attr_el(rep_attr_adj_mat(ig_dir, vrt_attr = "node_character")) %==% rep_attr_el(rep_attr_adj_mat(as_network(ig_dir), vrt_attr = "node_character")))
})
#* network undirected w/ conversion ====
test_that("`nw_undir %>% rep_attr_adj_mat() %>% rep_attr_el()
          %==%
          nw_undir %>% as_igraph() %>%  rep_attr_adj_mat() %>% rep_attr_el()`", {
  expect_true(rep_attr_el(rep_attr_adj_mat(nw_dir, vrt_attr = "node_character")) %==% rep_attr_el(rep_attr_adj_mat(as_igraph(nw_dir), vrt_attr = "node_character")))
})
#
#
#
#
# 
context("Undirected Mixing Matrices")
#* igraph w/ conversion ====
test_that("`ig_undir %>% rep_mixing_matrix()
          %==%
          ig_undir %>% as_network() %>% rep_mixing_matrix()`", {
  expect_true(rep_mixing_matrix(ig_undir, vrt_attr = "node_character") %==% rep_mixing_matrix(as_network(ig_undir), vrt_attr = "node_character"))
})
#* network w/ conversion ====
test_that("`nw_undir %>% rep_mixing_matrix()
          %==%
          nw_undir %>% as_igraph() %>%  rep_mixing_matrix()`", {
  expect_true(rep_mixing_matrix(nw_undir, vrt_attr = "node_character") %==% rep_mixing_matrix(as_igraph(nw_undir), vrt_attr = "node_character"))
})
#
#
#
#
# 
context("Directed Mixing Matrices")
#* igraph w/ conversion ====
test_that("`ig_dir %>% rep_mixing_matrix()
          %==%
          ig_dir %>% as_network() %>% rep_mixing_matrix()`", {
  expect_true(rep_mixing_matrix(ig_dir, vrt_attr = "node_character") %==% rep_mixing_matrix(as_network(ig_dir), vrt_attr = "node_character"))
})
#* network w/ conversion ====
test_that("`nw_dir %>% rep_mixing_matrix()
          %==%
          nw_dir %>% as_igraph() %>%  rep_mixing_matrix()`", {
  expect_true(rep_mixing_matrix(nw_dir, vrt_attr = "node_character") %==% rep_mixing_matrix(as_igraph(nw_dir), vrt_attr = "node_character"))
})

