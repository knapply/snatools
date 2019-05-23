context("subsetting edges and vertices")

test_that("edg_subset() works", {
  ig <- c(1, 2, 5,
          2, 3, 6,
          3, 4, 7) %>% 
    matrix(ncol = 3, byrow = TRUE) %>% 
    tibble::as_tibble() %>% 
    `names<-`(c(".ego", ".alter", "weight")) %>% 
    igraph::graph_from_data_frame()
  
  filtered_vert_df_iso <- tibble::tibble(.vrt_id = 1:4,
                                         .vrt_name = as.character(1:4))
  filtered_edge_df_iso <- tibble::tibble(.edg_id = 1:2,
                                         .ego = 2:3,
                                         .alter = 3:4,
                                         weight = as.double(6:7))
  expect_identical(
    vrt_as_df(edg_subset(ig, weight > 5)), filtered_vert_df_iso
  )
  expect_identical(
    edg_as_df(edg_subset(ig, weight > 5)), filtered_edge_df_iso
  )
  filtered_vert_df_no_iso <- tibble::tibble(.vrt_id = 1:3,
                                            .vrt_name = as.character(2:4))
  filtered_edge_df_no_iso <- tibble::tibble(.edg_id = 1:2,
                                            .ego = 1:2,
                                            .alter = 2:3,
                                            weight = as.double(6:7))
  expect_identical(
    vrt_as_df(edg_subset(ig, weight > 5, drop_isolates = TRUE)),
    filtered_vert_df_no_iso
  )
  expect_identical(
    edg_as_df(edg_subset(ig, weight > 5, drop_isolates = TRUE)),
    filtered_edge_df_no_iso
  )
})


test_that("vrt_subset() works", {
  verts <- tibble::tibble(name = letters[1:4],
                          foo = as.double(10:13),
                          bar = c(TRUE, FALSE, TRUE, FALSE))
  ig <- c("a", "b",
          "b", "c",
          "c", "d") %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    tibble::as_tibble() %>% 
    `names<-`(c(".ego", ".alter")) %>% 
    igraph::graph_from_data_frame(vertices = verts)
  
  filtered_vert_df_iso <- tibble::tibble(.vrt_id = 1:3,
                                         .vrt_name = c("b", "c", "d"),
                                         foo = c(11, 12, 13),
                                         bar = c(FALSE, TRUE, FALSE))
  filtered_edge_df_iso <- tibble::tibble(.edg_id = 1:2,
                                         .ego = 1:2,
                                         .alter = 2:3)
  expect_identical(
    vrt_as_df(vrt_subset(ig, .vrt_name != "a")), filtered_vert_df_iso
  )
  expect_identical(
    edg_as_df(vrt_subset(ig, .vrt_name != "a")), filtered_edge_df_iso
  )
})


