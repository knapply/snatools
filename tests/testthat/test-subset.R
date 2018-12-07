context("subsetting edges and vertices")

test_that("edg_subset() works", {
  edge_df <- c(1, 2, 5,
               2, 3, 6,
               3, 4, 7) %>%
    matrix(ncol = 3, byrow = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    `names<-`(c(".ego", ".alter", "weight")) 
  
  ig <- igraph::graph_from_data_frame(edge_df)

  filtered_vert_df_iso <- data.frame(.vrt_id = 1:4,
                                     .vrt_name = as.character(1:4),
                                     stringsAsFactors = FALSE)
  filtered_edge_df_iso <- data.frame(.edg_id = 2:3,
                                     .ego = 2:3,
                                     .alter = 3:4,
                                     weight = as.double(6:7),
                                     stringsAsFactors = FALSE)
  expect_identical(
    vrt_as_df(edg_subset(ig, weight > 5)), filtered_vert_df_iso
  )
  expect_identical(
    edg_as_df(edg_subset(ig, weight > 5)), filtered_edge_df_iso
  )
  filtered_vert_df_no_iso <- data.frame(.vrt_id = 2:4,
                                        .vrt_name = as.character(2:4),
                                        stringsAsFactors = FALSE)
  filtered_edge_df_no_iso <- data.frame(.edg_id = 2:3,
                                        .ego = 1:2,
                                        .alter = 2:3,
                                        weight = as.double(6:7),
                                        stringsAsFactors = FALSE)
  expect_identical(
    vrt_as_df(edg_subset(ig, weight > 5, .drop_isolates = TRUE)),
    filtered_vert_df_no_iso
  )
  expect_identical(
    edg_as_df(edg_subset(as_network(ig), weight > 5, .drop_isolates = TRUE)),
    filtered_edge_df_no_iso
  )
})


test_that("vrt_subset() works", {
  verts <- data.frame(name = letters[1:4],
                      foo = as.double(10:13),
                      bar = c(TRUE, FALSE, TRUE, FALSE),
                      stringsAsFactors = FALSE)
  ig <- c("a", "b",
          "b", "c",
          "c", "d") %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as.data.frame(stingsAsFactors = FALSE) %>%
    `names<-`(c(".ego", ".alter")) %>%
    igraph::graph_from_data_frame(vertices = verts)

  filtered_vert_df_iso <- data.frame(.vrt_id = 2:4,
                                     .vrt_name = c("b", "c", "d"),
                                     foo = c(11, 12, 13),
                                     bar = c(FALSE, TRUE, FALSE),
                                     stringsAsFactors = FALSE)
  filtered_edge_df_iso <- data.frame(.edg_id = 1:2,
                                     .ego = 1:2,
                                     .alter = 2:3,
                                     stringsAsFactors = FALSE)
  expect_identical(
    vrt_as_df(vrt_subset(ig, .vrt_name != "a")), filtered_vert_df_iso
  )
  expect_identical(
    edg_as_df(vrt_subset(as_network(ig), .vrt_name != "a")), filtered_edge_df_iso
  )
})


