#' The `bridge_net` object effectively bridges the `network` objects used by the `statnet` 
#' suite and `igraph` objects and their derivatives.
#' 
#' @param x `igraph` ([`igraph::graph`]), [`network::network`], or [`tidygraph::tbl_graph`].
#' @param ... Named arguments passed on to other methods. Currently `edg_attrs`, 
#' `vrt_attrs`, and `net_attrs`.
#' @param edg_attrs `logical`, defaults `TRUE`. Whether to retain `x`'s edge attributes.
#' @param vrt_attrs `logical`, defaults `TRUE`. Whether to retain `x`'s vertex attributes.
#' @param net_attrs `logical`, defaults `TRUE`. Whether to retain `x`s network/graph-level
#' attributes.
#' 
#' @details
#' `bridge_net` objects contain the following elements: \cr
#' * `graph_attributes`
#'   + `graph_attributes` describe external data fields used to annotate objects, such as
#'     graph names or citation information.
#' * `metadata`
#'   + `metadata` describe the graph itself and include:
#'     + `n_vertices`: `integer` counting the number of vertices in the graph.
#'     + `n_edges`: `integer` counting the number of edges in the graph.
#'     + `is_directed`: `logical` indicating whether edges are intended to be directed.
#'     + `is_bipartite`: `logical` indicating whether the graph is biparite
#'       + `n_actors`: `integer` counting the number of vertices belonging to a bipartite
#'         graph's "actor" mode.
#'     + `any_multiplex`: `logical` indicating whether the graph includes any parallel
#'        edges.
#'     + `has_loops`: `logical` indicating whether the graph contains loop edges.
#'     + `has_isolates`: `logical` indicating whether the graph contains any isolated 
#'       vertices.
#' * `edges`
#'   + `edges` are contained in either a `matrix` or `data.frame` where the first column
#'     (`.ego`) always contains an edge's source and the second column (`.alter`) always
#'     contains an edge's target.
#'     + `.ego` and `.alter` are stored as type `integer`.
#'   + If the edge data of an `bridge_net` object only include `.ego`s and `.alter`s, `edges`
#'     are stored in a two-column `matrix`.
#'   + If edge attribute data are present, `edges` are stored in a `data.frame` where 
#'     each additional column contains a given edge attribute.
#' * `vertices` 
#'   + `vertices` are contained in a `data.frame` where the first column always 
#'      contains vertex `.name`s and subsequent columns contain any vertex attributes
#'      present.
#'      + If an `bridge_net` contains a bipartite graph, the second column will always 
#'        be `.actor`, a `logical` `vector` indicating whether each vertex is intended 
#'        to belong to the "actor" mode.
#' 
#' @rdname bridge_net
#' 

#' @rdname bridge_net
#' 
#' @export
as_bridge_net <- function(x, ...) {
  UseMethod("as_bridge_net")
}

#' @rdname bridge_net
#' 
#' @export
as_bridge_net.bridge_net <- function(x, ...) {
  x
}

#' @rdname bridge_net
#' 
#' @export
as_bridge_net.network <- function(x, edg_attrs = TRUE, vrt_attrs = TRUE, net_attrs = TRUE) {
  gal <- x[["gal"]]
  # metadata 1
  metadata <- list(n_vertices = as.integer(gal[["n"]]),
                   n_edges = vector("integer", 1L),
                   is_directed = gal[["directed"]],
                   is_bipartite = is.numeric(gal[["bipartite"]]),
                   n_actors = ifelse(is.numeric(gal[["bipartite"]]), 
                                     gal[["bipartite"]], NA),
                   any_multiplex = vector("logical", 1L),
                   has_loops = vector("logical", 1L),
                   has_isolates = vector("logical", 1L))
  if (is.na(metadata[["n_actors"]])) {
    metadata[["n_actors"]] <- NULL
  }
  # graph attributes
  if (net_attrs) {
    misonomers <- c("n", "directed", "hyper", "loops", "multiple", "bipartite", "mnext")
    graph_attrs <- gal[!names(gal) %in% misonomers]
    if (!length(graph_attrs)) {
      graph_attrs <- NULL
    }
  } else {
    graph_attrs <- NULL
  }
  # edge list
  outl <- unlist(lapply(x[["mel"]], `[[`, "outl")) # faster `NULL` discard than `%||%`
  inl <- unlist(lapply(x[["mel"]], `[[`, "inl"))
  nrow_el <- unique(length(inl), length(outl))
  if (length(nrow_el) > 1L) {
    stop('edgelist "inl" and "outl" are different lengths; some edges are missing one half
         of their dyad.', call. = FALSE)
  }
  if (nrow_el) {
    outl <- as.integer(outl)
    inl <- as.integer(inl)
    el <- vector("integer", nrow_el * 2L)
    if (metadata[["is_directed"]] && !metadata[["is_bipartite"]]) {
      el <- cbind(outl, inl)
      # metadata 2.a
    } 
    if (metadata[["is_directed"]] && metadata[["is_bipartite"]]) {
      el <- cbind(outl, inl)
    }
    if (metadata[["is_directed"]]) {
      metadata[["any_multiplex"]] <- any(anyDuplicated(el) > 0L)
    }
    if (!metadata[["is_directed"]]) {
      el <- cbind(outl, inl)
      el <- t(apply(el, 1, FUN = sort))
      # metadata 2.b
      metadata[["any_multiplex"]] <- any(anyDuplicated(el) > 0L)
    }
  } else {
    el <- matrix(integer(0), nrow = 0L, ncol = 2L)
  }
  # if (ncol(el)) {
    colnames(el) <- c(".ego", ".alter")
  # }
  # if (nrow(el)) {
    # rownames(el) <- NULL
  # }
  
  # metadata 3, 4
  metadata[["n_edges"]] <- nrow(el)
  if (metadata[["n_edges"]]) {
    metadata[["has_loops"]] <- any(el[, 1] == el[, 2])
    metadata[["has_isolates"]] <- !all(seq_len(metadata[["n_vertices"]]) %in% el)
  } else {
    metadata[["has_loops"]] <- FALSE
    metadata[["has_isolates"]] <- FALSE
  }
  
  if (metadata[["n_edges"]] && edg_attrs) {
    edge_attrs <- lapply(x[["mel"]], `[[`, "atl")
    if (requireNamespace("data.table", quietly = TRUE)) {
      edge_attrs <- data.table::rbindlist(edge_attrs)
      edge_attrs <- as.data.frame(edge_attrs, stringsAsFactors = FALSE,
                                  row.names = NULL)
    } else if (requireNamespace("dplyr", quietly = TRUE)) {
      edge_attrs <- dplyr::bind_rows(edge_attrs)
      edge_attrs <- as.data.frame(edge_attrs, stringsAsFactors = FALSE)
      rownames(edge_attrs) <- NULL
    } else {
      edge_attrs <- do.call(rbind.data.frame, c(edge_attrs, 
                                                stringsAsFactors = FALSE,
                                                make.row.names = FALSE))
    }
    edge_attrs[["na"]] <- NULL
    rownames(edge_attrs) <- NULL
    edge_df <- cbind.data.frame(el, edge_attrs, stringsAsFactors = FALSE,
                                row.names = NULL)
  } else {
    edge_df <- NULL
  }
  
  ### OLD
  
  # if (metadata[["n_vertices"]] && vrt_attrs) {
  #   if (requireNamespace("data.table", quietly = TRUE)) {
  #     vertex_df <- data.table::rbindlist(x[["val"]], fill = TRUE)
  #     vertex_df <- as.data.frame(vertex_df, stringsAsFactors = FALSE)
  #   } else if (requireNamespace("dplyr", quietly = TRUE)) {
  #     vertex_df <- dplyr::bind_rows(x[["val"]])
  #     vertex_df <- as.data.frame(vertex_df, stringsAsFactors = FALSE)
  #   } else {
  #     vertex_df <- do.call(rbind.data.frame, c(x[["val"]], stringsAsFactors = FALSE))
  #   }
  #   if (is.numeric(gal[["bipartite"]])) {
  #     vertex_df$.actor <- c(rep(TRUE, gal[["bipartite"]]),
  #                             rep(FALSE, gal[["n"]] - gal[["bipartite"]]))
  #   }
  #   vertex_df[["na"]] <- NULL
  #   colnames(vertex_df)[colnames(vertex_df) == "vertex.names"] <- ".name"
  #   if (is.numeric(gal[["bipartite"]])) {
  #     first_cols <- c(".name", ".actor")
  #   } else {
  #     first_cols <- ".name"
  #   }
  #   other_col_names <- colnames(vertex_df)[!colnames(vertex_df) %in% first_cols]
  #   col_order <- c(first_cols, other_col_names)
  #   vertex_df <- vertex_df[, col_order, drop = FALSE]
  #   rownames(vertex_df) <- NULL
  # } else {
  #   vertex_df <- NULL
  # }
  
  ####
  
  ###
  # NEW
  
  vertex_df <- vrt_to_df(x)
  
  ##
  out <- list(metadata = metadata,
              graph_attributes = graph_attrs,
              edges = edge_df %||% el,
              vertices = vertex_df)
  class(out) <- "bridge_net"
  
  out
}

#' @rdname bridge_net
#' 
#' @importFrom igraph as_edgelist delete_vertex_attr ecount edge_attr graph_attr has.multiple is_directed 
#' @importFrom igraph is.loop permute set_vertex_attr V vcount vertex_attr vertex_attr_names
#' @export
as_bridge_net.igraph <- function(x, edg_attrs = TRUE, vrt_attrs = TRUE, net_attrs = TRUE) {
  if (net_attrs) {
    graph_attrs <- graph_attr(x)
    if (!length(graph_attrs)) {
      graph_attrs <- NULL
    }
  } else {
    graph_attrs <- NULL
  }
  metadata <- list(n_vertices = as.integer(vcount(x)),
                   n_edges = as.integer(ecount(x)),
                   is_directed = is_directed(x),
                   is_bipartite = "type" %in% vertex_attr_names(x),
                   n_actors = ifelse("type" %in% vertex_attr_names(x),
                                     length(which(V(x)$type)), NA_integer_),
                   any_multiplex = has.multiple(x),
                   has_loops = any(is.loop(x)),
                   has_isolates = vector("logical", 1L))
  
  if (is.na(metadata[["n_actors"]])) {
    metadata[["n_actors"]] <- NULL
  }
  # if (metadata[["is_bipartite"]]) {
  #   if (!"name" %in% vertex_attr_names(x)) {
  #     V(x)$name <- seq_len(vcount(x))
  #   }
  #   type_order <- V(x)$type
  #   names(type_order) <- seq_along(type_order)
  #   new_order <- as.integer(names(sort(type_order, decreasing = TRUE)))
  #   x <- permute(x, match(seq_along(new_order), new_order))
  # }
  
  el <- as_edgelist(x, names = FALSE)
  el <- matrix(as.integer(el), ncol = 2)
  colnames(el) <- c(".ego", ".alter")
  
  if (metadata[["n_edges"]]) {
    metadata[["has_isolates"]] <- !all(seq_len(metadata[["n_vertices"]]) %in% el)
  } else {
    metadata[["has_isolates"]] <- FALSE
  }
  
  if (metadata[["n_edges"]] && edg_attrs) {
    if (length(edge_attr_names(x))) {
      # edge_df <- edge_attr(x) # %{}% NULL
    # } else {
      # ed
    # }
    # if (!is.null(edge_df)) {
      edge_df <- as.data.frame(edge_attr(x), stringsAsFactors = FALSE)
      rownames(edge_df) <- NULL
      edge_df <- cbind.data.frame(el, edge_df, stringsAsFactors = FALSE)
    # }
    } else {
      edge_df <- NULL
    }
  } else {
    edge_df <- NULL
  }
  
  ### OLD
  
  # if (metadata[["n_vertices"]] && vrt_attrs) {
  #   if (!"name" %in% vertex_attr_names(x)) {
  #     if ("Name" %in% vertex_attr_names(x)) {
  #       x <- set_vertex_attr(x, name = ".name", value = V(x)$Name)
  #       x <- delete_vertex_attr(x, name = "Name")
  #     } else {
  #       x <- set_vertex_attr(x, name = ".name", value = seq_len(vcount(x)))
  #     }
  #   } else {
  #     x <- set_vertex_attr(x, name = ".name", value = V(x)$name)
  #     x <- delete_vertex_attr(x, name = "name")
  #   }
  #   vertex_df <- as.data.frame(vertex_attr(x), stringsAsFactors = FALSE)
  #   if (metadata[["is_bipartite"]]) {
  #     names(vertex_df)[names(vertex_df) == "type"] <- ".actor"
  #     first_cols <- c(".name", ".actor")
  #   } else {
  #     first_cols <- ".name"
  #   }
  #   other_col_names <- colnames(vertex_df)[!colnames(vertex_df) %in% first_cols]
  #   if (!length(other_col_names)) {
  #     other_col_names <- NULL
  #   }
  #   col_order <- c(first_cols, other_col_names)
  #   vertex_df <- vertex_df[, col_order, drop = FALSE]
  #   rownames(vertex_df) <- NULL
  # } else {
  #   vertex_df <- NULL
  # }
  
  ### OLD
  
  #### NEW
  vertex_df <- vrt_to_df(x)
  ### NEW
  
  out <- list(metadata = metadata,
              graph_attributes = graph_attrs,
              edges = edge_df %||% el,
              vertices = vertex_df)
  class(out) <- "bridge_net"
  
  out
}

#' @rdname bridge_net
#' @export
as_bridge_net.tbl_graph <- function(x, edg_attrs = TRUE, vrt_attrs = TRUE, net_attrs = TRUE) {
  as_bridge_net(as_igraph(x))
}


#' @export
print.bridge_net <- function(x, .nrow = 3L) {
  dir_chr <- ifelse(x[["metadata"]][["is_directed"]],
                    "directed", "undirected")
  article <- ifelse(dir_chr == "directed", "A", "An")
  multi_chr <- ifelse(x[["metadata"]][["any_multiplex"]],
                      "multiplex,", "")
  bip_chr <- ifelse(x[["metadata"]][["is_bipartite"]],
                    "bipartite", "1-mode")
  
  descrip <- sprintf("%s %s, %s %s `bridge_net`.", 
                     article, dir_chr, multi_chr, bip_chr)
  cat(txt_wrap(descrip, prefix = "# "))
  cat("\n")
  loops_chr <- ifelse(x[["metadata"]][["has_loops"]], ">0", 0L)
  iso_chr <- ifelse(x[["metadata"]][["has_isolates"]], ">0", 0L)
  loops_iso <- sprintf("- Contains %s loops and %s isolates.", loops_chr, iso_chr)
  cat(txt_wrap(loops_iso, prefix = "## "))
  cat("\n")
  if (!is.null(x[["edges"]]) && nrow(x[["edges"]])) {
    edges <- head(x[["edges"]], .nrow)
    if (!is.null(x[["vertices"]])) {
      if (is.matrix(edges)) {
        edges <- as.data.frame(edges)
        edges[, ".ego"] <- x[["vertices"]][[".name"]][edges[, ".ego"]]
        edges[, ".alter"] <- x[["vertices"]][[".name"]][edges[, ".alter"]]
        edges <- as.matrix(edges)
      } else if (is.data.frame(edges)) {
        edges[, ".ego"] <- x[["vertices"]][[".name"]][edges[, ".ego"]]
        edges[, ".alter"] <- x[["vertices"]][[".name"]][edges[, ".alter"]]
      }
    }
    nrow_edges <- nrow(edges)
    cat(sprintf("$edges # first %s of %s", 
                nrow_edges, 
                format(x[["metadata"]][["n_edges"]], big.mark = ",")))
    cat("\n")
    if (!is.null(x[["vertices"]])) {
      print(format_table(edges, edges_or_verts = "edges", 
                         specify_edge_dyad_types = TRUE), 
            quote = FALSE)
    } else {
      print(format_table(edges, edges_or_verts = "edges", 
                         specify_edge_dyad_types = TRUE), 
            quote = FALSE) 
    }
  } else {
    cat("# No edge data.")
    cat("\n")
  }
  if (!is.null(x[["vertices"]]) && nrow(x[["vertices"]])) {
    vertices <- head(x[["vertices"]], .nrow)
    nrow_verts <- nrow(vertices)
    cat(sprintf("$vertices # first %s of %s", 
                nrow_verts, 
                format(x[["metadata"]][["n_vertices"]], big.mark = ",")))
    cat("\n")
    print(format_table(vertices, edges_or_verts = "verts"), quote = FALSE)
  } else {
    cat("# No vertex data.")
  }
  invisible(x)
}

#' #' @rdname bridge_net
#' #' 
#' #' @importFrom crayon make_style
#' #' @export
#' print.bridge_net <- function(x, n_rows = 2L) {
#'   title_col <- crayon::red
#'   top_col <- crayon::blue
#'   sub_col <- crayon::cyan
#'   class_col <- crayon::magenta
#'   missing_col <- crayon::silver
#'   
#'   cat(title_col("# An bridge_net."), "\n")
#'   metadata_buffer <- max(nchar(names(x[["metadata"]])))
#'   cat(top_col("$metadata"), "\n")
#'   for (i in names(x[["metadata"]])) {
#'     target <- txt_trim(txt_squish(x[["metadata"]][[i]]))
#'     name_width <- nchar(i)
#'     cat(sub_col(" $", i, sep = ""), ": ",
#'         txt_wrap(target, indent = metadata_buffer - name_width, 
#'                  exdent = metadata_buffer + 4L), 
#'         "\n", sep = "")
#'     
#'   }
#'   if (!is.null(x[["graph_attributes"]])) {
#'     graph_attr_buffer <- max(nchar(names(x[["graph_attributes"]])))
#'     cat(top_col("$graph_attributes"), "\n")
#'     for (i in names(x[["graph_attributes"]])) {
#'       target <- txt_trim(txt_squish(x[["graph_attributes"]][[i]]))
#'       name_width <- nchar(i)
#'       cat(sub_col(" $", i, sep = ""), ": ",
#'         txt_wrap(target, indent = graph_attr_buffer - name_width, 
#'                  exdent = graph_attr_buffer + 4L), "\n", sep = "")
#'     }
#'   } else {
#'     cat(missing_col("No Graph-Level Attributes"), "\n")
#'   }
#'   if (!nrow(x[["edges"]])) {
#'     cat(missing_col("No Edges"), "\n")
#'   } else {
#'     cat(top_col("$edges"), 
#'         class_col(class(x[["edges"]])[[1]]),
#'         "-",
#'         n_rows, "of", nrow(x[["edges"]]), "rows", "\n")
#'     print(head(x[["edges"]], n_rows))
#'   }
#'   
#'   if (!is.null(x[["vertices"]])) {
#'     cat(top_col("$vertices"), 
#'         class_col(class(x[["vertices"]])[[1]]),
#'         "-", 
#'         n_rows, "of", nrow(x[["vertices"]]), "rows", "\n")
#'     print(head(x[["vertices"]], n_rows))
#'   } else {
#'     cat(missing_col("No Vertex Attributes"), "\n")
#'   }
#'   invisible(x)
#' }
