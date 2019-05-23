# NSE helpers ====
dp_sub <- function(x, env = parent.frame()) {
  deparse(substitute(x, env = env))
}

# transformation helpers ====
sort_el_cols_by_row <- function(x) {
  cbind(pmin.int(x[, 1L], x[, 2L]), pmax.int(x[, 1L], x[, 2L]))
}

# misc. helpers
`%||%` <- function(lhs, rhs) {
  if (identical(lhs, NULL)) return(rhs)
  lhs
}

`%{}%` <- function(lhs, rhs) {
  if (length(lhs) == 0L) return(rhs)
  lhs
}

drop_nulls <- function(x) {
  Filter(length, x)
}

is_empty <- function(x) {
  length(x) == 0L
}

# is_null <- function(x) {
  # identical(x, NULL)
# }

is_scalar <- function(x) {
  is.atomic(x) && length(x) == 1L
}


is_true <- function(x) {
  identical(x, TRUE) && isTRUE(x)
}

is_named <- function(x) {
  le_names <- names(x)
  if (is.null(le_names) || all(is.na(le_names))) {
    return(FALSE)
  }
  if(any(nchar(le_names) == 0L)) {
    return(FALSE)
  }
  TRUE
}

# bipartite handling ====

#' @importFrom igraph permute set_vertex_attr vcount vertex_attr vertex_attr_names
prep_bipartite_igraph <- function(x) {
  # if (!"name" %in% vertex_attr_names(x)) {
  #   x <- set_vertex_attr(x, "name", value = seq_len(vcount(x)))
  # }
  if (all(vertex_attr(x, "type")[seq_len(net_count_actors.igraph(x))])) {
    return(x)
  }
  if (".actor" %in% vertex_attr_names(x)) {
    new_vrt_seq <- vertex_attr(x, ".actor")
    decrease <- FALSE
  } else {
    new_vrt_seq <- vertex_attr(x, "type")
    decrease <- TRUE
  }
  names(new_vrt_seq) <- seq_along(new_vrt_seq)
  new_order <- as.integer(names(sort(new_vrt_seq, decreasing = decrease)))
  x <- permute(x, match(seq_along(new_vrt_seq), new_order))
  
  x
}


# validation ====
is_valid_graph <- function(x) {
  inherits(x, c("igraph", "network", "tbl_graph"))
}

is_valid_vrt_attr <- function(x, vrt_attr, max_length = 1L) {
  !is.na(vrt_attr) && is.character(vrt_attr) && 
    length(vrt_attr) <= max_length && all(vrt_attr %in% vrt_attr_names(x))
}

is_valid_edg_attr <- function(x, edg_attr, max_length = 1L) {
  !is.na(edg_attr) && is.character(edg_attr) && 
    length(edg_attr) <= max_length && edg_attr %in% edg_attr_names(x)
}

validate_args <- function(x, vrt_attr = NULL, edg_attr = NULL, net_attr = NULL, 
                          validate_graph = FALSE) {
  throw_error <- FALSE
  if (!is.null(x)) {
    x_name <- dp_sub(x)
    if (validate_graph && !is_valid_graph(x)) {
      glue_stop("`{x_name}` must be an `igraph`, `network`, or `tbl_graph`.")
    }
  }
  if (!is.null(vrt_attr) && !is_valid_vrt_attr(x, vrt_attr)) {
    throw_error <- TRUE
    bad_arg <- "vrt_attr"
    bad_attr <- vrt_attr
    valid_attrs <- paste0('"', vrt_attr_names(x), '"', collapse = ", ")
    attr_type <- "vertex"
  } else if (!is.null(edg_attr) && !is_valid_edg_attr(x, edg_attr)) {
    throw_error <- TRUE
    bad_arg <- "edg_attr"
    bad_attr <- edg_attr
    valid_attrs <- paste0('"', edg_attr_names(x), '"', collapse = ", ")
    attr_type <- "edge"
  } 
  # else if (!is.null(net_attr) && !is_valid_edg_attr(x, net_attr)) {
  #   throw_error <- TRUE
  #   bad_arg <- "net_attr"
  #   bad_attr <- net_attr
  #   valid_attrs <- paste0('"', net_attr_names(x), '"', collapse = ", ")
  #   attr_type <- "net"
  # }
  if (throw_error) {
    if (!txt_detect(valid_attrs, "\\w")) {
      valid_attrs <- glue::glue("- No {attr_type} attributes present.")
    }
    glue_stop("Invalid {attr_type} attributes: `\"{bad_attr}\"`.

              `{bad_arg}` must be a character vector of length 1 corresponding to the name
              of a {attr_type} attribute in `x`.

              Your x: 

              `{x_name}`

              The following {attr_type} attributes are valid:

              {valid_attrs}")
  }
}

is_vrt_names_attr <- function(x) {
  x %in% c(".name", "name", "vertex.names")
}

get_vrt_names_attr <- function(x) {
  if (inherits(x, "bridge_net")) {
    return(".name")
  }
  if (inherits(x, "igraph")) {
    return("name")
  } 
  if (inherits(x, "network")) {
    return("vertex.names")
  }
}

# txt ====
txt_detect <- function(text, pattern, ignore_case = FALSE, ...) {
  grepl(pattern = pattern, x = text, ignore.case = ignore_case, perl = TRUE, ...)
}

txt_wrap <- function(text, width = floor(options("width")[["width"]] * 0.9), 
                     indent = 0, exdent = 0, initial = "", prefix = "") {
  out <- strwrap(text, width = width, indent = indent, exdent = exdent, initial = initial,
                 prefix = prefix, simplify = FALSE)
  vapply(out, paste0, character(1), collapse = ("\n"))
}

# print ====
glue_wrap <- function(..., .sep = "", .envir = parent.frame(), initial = "", 
                      prefix = "") {
  txt_wrap(glue::glue(..., .envir = .envir, initial = initial), 
           initial = initial, prefix = prefix)
}

glue_cat <- function(..., .sep = "", .envir = parent.frame(), initial = "", prefix = "") {
  cat(glue_wrap(..., .envir = .envir, initial = initial))
}

# messages ====
glue_message <- function(..., .sep = "", .envir = parent.frame(), 
                         domain = NULL, appendLF = TRUE) {
  message(glue::glue(..., .sep = .sep, .envir = .envir), 
          domain = domain, appendLF = appendLF)
}


glue_stop <- function(..., .sep = "", .envir = parent.frame(), 
                         domain = NULL, call. = FALSE) {
  stop(glue_wrap(..., .sep = .sep, .envir = .envir),  
       call. = call.)
}



