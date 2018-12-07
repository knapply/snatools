is_valid_vrt_attr <- function(x, vrt_attr, max_length = 1L) {
  !is.na(vrt_attr) && is.character(vrt_attr) && 
    length(vrt_attr) <= max_length && all(vrt_attr %in% vrt_attr_names(x))
}

validate_vrt_attr <- function(x, vrt_attr, max_length = 1L) {
    if (!is_valid_vrt_attr(x, vrt_attr, max_length = max_length)) {
      if (length(vrt_attr) > 1L) {
        concat_attr <- paste(paste0('"', vrt_attr, '"', collapse = ", "))
        is_are <- "are"
      } else {
        concat_attr <- vrt_attr
        is_are <- "is"
      }
      if (is_empty(vrt_attr_names(x))) {
        valid_vrt_attrs <- ""
      } else {
        valid_vrt_attrs <- patch("The following vertex attributes are valid:
                                 
                                 %s", paste(paste0('"', vrt_attr_names(x), '"',
                                                   collapse = ", ")))
      }
      terminate(patch('%s %s invalid vertex attribute(s). 
  
                      `vrt_attr` must be a character vector of length %s corresponding to the
                       name(s) of vertex attribute(s) in `x`.
  
                      %s',
                      concat_attr, is_are, max_length, valid_vrt_attrs))
    }
}

is_valid_edg_attr <- function(x, edg_attr, max_length = 1L) {
  !is.na(edg_attr) && is.character(edg_attr) && 
    length(edg_attr) <= max_length && edg_attr %in% edg_attr_names(x)
}

validate_edg_attr <- function(x, edg_attr, max_length = 1L) {
    if (!is_valid_edg_attr(x, edg_attr, max_length = max_length)) {
      if (length(edg_attr) > 1L) {
        concat_attr <- paste(paste0('"', edg_attr, '"', collapse = ", "))
        is_are <- "are"
      } else {
        concat_attr <- edg_attr
        is_are <- "is"
      }
      if (is_empty(edg_attr_names(x))) {
        valid_edg_attrs <- ""
      } else {
        valid_edg_attrs <- patch("The following edge attributes are valid:
                                 
                                 %s", paste(paste0('"', edg_attr_names(x), '"',
                                                   collapse = ", ")))
      }
      terminate(patch('%s %s invalid edge attribute(s). 
  
                      `edg_attr` must be a character vector of length %s corresponding to the
                       name(s) of edge attribute(s) in `x`.
  
                      %s',
                      concat_attr, is_are, max_length, valid_edg_attrs))
    }
}


is_valid_net_attr <- function(x, net_attr, max_length = 1L) {
  !is.null(net_attr) && !is.na(net_attr) && is.character(net_attr) && 
    length(net_attr) <= max_length && net_attr %in% net_attr_names(x)
}

validate_net_attr <- function(x, net_attr, max_length = 1L) {
  if(max_length == 1L) {
    valid <- is_valid_net_attr(x, net_attr)
  } else {
    valid <- vapply(net_attr, function(y) is_valid_net_attr(x, y), logical(1))
  }
  if (any(!valid)) {
    if (length(net_attr) > 1L) {
      concat_attr <- paste(paste0('"', net_attr, '"', collapse = ", "))
      is_are <- "are"
    } else {
      concat_attr <- net_attr
      is_are <- "is"
    }
    if (is_empty(net_attr_names(x))) {
      valid_net_attrs <- ""
    } else {
      valid_net_attrs <- patch("The following network attributes are valid:
                               
                               %s", paste(paste0('"', net_attr_names(x), '"',
                                                 collapse = ", ")))
    }
    terminate(patch('%s %s invalid network attribute(s). 

                    `net_attr` must be a character vector of length %s corresponding to the
                     name(s) of network attribute(s) in `x`.

                    %s',
                    concat_attr, is_are, max_length, valid_net_attrs))
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