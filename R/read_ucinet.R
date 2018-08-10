#' Read UCINET's .dl files
#' 
#' @param path Path to file or URL.
#' 
#' @return A `ucinet` object.
#'
#' @export
read_ucinet <- function(path, directed = NULL) {
  raw <- readLines(path)
  file_type <- raw[[1]]
  if(txt_to_lower(file_type) != "dl"){
    stop("`read_ucinet()` expects line 1 to be 'DL' or 'dl' (Data Language)", 
         call. = FALSE)
  }
  node_count <- as.integer(txt_extract(raw[[2]], "(?<=^N=)\\d+"))
  matrix_count <- as.integer(txt_extract(raw[[2]], "(?<=\\sNM=)\\d+$"))
  if(length(matrix_count) == 0) {
    matrix_count <- 1L
  }
  file_format <- txt_extract(txt_subset(raw, "FORMAT\\s=\\s"), "(?<=\\s=\\s).*$")
  n_row <- as.integer(txt_extract(raw[[2]], "(?<=^NR=)\\d+"))
  if(length(n_row) == 0) {
    n_row <- node_count
  }
  n_col <- as.integer(txt_extract(raw[[2]], "(?<=\\sNC=)\\d+$"))
  if(length(n_col) == 0) {
    n_col <- node_count
  }
  
  attr_lines <- txt_which(raw, ":$")
  names(attr_lines) <- txt_remove(txt_subset(raw, ":$"), ":")
  
  # row_labels_line <- as.integer(txt_which(raw, "ROW LABELS:"))
  # col_labels_line <- as.integer(txt_which(raw, "COLUMN LABELS:"))
  # data_line <- as.integer(txt_which(raw, "DATA:"))
  # level_labels_line <- as.integer(txt_which(raw, "LEVEL LABELS:"))
  
  data_raw <- raw[(attr_lines["DATA"] + 1):length(raw)]
  data_trimmed <- txt_squish(txt_trim(data_raw))
  data_list <- lapply(txt_split(data_trimmed, "\\s+"), as.integer)
  # data_flat <- unlist(data_list)
  if(matrix_count == 1) {
    data_flat <- unlist(data_list)
    out <- matrix(data_flat, nrow = n_row, ncol = n_col, byrow = TRUE)
  }
  if(matrix_count > 1) {
    split_list <- split(data_list, rep(seq_len(matrix_count), each = n_row))
    flat_matrices <- lapply(split_list, unlist)
    out <- lapply(flat_matrices, matrix, nrow = n_row, ncol = n_col, byrow = TRUE)
    # rows_per_matrix <- nrow(out) %/% matrix_count
    # flat_matrices <- split(matrix(data_flat, ncol = n_col, byrow = TRUE), rep(seq_len(matrix_count), each = n_row))
    # out <- lapply(flat_matrices, matrix, nrow = n_row, ncol = n_col)
  }
  if(!is.na(attr_lines["ROW LABELS"])) {
    row_labels <- raw[(attr_lines["ROW LABELS"] + 1):(attr_lines["COLUMN LABELS"] - 1)]
    if(is.list(out)) {
      out <- lapply(out, `rownames<-`, row_labels)
    } else {
      rownames(out) <- row_labels
    }
  }
  if(!is.na(attr_lines["COLUMN LABELS"])) {
    if(is.list(out)) {
      col_labels <- raw[(attr_lines["COLUMN LABELS"] + 1):(attr_lines["LEVEL LABELS"] - 1)]
      out <- lapply(out, `colnames<-`, col_labels)
    } else {
      col_labels <- raw[(attr_lines["COLUMN LABELS"] + 1):(attr_lines["DATA"] - 1)]
      colnames(out) <- col_labels
    }
  }
  if(!is.na(attr_lines["LEVEL LABELS"])) {
    lev_labels <- raw[(attr_lines["LEVEL LABELS"] + 1):(attr_lines["DATA"] - 1)]
    names(out) <- lev_labels
  }
  if(is.null(directed)) {
    if(is.list(out)) {
      directed <- as.character(vapply(out, function(x) !isSymmetric(x), logical(1)))
    } else {
      directed <- as.character(!isSymmetric(out))
    }
  }
  attr(out, "is_list") <- is.list(out)
  attr(out, "is_directed") <- as.character(directed)
  class(out) <- "ucinet"
  
  out
}



# "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/sampson.dat" %>% 
#   read_ucinet() %>% 
#   as_igraph()
# 
# 
# "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/davis.dat" %>% 
#   read_ucinet() %>% 
#   as_igraph()
# 
# "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/newfrat.dat" %>% 
#   read_ucinet() %>% 
#   as_igraph()
# 
# "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/wiring.dat" %>% 
#   read_ucinet() %>% 
#   as_igraph()
# 
# "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/padgett.dat" %>% 
#   read_ucinet() %>% 
#   as_igraph()
