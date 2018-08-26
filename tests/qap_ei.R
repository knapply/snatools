`%>%` <- magrittr::`%>%`
data("samplk", package = "ergm")   # data =====================================================
g1 <- samplk1
g2 <- samplk2

qap_cor <- function(g1, g2, iterations = 10000L, diagonal = FALSE) { # QAP Correlation =========
  mat1 <- network::as.matrix.network.adjacency(g1)
  if(!diagonal) {
    diag(mat1) <- NA_integer_
  }
  vectorized_mat1 <- as.vector(mat1)
  
  mat2 <- network::as.matrix.network.adjacency(g2)
  if(!diagonal) {
    diag(mat2) <- NA_integer_
  }
  
  test_rho <- cor(vectorized_mat1, as.vector(mat2), use = "complete.obs")
  
  permuted_rhos <- vector("numeric", iterations)
  for(i in seq_len(iterations)) {
    permuted_matrix <- mat2[sample(nrow(mat2)), sample(ncol(mat2))]
    if(!diagonal) {
      diag(permuted_matrix) <- NA_integer_
    }
    permuted_rhos[[i]] <- cor(vectorized_mat1, as.vector(permuted_matrix), 
                              use = "complete.obs")
  }
  temp_out <- paste0( # return is only temporary until the most useful format is decided =======
    "rho: ", round(test_rho, 6), "\n",
    "permuted rhos >= rho: ", mean(as.numeric(permuted_rhos >= test_rho)), "\n",
    "permuted rhos < rho: ", mean(as.numeric(permuted_rhos < test_rho)), "\n\n", 
    sep = "")
  
  temp_out
}

qap_cor(g1, g2) 

# %>% cat() # results ============================================================

sna::gcor(list(g1, g2), g1 = 1, g2 = 2)                 # {sna}'s version =====================
sna::qaptest(list(g1, g2), sna::gcor, g1 = 1, g2 = 2)   # {sna}'s version =====================

# performance comparison =======================================================================
benchmarks <- bench::mark(
  qap_cor(g1, g2),
  sna::qaptest(list(g1, g2), sna::gcor, g1 = 1, g2 = 2), 
  iterations = 50, check = FALSE
)

benchmarks[, names(benchmarks) %in% c("expression", "mean", "median")]

ggplot2::autoplot(benchmarks)

reprex::reprex(opts_chunk = list(message = FALSE, warning = FALSE))

# qap_cor <- function(g1, g2, iterations = 1000L, diagonal = FALSE) { # QAP Correlation =========
#   mat1 <- as.matrix.network.adjacency(g1)
#   if(!diagonal) {
#     diag(mat1) <- NA_integer_
#   }
#   vectorized_mat1 <- as.vector(mat1)
#   
#   mat2 <- as.matrix.network.adjacency(g2)
#   if(!diagonal) {
#     diag(mat2) <- NA_integer_
#   }
#   
#   test_rho <- cor(vectorized_mat1, as.vector(mat2), use = "complete.obs")
#   
#   permuted_rhos <- vector("numeric", iterations)
#   for(i in seq_len(iterations)) {
#     permuted_matrix <- mat2[sample(nrow(mat2)), sample(ncol(mat2))]
#     if(!diagonal) {
#       diag(permuted_matrix) <- NA_integer_
#     }
#     permuted_rhos[[i]] <- cor(vectorized_mat1, as.vector(permuted_matrix), 
#                               use = "complete.obs")
#   }
#   temp_out <- paste0( # return is only temporary until the most useful format is decided =======
#     "rho: ", test_rho, "\n",
#     "percent of permuted rhos >= rho: ", mean(as.numeric(permuted_rhos >= test_rho)), "\n",
#     "percent of permuted rhos < rho: ", mean(as.numeric(permuted_rhos < test_rho)), "\n\n", 
#     sep = "")
#   
#   temp_out
# }






rep_as_adjacency_matrix(graph("Zachary"))


qap_cor(g1, g2, iterations = 20)$permuted_rhos %>% hist()
qaptest(list(g1, g2), gcor, reps = 20, g1 = 1, g2 = 2)$dist %>% hist()

bench::mark(
  qap_cor(g1, g2),
  qaptest(list(g1, g2), gcor, g1 = 1, g2 = 2),
  iterations = 10,
  check = FALSE
) %>% 
  ggplot2::autoplot()



function (exchange.list) {
    n <- length(exchange.list)
    grp <- unique(exchange.list)
    o <- 1:n
    for (i in grp) {
        v <- (1:n)[exchange.list == i]
        if (length(v) > 1) 
            o[v] <- sample(v)
    }
    o
}




qaptest(list(samplk1, samplk2), gcor, g1 = 1, g2 = 2)

qap_ei <- function(g1, g2, vrt_attr, iterations = 1000L) {
  mat1 <- unclass(mix_mixing_matrix(g1, vrt_attr))
  # diag(mat1) <- NA_integer_
  mat2 <- unclass(mix_mixing_matrix(g2, vrt_attr))
  # diag(mat2) <- NA_integer_
  target_rho <- cor(as.vector(mat1), as.vector(mat2))
  
  distances <- vector("numeric", iterations)

  for(i in seq_len(iterations)) {
    permuted <- mat2[sample(nrow(mat2)), sample(ncol(mat2))]
    permuted_rho <- cor(as.vector(mat1), as.vector(permuted))
    distances[[i]] <- permuted_rho
  }
  
  list(n_greater_than_or_equal_to_target_rho = mean(as.numeric(dist >= target_rho)),
       n_lesser_than_target_rho = mean(as.numeric(dist < target_rho)))
}
qap_ei(g1, g2, "group")





mat1 <- as.matrix.network.adjacency(samplk1)
mat2 <- as.matrix.network.adjacency(samplk2)
cor(mat1[!is.na(mat1)], mat2[!is.na(mat2)])

samplk1 %>% mix_ei_index("group")
samplk1 %>% 
  mix_mixing_matrix("group") %>% 
  as.data.frame(stringsAsFactors = FALSE)

samplk2 %>% mix_ei_index("group")
samplk2 %>% mix_mixing_matrix("group")

samplk3 %>% mix_ei_index("group")
samplk3 %>% mix_mixing_matrix("group")

mats <- lapply(list(samplk1, samplk2, samplk3),
       mix_mixing_matrix, "group") %>% 
  as.array() %>% 
  lapply(`class<-`, "matrix")

mat1 <- mats[[1]]
mat2 <- mats[[3]]

# mat1 <- samplk1 %>% as.matrix.network.adjacency()
# mat2 <- samplk2 %>% as.matrix.network.adjacency()

samplk1 %>% 
  as.matrix.network.adjacency()


arr[1, , ] <- mats[[1]]
arr[2, , ] <- mats[[2]]
arr[3, , ] <- mats[[3]]

gcor(samplk1, samplk3)

qaptest(list(samplk1, samplk3), gcor, g1 = 1, g2 = 2) 

x <- samplk1
vrt_attr <- "group"

attr_adj_matrix <- function(x, vrt_attr) {
  attrs <- network::get.vertex.attribute(x, vrt_attr)
  cats <- sort(unique(attrs))
  # el <- network::as.matrix.network.edgelist(x)
  adj_mat <- as.matrix.network.adjacency(x)
  if(nrow(adj_mat) == 0L) {
    message("`x` does not have any edges.")
  }
  rownames(adj_mat) <- attrs
  colnames(adj_mat) <- attrs
  # from <- factor(attrs[el[, 1]], levels = cats)
  # to <- factor(attrs[el[, 2]], levels = cats)
  
  # table(from, to)
  adj_mat
}

gcor(mat1, mat2)
cancor(c(mat1), c(mat2))

# mat1 <- attr_adj_matrix(samplk1, "group")
# mat2 <- attr_adj_matrix(samplk2, "group")
# mat1 <- `class<-`(mix_mixing_matrix(samplk1, "group"), "matrix")
# mat2 <- `class<-`(mix_mixing_matrix(samplk2, "group"), "matrix")
mat1 <- as.matrix.network.adjacency(samplk1)
# mat1 == mat2
mat2 <- as.matrix.network.adjacency(samplk2)
out <- list()
dist <- vector("numeric", 1000L)
test_val <- cor(as.vector(mat1), as.vector(mat2))

for(i in seq_len(1000L)) {
  permuted <- mat2[sample(nrow(mat2)), sample(ncol(mat2))]
  mat_rho <- gcor(mat1, permuted)
  dist[i] <- mat_rho
}
pgreq <- mean(as.numeric(dist >= test_val))
pleeq <- mean(as.numeric(dist <= test_val))


dat <- samplk1
dat2 <- samplk2
gcor(dat, dat2)

mat1 <- as.matrix.network.adjacency(samplk1)
mat2 <- as.matrix.network.adjacency(samplk2)
cor(mat1[!is.na(mat1)], mat2[!is.na(mat2)])


g1 <- samplk1
g2 <- samplk2
qap_cor <- function(g1, g2) {
  mat1 <- as.matrix.network.adjacency(g1)
  mat2 <- as.matrix.network.adjacency(g2)
  
  cor(mat1[!is.na(mat1)], mat2[!is.na(mat2)])
}


diag(mat1) <- NA_integer_
# mat1[!diag(mat1)]
mat2 <- as.matrix.network.adjacency(samplk2)
diag(mat2) <- NA_integer_
cor(as.vector(mat1), as.vector(mat2), use = "complete.obs")

cor(mat1[!is.na(mat1)], mat2[!is.na(mat2)])



empty <- matrix(nrow = nrow(as.matrix.network.adjacency(samplk1)),
                ncol = nrow(as.matrix.network.adjacency(samplk2)))
d[g1[i], , ]

all(`diag<-`(d[g1[i], , ], 0) == mat1)

for (i in 1:gn1) {
      for (j in 1:gn2) {
        empty[i, j] <- cor(as.vector(d[g1[i], , ]),
                           as.vector(d[g2[j], , ]),
                           use = "complete.obs")
      }
    }

function (dat, dat2 = NULL, g1 = NULL, g2 = NULL, diag = FALSE, 
    mode = "digraph") {
    dat <- as.sociomatrix.sna(dat)
    if (is.list(dat)) 
        stop("Identical graph orders required in gcor.")
    if (!is.null(dat2)) {
        dat2 <- as.sociomatrix.sna(dat2)
        if (is.list(dat2)) {
            stop("Identical graph orders required in gcor.")
        }
    }
    if (is.null(g1)) {
        g1 <- 1:dim(dat)[1]
    }
    if (is.null(g2)) {
        g2 <- 1:dim(dat)[1]
    }
    if (!is.null(dat2)) {
        if (length(dim(dat)) > 2) 
            temp1 <- dat
        else {
            temp1 <- array(dim = c(1, dim(dat)[2], dim(dat)[2]))
            temp1[1, , ] <- dat
        }
        if (length(dim(dat2)) > 2) 
            temp2 <- dat2
        else {
            temp2 <- array(dim = c(1, dim(dat2)[2], dim(dat2)[2]))
            temp2[1, , ] <- dat2
        }
        if (dim(temp1)[2] > dim(temp2)[2]) 
            temp2 <- add.isolates(temp2, dim(temp1)[2] - dim(temp2)[2])
        if (dim(temp2)[2] > dim(temp1)[2]) 
            temp1 <- add.isolates(temp1, dim(temp2)[2] - dim(temp1)[2])
        n <- dim(temp1)[2]
        gn <- dim(temp1)[1] + dim(temp2)[1]
        gn1 <- dim(temp1)[1]
        gn2 <- dim(temp2)[1]
        d <- array(dim = c(gn, n, n))
        d[1:gn1, , ] <- temp1
        d[(gn1 + 1):(gn2 + gn1), , ] <- temp2
        g1 <- 1:gn1
        g2 <- (gn1 + 1):(gn1 + gn2)
    } else {
        d <- dat
        n <- dim(dat)[2]
        gn <- dim(dat)[1]
        gn1 <- length(g1)
        gn2 <- length(g2)
    }
    if (!diag) {
        d <- diag.remove(d)
    }
    if (mode == "graph") {
        d <- upper.tri.remove(d)
    }
    gd <- matrix(nrow = gn1, ncol = gn2)
    rownames(gd) <- g1
    colnames(gd) <- g2
    for (i in 1:gn1) {
      for (j in 1:gn2) {
        gd[i, j] <- cor(as.vector(d[g1[i], , ]),
                        as.vector(d[g2[j], , ]),
                        use = "complete.obs")
      }
    }
    if ((gn1 == 1) & (gn2 == 1)) 
        gd[1, 1]
    else gd
}








