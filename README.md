
<!-- README.Rmd generates README.md. -->
snatools <a href="man/figures/logo.png"> <img src="man/figures/logo.png" align="right" height="45%" width="45%" href="man/figures/logo.png"/> </a>
==================================================================================================================================================

[![](https://img.shields.io/badge/devel%20version-0.0.0.9-red.svg)](https://github.com/knapply/snatools)

An R toolkit to bridge graph classes and streamline network analytic workflows.

Installation
------------

``` r
# Install {devtools} if you haven't already.
# install.packages("devtools")

# Install {snatools} from GitHub.
devtools::install_github("knapply/snatools")

# Load {snatools}.
library(snatools)
```

Development Test Results
------------------------

``` r
printed <- capture.output(res <- as.data.frame(devtools::test()))
#> Loading snatools
#> Testing snatools
```

``` r
res$success <- ifelse(res$failed == 0, TRUE, FALSE)

cat("Test Success Rate:", scales::percent(mean(as.numeric(res$success))))
#> Test Success Rate: 100%
```

``` r
res$test <- txt_replace_all(res$test, "\\s+", " ")
res$success <- paste0("`", res$success, "`")

res[, c("context", "success")] %>% 
  knitr::kable(caption = "Test Details")
```

| context                                    | success |
|:-------------------------------------------|:--------|
| Undirected Edge Lists                      | `TRUE`  |
| Undirected Edge Lists                      | `TRUE`  |
| Undirected Edge Lists                      | `TRUE`  |
| Undirected Edge Lists                      | `TRUE`  |
| Undirected Edge Lists                      | `TRUE`  |
| Undirected Edge Lists                      | `TRUE`  |
| Undirected Edge Lists                      | `TRUE`  |
| Undirected Edge Lists                      | `TRUE`  |
| Directed Edge Lists                        | `TRUE`  |
| Directed Edge Lists                        | `TRUE`  |
| Directed Edge Lists                        | `TRUE`  |
| Directed Edge Lists                        | `TRUE`  |
| Directed Edge Lists                        | `TRUE`  |
| Directed Edge Lists                        | `TRUE`  |
| Directed Edge Lists                        | `TRUE`  |
| Directed Edge Lists                        | `TRUE`  |
| Undirected Adjacency Matrices              | `TRUE`  |
| Undirected Adjacency Matrices              | `TRUE`  |
| Undirected Adjacency Matrices              | `TRUE`  |
| Undirected Adjacency Matrices              | `TRUE`  |
| Undirected Adjacency Matrices              | `TRUE`  |
| Directed Adjacency Matrices                | `TRUE`  |
| Directed Adjacency Matrices                | `TRUE`  |
| Directed Adjacency Matrices                | `TRUE`  |
| Directed Adjacency Matrices                | `TRUE`  |
| Directed Adjacency Matrices                | `TRUE`  |
| Undirected Attribute Edge Lists            | `TRUE`  |
| Undirected Attribute Edge Lists            | `TRUE`  |
| Directed Attribute Edge Lists              | `TRUE`  |
| Directed Attribute Edge Lists              | `TRUE`  |
| Undirected Attribute Adjacency Matrices    | `TRUE`  |
| Undirected Attribute Adjacency Matrices    | `TRUE`  |
| Directed Attribute Adjacency Matrices      | `TRUE`  |
| Directed Attribute Adjacency Matrices      | `TRUE`  |
| Undirected `attr_el`s from `attr_adj_mats` | `TRUE`  |
| Undirected `attr_el`s from `attr_adj_mats` | `TRUE`  |
| Directed `attr_el`s from `attr_adj_mats`   | `TRUE`  |
| Directed `attr_el`s from `attr_adj_mats`   | `TRUE`  |
| Undirected Mixing Matrices                 | `TRUE`  |
| Undirected Mixing Matrices                 | `TRUE`  |
| Directed Mixing Matrices                   | `TRUE`  |
| Directed Mixing Matrices                   | `TRUE`  |
| Round trip conversion: simple graph        | `TRUE`  |
| Round trip conversion: undirected graphs   | `TRUE`  |
| Round trip conversion: undirected graphs   | `TRUE`  |
| Round trip conversion: directed graphs     | `TRUE`  |
| Round trip conversion: directed graphs     | `TRUE`  |
| Round trip conversion: bipartite graphs    | `TRUE`  |
| Round trip conversion: bipartite graphs    | `TRUE`  |
| Round trip conversion: bipartite graphs    | `TRUE`  |
| E-I Index: directed                        | `TRUE`  |
| E-I Index: directed                        | `TRUE`  |
| E-I Index: undirected                      | `TRUE`  |
| E-I Index: undirected                      | `TRUE`  |
