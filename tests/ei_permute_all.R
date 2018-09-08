ei_index_permute <- function(x, vrt_attr, iterations = 1000L, diagonal = FALSE) {
  if(!class(x) %in% c("igraph", "network")) {
    stop("`x` must be of class `igraph` or `network`.", call. = FALSE)
  }
  observed_global_ei <- ei_index_global(x, vrt_attr)
  observed_grp_ei <- ei_index_grp(x, vrt_attr)
  observed_vrt_ei <- ei_index_vrt(x, vrt_attr)
  attr_adj_mat <- rep_attr_adj_mat(x, vrt_attr)
  
  vrt_names <- vrt_get_names(x)
  vrt_attrs <- vrt_get_attr(x, vrt_attr)
  
  global_permuted_eis <- vector("double", iterations)
  
  # n_rows <- length(unique(rownames(attr_adj_mat))) * iterations
  grp_ei_df <- data.frame(iteration = integer(),
                          attribute = character(),
                          external_ties = integer(),
                          internal_ties = integer(),
                          ei_index = double(),
                          stringsAsFactors = FALSE)
  
  vrt_ei_df <- data.frame(iteration = integer(),
                          name = character(),
                          attribute = character(),
                          external_ties = integer(),
                          internal_ties = integer(),
                          ei_index = double(),
                          stringsAsFactors = FALSE)
  for(i in seq_len(iterations)) {
    permuted_matrix <- permute_matrix(attr_adj_mat)
    if(net_is_directed(x)) {
      permuted_matrix[upper.tri(permuted_matrix)] <- NA_integer_
    }
    if(!diagonal) {
      diag(permuted_matrix) <- NA_integer_
    }
    permuted_attr_el <- rep_attr_el(permuted_matrix)
    global_permuted_eis[[i]] <- ei_index_global(permuted_attr_el)
    
    permuted_grp_df <- ei_index_grp(permuted_attr_el)
    permuted_grp_df$iteration <- i
    grp_ei_df <- rbind.data.frame(grp_ei_df, permuted_grp_df)
    
    permuted_vrt_df <- ei_index_vrt.attr_adj_mat(permuted_matrix, 
                                                 vrt_names = vrt_names, 
                                                 vrt_attrs = vrt_attrs)
    permuted_vrt_df$iteration <- i
    vrt_ei_df <- rbind.data.frame(vrt_ei_df, permuted_vrt_df)
  }
  
  out <- list(vrt_attr = vrt_attr,
              observed_global_ei = observed_global_ei,
              observed_group_ei = observed_grp_ei,
              observed_vertex_ei = observed_vrt_ei,
              iterations = iterations,
              global_permuted_eis = permuted_eis,
              group_permuted_eis = grp_ei_df,
              vertex_permuted_eis = vrt_ei_df,
              n_greater = length(which(permuted_eis >= observed_ei)),
              prop_greater = mean(as.numeric(permuted_eis >= observed_ei)),
              n_lesser = length(which(permuted_eis < observed_ei)),
              prop_lesser = mean(as.numeric(permuted_eis < observed_ei)))
  class(out) <- "ei_index_global_permute"

  out
}
data("samplk", package = "ergm")
test <- ei_index_permute(samplk1, "group", iterations = 2000)

test$group_permuted_eis %>% 
  ggplot(aes(x = attribute, y = ei_index)) +
  stat_ydensity() +
  coord_flip() +
  geom_point(aes(x = attribute, y = ei_index, color = "Observed E-I Indices"),
             data = test$observed_group_ei) +
  guides(color = guide_legend(NULL))


test$vertex_permuted_eis %>% 
  ggplot(aes(x = ei_index)) +
  stat_density(aes(fill = name), show.legend = FALSE) +
  geom_vline(aes(xintercept = ei_index, color = "Observed"),
             data = test$observed_vertex_ei) +
  guides(color = guide_legend(NULL)) +
  # scale_fill_b() +
  facet_wrap(~ name, nrow = 3) +
  theme_sna() +
  theme(panel.border = element_rect(color = "gray", fill = "transparent")) +
  labs(x = "E-I Index", y = "Density", title = "Homophily", subtitle = "Sampson's Monastery")
  

library(snatools)

# packages
library(tidygraph, warn.conflicts = FALSE) # non-standard evaluation 
library(ggraph, quietly = TRUE) 
library(purrr)

# data
data(samplk, package = "ergm")

# construct a graph
g <- samplk1 %>% # assignment, lazy evaluation via the pipe
  snatools::as_igraph() %>% # namespaces
  as_tbl_graph() %E>% # a different pipe
  mutate(tie_type = ifelse(.N()$group[from] == .N()$group[to], # named-list extraction, vector subsetting
                           "Internal Ties", "External Ties"))

coords <- g %>% 
  igraph::layout_with_fr() %>% # homogenous matrix
  as.data.frame() %>% # conversion to heterogenous data.frame
  `colnames<-`(c("x", "y")) # named objects

plotter <- function(graph, main, color) { # functions
  g <- create_layout(graph, coords) # environments, scoping (we're not overwriting g in global environment)
  
  ggraph(g) + # all of this is completely foreign unless user familiar with ggplot
    geom_edge_fan(colour = color,
                  end_cap = circle(3, "mm"),
                  arrow = arrow(length = unit(2, "mm"), type = "closed"),
                  show.legend = FALSE) +
    geom_node_point(aes(fill = group), 
                    shape = 21, color = "transparent",
                    size = 5, show.legend = FALSE) +
    scale_edge_color_manual(values = c("lightblue", "salmon")) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = main)
}

graphs <- list(g,
               g %E>% filter(tie_type == "Internal Ties"),
               g %E>% filter(tie_type == "External Ties"))

titles <- c("All Ties", "Internal Ties", "External Ties")     

edge_colors <- c("lightgray", "lightblue", "salmon")

plots <- list(graphs, titles, edge_colors) %>% 
  pmap(plotter) # object iteration: iterating through three lists of three

# rough equivelent in base: ====
# mapply(function(x, y, z) {
#   plotter(x, y, z)
#   }, graphs, titles, edge_colors, 
#   SIMPLIFY = FALSE, USE.NAMES = FALSE)


plots %>% walk(plot) # side-effect iteration

# equivelent in base:
# for (i in seq_along(plots)) {
  # plot(plots[[i]])
# }


reprex::reprex(opts_chunk = list(message = FALSE, warning = FALSE, 
                                 fig.align = "center"),
               venue = "r",
               advertise = FALSE)


