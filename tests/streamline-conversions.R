ig_undir <- graph("Zachary") %>% 
  clean_graph()

ig_undir %>% igraph::as_data_frame() %>% as_tibble()

ig_undir %>% 
  as_network() %>% 
  as_igraph() %>% 
  igraph::as_data_frame() %>% 
  as_tibble()

ig_undir %>% 
  igraph::as_data_frame("vertices") %>% 
  as_tibble()

ig_undir %>% 
  as_network() %>% 
  as_igraph() %>% 
  igraph::as_data_frame("vertices") %>% 
  as_tibble()

ig_undir %>% 
  edg_attrs()

ig_undir %>% 
  as_network() %>% #edg_attrs()
  as_igraph() %>% 
  edg_attrs()
  igraph::graph_attr_names()
  
graph("Zachary") %>% 
  clean_graph() %>% 
  edg_attrs()
  
  
x <- ig_undir
