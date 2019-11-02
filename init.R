
# Environments are modified in place, which is useful for tracking state.
# Basically, they can be used to emulate pass-by-reference. But I'm not
# sure it's worth the effort here, so I'm tracking the state with global
# variables. I know that's generally bad, but for a small personal
# project with a very visual output being ran O(1) times, I don't care.

init = function(w){
  letter_vertices_and_edgelist = get_letter_vertices_and_edgelist(w, indents)
  n_w = nrow(letter_vertices_and_edgelist$p)
  
  x0 = min(letter_vertices_and_edgelist$p[,1])
  x1 = max(letter_vertices_and_edgelist$p[,1])
  y0 = min(letter_vertices_and_edgelist$p[,2])
  y1 = max(letter_vertices_and_edgelist$p[,2])
  
  p_random = cbind(runif(n_rand,
                         x0 - point_border_width,
                         x1 + point_border_width),
                   runif(n_rand,
                         y0 - point_border_width,
                         y1 + point_border_width))
  
  p = rbind(letter_vertices_and_edgelist$p,
            p_random)
  
  n = nrow(p)
  adjl = get_relative_neighbourhood_graph_as_adjlist(p)
  
  vertex_properties = c("infected")
  vertices = matrix(0, n, length(vertex_properties))
  
  m_max = sum(unlist(lapply(adjl, length))) / 2 + 
    nrow(letter_vertices_and_edgelist$E)
  
  undir_edge_properties = c("v1", "v2", "id", "col", "length", "segments", "show")
  dir_edge_properties = c("v1", "v2", "col1", "col2", "progress")
  undir_edges = matrix(0, m_max, length(undir_edge_properties))
  
  # Put the edges from the letters in first, where edges (i,j) have i<j
  # Possibly stupid for large n, but n isn't large and this is neat.
  for (k in 1:n_w){
    undir_edges[k, 1:2] = range(letter_vertices_and_edgelist$E[k,])
  }
  
  # I'm very glad n isn't large so I don't need to optimise much.
  for (i in 1:n){
    for (j in adjl[[i]]){
      if (i > j) next
      if (!any(letter_vertices_and_edgelist$E[,1] == i &
               letter_vertices_and_edgelist$E[,2] == j)){
        k = k + 1
        undir_edges[k, 1:2] = c(i, j)
      }
    }
  }
  
  m = k
  
  undir_edges = undir_edges[1:k,]
  dir_edges = matrix(0, 2*m, length(dir_edge_properties))
  dir_edges[1:m, 1:2] = dir_edges[1:m + m, 2:1] = undir_edges[, 1:2]
  
  colnames(undir_edges) = undir_edge_properties
  colnames(dir_edges) = dir_edge_properties
  
  undir_edges[,"id"] = c(letter_vertices_and_edgelist$id,
                         rep(0, m - length(letter_vertices_and_edgelist$id)))
  
  for (i in 1:max(undir_edges[,"id"])){
    poly = which(undir_edges[,"id"] == i)
    rand_colours = get_colour_cycle(length(poly))
    dir_edges[poly, "col1"] = rand_colours
    dir_edges[poly, "col2"] = rand_colours[c(2:length(poly),1)]
    dir_edges[poly + m, "col2"] = rand_colours
    dir_edges[poly + m, "col1"] = rand_colours[c(2:length(poly),1)]
    undir_edges[poly, "col"] = rgb(1,1,1)
  }
  
  list(dir_edges, undir_edges, vertices, p)
}





