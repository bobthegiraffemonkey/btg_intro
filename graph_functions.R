
# Pass in an n*2 matrix of points and get the RNG as an adjlist.
get_relative_neighbourhood_graph_as_adjlist = function(p){
  point_config = list(x=p[,1],
                      y=p[,2],
                      n=nrow(p),
                      window=list(x=range(p[,1]),
                                  y=range(p[,2])))
  spatgraph(point_config, "RNG")$edges
}

# Returns an edgelist where at least one incident node has id > n_filter
# In other words, it removes edges where both incident nodes are in the
# first n_filter nodes.
get_filtered_edgelist = function(adj, n_filter){
  E_out = matrix(0,
                 sum(unlist(lapply(adj, length))),
                 2)
  k=0
  for (i in 1:length(adj)){
    for (j in adj[[i]]){
      if (j > i && (i > n_filter || j > n_filter)){
        k = k+1
        E_out[k,] = c(i,j)
      }
    }
  }
  E_out[E_out[,1]>0,]
}

get_colour_cycle = function(n){
  cols = rep(0L, n)
  
  # Start with a random colour.
  cols[1] = sample(0:5, 1)
  
  # Probability to stay with colour at next edge.
  stay = 1/exp(1)
  
  # Fill the vector accordingly.
  for (i in 2:n){
    x = runif(1)
    cols[i] = cols[i-1]
    if (x > stay){
      cols[i] = cols[i] + sample (1:5, 1)
    }
  }
  
  # Cycle the vector randomly, I don't care that this is inefficient!
  for (i in sample(1:n, 1)){
    cols = cols[c(2:n, 1)]
  }
  
  # Needs to be in 1:6 at the end.
  cols%%6 + 1
}
