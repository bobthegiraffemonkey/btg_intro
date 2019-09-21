
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
      if (i > n_filter || j > n_filter){
        k = k+1
        E_out[k,] = c(i,j)
      }
    }
  }
  E_out[E_out[,1]>0,]
}
