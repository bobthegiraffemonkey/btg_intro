
require('spatgraphs')

# n = 500
# a = 1
# b = 1/a
# p = cbind(runif(n,0,a), runif(n,0,b))

get_relative_neighbourhood_graph_as_adjlist = function(p){
  point_config = list(x=p[,1],
                      y=p[,2],
                      n=nrow(p),
                      window=list(x=range(p[,1]),
                                  y=range(p[,2])))
  spatgraph(point_config, "RNG")$edges
}
