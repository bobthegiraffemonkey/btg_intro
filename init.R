
# Environments are modified in place, which is useful for tracking state.
# Basically, they can be used to emulate pass-by-reference. But I'm not
# sure it's worth the effort here, so I'm tracking the state with global
# variables. I know that's generally bad, but for a small personal
# project with a very visual output being ran O(1) times, I don't care.
# At least I'm passing some of it around in lists.

init = function(w, settings){
  # Things to pass out to make available in other places
  vars = list()
  
  set.seed(settings$seed)
  
  # This is just for the words, not any other edges or V.
  letter_vertices_and_edgelist = get_letter_vertices_and_edgelist(w, indents)
  n_w = nrow(letter_vertices_and_edgelist$p)
  
  x0 = min(letter_vertices_and_edgelist$p[,1])
  x1 = max(letter_vertices_and_edgelist$p[,1])
  y0 = min(letter_vertices_and_edgelist$p[,2])
  y1 = max(letter_vertices_and_edgelist$p[,2])
  
  vars$view_xlim = c(x0 - settings$view_border_width,
                     x1 + settings$view_border_width)
  vars$view_ylim = c(y0 - settings$view_border_width,
                     y1 + settings$view_border_width)

  p_random = cbind(runif(settings$n_rand,
                         x0 - settings$point_border_width,
                         x1 + settings$point_border_width),
                   runif(settings$n_rand,
                         y0 - settings$point_border_width,
                         y1 + settings$point_border_width))
  
  p = rbind(letter_vertices_and_edgelist$p,
            p_random)
  
  n = nrow(p)
  adjl = get_relative_neighbourhood_graph_as_adjlist(p)
  
  vertex_properties = c("exposed", "infected")
  V = matrix(FALSE, n, length(vertex_properties))
  colnames(V) = vertex_properties
  V[sample(n, 1), ] = TRUE
  
  m_max = sum(unlist(lapply(adjl, length))) / 2 + 
    nrow(letter_vertices_and_edgelist$E)
  
  E_props = c("v1", "v2", "id", "col_0", "col_1", "col_2", "prog", "rev_prog", "segments")
  E = matrix(0, m_max, length(E_props))
  colnames(E) = E_props
  
  # Put the edges from the letters in first, where edges (i,j) have i<j
  # Possibly stupid for large n, but n isn't large and this is neat.
  for (k in 1:n_w){
    E[k, c("v1", "v2")] = range(letter_vertices_and_edgelist$E[k,])
  }
  
  # I'm very glad n isn't large so I don't need to optimise much.
  for (i in 1:n){
    for (j in adjl[[i]]){
      if (i > j) next
      if (!any(letter_vertices_and_edgelist$E[,1] == i &
               letter_vertices_and_edgelist$E[,2] == j)){
        k = k + 1
        E[k, c("v1", "v2")] = c(i, j)
      }
    }
  }
  
  m = k
  E = E[1:k,]

  E[,"id"] = c(letter_vertices_and_edgelist$id,
               rep(0, m - length(letter_vertices_and_edgelist$id)))
  
  for (i in 0:max(E[,"id"])){
    poly = which(E[,"id"] == i)
    if (i > 0){
      rand_colours = get_colour_cycle(length(poly))
      E[poly, "col_1"] = rand_colours
      E[poly, "col_2"] = rand_colours[c(2:length(poly),1)]
    } else {
      E[poly, "col_0"] = runif(length(poly), 0.01, 0.1)
      E[poly, c("col_1", "col_2")] = 7
    }
  }

  psum = rowSums(p^2)
  D = tcrossprod(psum, matrix(1, n, 1))
  D = D + t(D) - 2*tcrossprod(p)
  diag(D) = 0
  lengths = sqrt(D[E[, c("v1", "v2")]])
  E[,"segments"] = floor(3 + 2 * lengths)

  done = FALSE

  list(data=list(E=E, V=V, done=done),
       p=p,
       vars=vars)
}
