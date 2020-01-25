
# Set up some colours to refer to. Tweak them for dev if it's late
# and f.lux has kicked in.
if (settings$f.lux){
  colours=matrix(c(.9,.9,0,
                   1,1,1,
                   1,0,0,
                   1,.5,0,
                   0,1,0, 
                   0.2,0.2,1,
                   0.4,0.4,0.4),
                 ncol=3,
                 byrow = TRUE)
} else {
  colours=matrix(c(1,1,0,
                   1,1,1,
                   1,0,0,
                   1,.5,0,
                   0,1,0,
                   0,0,1,
                   0.4,0.4,0.4),
                 ncol=3,
                 byrow = TRUE)
}


# Set up some data and stuff.
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
  
  E_props = c("v1", "v2", "id", "col_0", "col_1", "col_2", "fwd_prog", "rev_prog", "segments", "lwd")
  E = matrix(0, m_max, length(E_props))
  colnames(E) = E_props
  
  # Put the edges from the letters in first.
  E[1:n_w, c("v1", "v2")] = letter_vertices_and_edgelist$E[1:n_w,]
  
  # I'm very glad n isn't large so I don't need to optimise much.
  k = n_w
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
  
  # E was made too large, so only keep the rows with actual edges.
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
  
  E[E[, "id"] == 0, "lwd"] = settings$bg_lwd
  E[E[, "id"] > 0, "lwd"] = settings$text_lwd

  E_col = list()
  for (i in 1:m){
    E_col[[i]] = get_col_segs(E[i,"segments"], E[i,"col_1"], E[i,"col_2"])
  }

  E_p = list()
  for (i in 1:m){
    E_p[[i]] = get_segs(p[E[i, c("v1", "v2")], ], E[i, "segments"])
  }

  done = FALSE

  # Return a list of stuff, including lists.
  list(data=list(E=E,
                 V=V,
                 done=done),
       E_aux=list(E_col=E_col,
                  E_p=E_p),
       p=p,
       vars=vars)
}
