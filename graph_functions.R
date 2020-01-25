
# Pass in an n*2 matrix of points and get the RNG as an adjlist.
get_relative_neighbourhood_graph_as_adjlist = function(p){
  point_config = list(x=p[,1],
                      y=p[,2],
                      n=nrow(p))
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


# Pass in vertex and edge data and draw the graph.
draw_graph = function(p, E, E_aux, vars, settings, dev=FALSE){
  if (dev){
    graphics.off()
    x11(width=settings$win_width,
        height=settings$win_height)
  }
  
  par(mar=rep(0, 4),
      mgp=c(0,0,0),
      bg="black")
  
  plot(NULL,
       xlim=vars$view_xlim,
       ylim=vars$view_ylim)
  
  abline(v=vars$view_xlim[1])
  abline(v=vars$view_xlim[2])
  abline(h=vars$view_ylim[1])
  abline(h=vars$view_ylim[2])
  
  m = nrow(E)
  for (i in 1:m){
    tot_segs = E[i, "segments"]
    
    # Each of forward and reverse progress being done imply the other,
    # we draw the base edge if neither are complete.
    if (E[i, "fwd_prog"] < tot_segs){
      lines(p[E[i, c("v1", "v2")],], col=get_colour(E[i, "col_0"]), lwd=E[i, "lwd"])
    }
    
    # Draw the directed edge in each direction.
    draw_dir_edges(E[i, "fwd_prog"], E[i, "rev_prog"], tot_segs, E_aux$E_p[[i]], E_aux$E_col[[i]], lwd=E[i, "lwd"])
  }
}


draw_dir_edges = function(fwd_prog, rev_prog, tot_segs, segs_p, segs_col, lwd){
  if (fwd_prog > 0){
    for (i in 1:fwd_prog){
      lines(segs_p[i:(i+1),], col=segs_col[i], lwd=lwd)
    }
  }
  if ((fwd_prog < tot_segs) && (rev_prog > 0)){
    for (i in tot_segs:(tot_segs - rev_prog + 1)){
      lines(segs_p[i:(i+1),], col=segs_col[i], lwd=lwd)
    }
  }
}


get_colour = function(n){
  if (n < 1){
    grey(n)
  } else {
    rgb(colours[n, 1], colours[n, 2], colours[n, 3])
  }
}


get_col_segs = function(n_segs, c1, c2){
  segs = (0:n_segs) / n_segs
  out = vector(mode="character", n_segs)
  if (c1 < 1){
    out = rep(grey(c1), segs)
  } else {
    for (i in seq_along(out)){
      s = segs[i]
      colour = colours[c1,] * (1-s) + colours[c2,] * s
      out[i] = rgb(colour[1], colour[2], colour[3])
    }
  }
  out
}


get_segs = function(e, n_segs){
  segs = (0:n_segs) / n_segs
  p_e = matrix(0, n_segs+1, 2)
  for (i in 1:2) p_e[, i] = segs * (e[2, i] - e[1, i]) + e[1, i]
  p_e
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


update_state = function(E, V, settings){
  # If a vertex is exposed, it gets randomly infected at the given rate.
  # Get a list of infected vertices.
  V[,"infected"] = V[,"infected"] |
                  (V[,"exposed"] & (runif(nrow(V)) < settings$infection_rate))
  v_inf = which(V[,"infected"])

  # Update forward progress then backward progress.
  e_update_fwd = which((E[,"fwd_prog"] < E[,"segments"]) & (E[,"v1"] %in% v_inf))
  e_update_rev = which((E[,"rev_prog"] < E[,"segments"]) & (E[,"v2"] %in% v_inf))
  E[e_update_fwd, "fwd_prog"] = E[e_update_fwd, "fwd_prog"] + 1
  E[e_update_rev, "rev_prog"] = E[e_update_rev, "rev_prog"] + 1

  # Tidy up complete edges, make sure both incident vertices are considered exposed.
  complete = (E[,"fwd_prog"] + E[,"rev_prog"]) >= E[,"segments"]
  E[complete, "fwd_prog"] = E[complete, "rev_prog"] = E[complete, "segments"]
  V[c(E[complete, c("v1", "v2")]), "exposed"] = TRUE
  
  done = all(complete)

  list(E=E,
       V=V,
       done=done)
}

