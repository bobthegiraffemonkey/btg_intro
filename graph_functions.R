
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


# Pass in vertex and edge data and draw the graph.
draw_graph = function(p, E, vars, dev=T){
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
    e = p[E[i, c("v1", "v2")],]
    tot_segs = E[i, "segments"]
    if (E[i, "id"] == 0){
      lwd = settings$bg_lwd
    } else {
      lwd = settings$text_lwd
    }
    # Each of forward and reverse progress being done imply the other.
    if (E[i, "prog"] < tot_segs){
      lines(e, col=get_colour(E[i, "col_0"]), lwd=lwd)
    }
    draw_dir_edge(e, E[i, "prog"], tot_segs, E[i,"col_1"], E[i, "col_2"], lwd)
    if (E[i, "prog"] < tot_segs){
      draw_dir_edge(e[2:1,], E[i, "rev_prog"], tot_segs, E[i,"col_2"], E[i, "col_1"], lwd)
    }
  }
}


draw_dir_edge = function(e, progress, total, col_1, col_2, lwd){
  if (progress > 0){
    p_e = matrix(0, progress+1, 2)
    segs = get_segs(progress, total)
    for (i in 1:2) p_e[, i] = segs * (e[2, i] - e[1, i]) + e[1, i]
    cols = get_colour_inter(segs, col_1, col_2)
    for (j in 1:progress){
      lines(p_e[j:(j+1), 1], p_e[j:(j+1), 2], col=cols[j], lwd=lwd)
    }
  }
}


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


get_colour = function(n){
  if (n < 1){
    grey(n)
  } else {
    rgb(colours[n, 1], colours[n, 2], colours[n, 3])
  }
}


get_colour_inter = function(segs, c1, c2){
  out = vector(mode="character", length(segs))
  for (i in seq_along(out)){
    s = segs[i]
    colour = colours[c1,] * (1-s) + colours[c2,] * s
    out[i] = rgb(colour[1], colour[2], colour[3])
  }
  out
}


get_segs = function(progress, total){
  (0:progress)/total
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


update_state = function(E, V){
  infection_rate = 0.42
  n = nrow(V)
  m = nrow(E)
  V[,"infected"] = V[,"infected"] | (V[,"exposed"] & runif(n) < infection_rate)
  v_inf = which(V[,"infected"])

  e_update = which((E[,"prog"] < E[,"segments"]) & (E[,"v1"] %in% v_inf))
  E[e_update, "prog"] = E[e_update, "prog"] + 1
  e_update = which((E[,"rev_prog"] < E[,"segments"]) & (E[,"v2"] %in% v_inf))
  E[e_update, "rev_prog"] = E[e_update, "rev_prog"] + 1

  complete = (E[,"prog"] + E[,"rev_prog"]) >= E[,"segments"]
  E[complete, "prog"] = E[complete, "rev_prog"] = E[complete, "segments"]
  V[c(E[complete, c("v1", "v2")]), "exposed"] = TRUE
  done = all(complete)

  list(E=E,
       V=V,
       done=done)
}

