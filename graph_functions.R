
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
draw_graph = function(p, undir_edges, dir_edges, vars, dev=T){
  if (dev){
    graphics.off()
    x11(width=settings$win_width,
        height=settings$win_height)
  }
  
  par(mar=rep(0, 4),
      mgp=c(0,0,0),
      lwd=settings$line_width,
      bg="black")
  
  plot(NULL,
       xlim=vars$view_xlim,
       ylim=vars$view_ylim)
  
  abline(v=min(p[,1]) - settings$view_border_width)
  abline(v=max(p[,1]) + settings$view_border_width)
  abline(h=min(p[,2]) - settings$view_border_width)
  abline(h=max(p[,2]) + settings$view_border_width)
  
  m = nrow(undir_edges)
  for (i in 1:m){
    e = p[undir_edges[i, c("v1", "v2")],]
    if (undir_edges[i, "hide"] == 0){
      lines(e, col=get_colour(undir_edges[i, "col"]))
    }
    tot_segs = undir_edges[i,"segments"]
    for (j in c(i, i+m)){
      if (j == i) order = 1:2 else order = 2:1
      pro_segs = dir_edges[j, "progress"]
      if (pro_segs > 0){
        p_e = matrix(0, progress+1, 2)
        segs = get_segs(pro_segs, tot_segs)
        for (ii in 1:2) p_e[,order[ii]] = segs * (e[2,ii]-e[1,ii]) + e[1,ii]
        cols = get_colour_inter(segs, dir_edges[i,"col1"], dir_edges[i,"col2"])
        for (jj in 1:pro_segs){
          lines(e[(jj-1):jj, 1], e[(jj-1):jj, 2], col=cols[jj])
        }
      }
    }
  }
}


if (settings$f.lux){
  colours=matrix(c(.9,.9,0,
                   1,1,1,
                   1,0,0,
                   1,.5,0,
                   0,1,0, 
                   0.2,0.2,1),
                 ncol=3,
                 byrow = TRUE)
} else {
  colours=matrix(c(1,1,0,
                   1,1,1,
                   1,0,0,
                   1,.5,0,
                   0,1,0,
                   0,0,1),
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



get_segs = function(n, progress){
  (0:progress)/n
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


update_state = function(undir_edges, dir_edges, vertices){
  infection_rate = 0.42
  n = nrow(vertices)
  m = nrow(undir_edges)
  vertices$infected = vertices$infected || (vertices$exposed && runif(n) < infection_rate)
  v_inf = which(vertices$infected)
  e_inf = which(undir_edges[c("v1", "v2")] %in% v_inf)
  
  dir_edges[e_inf, "progress"] = max(dir_edges[e_inf, "progress"] + 1,
                                     dir_edges[e_inf, "segments"])
  met = which(dir_edges[1:m, "progress"] + dir_edges[(m+1):(2*m), "progress"] >= dir_edges[1:m, "segments"])
  dir_edges[met, "progress"] = dir_edges[met + m, "progress"] = dir_edges[met, "segments"]
  undir_edges[met, "hide"] = TRUE
  complete = which(dir_edges$progress == dir_edges$segments)
  vertices$exposed[dir_edges[complete, "v2"]] = TRUE
  done = length(complete) == 2*m

  list(dir_edges=dir_edges,
       undir_edges=undir_edges,
       vertices=vertices,
       done=done)
}

