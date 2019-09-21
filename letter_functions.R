
# We want the sensible circle constant here!
tau = 2 * pi

add_points = list("p" = matrix(0, 0, 2),
                  "xlim" = c(0, 0),
                  "ylim" = c(0, 0))

make_fraccircle = function(x, y, t1, t2, r, n, sd.adj=0.2){
  sd = sd.adj * abs((t2 - t1)/n)
  t = seq(t1 * tau, t2 * tau, length = n + 1)
  t = t + rnorm(n + 1, 0, sd)
  if ((t2 - t1) %% 1 == 0){
    t = t[1:n]
    t = t + runif(1, 0, tau/n)
  }
  add_points$p = cbind(cos(t)*r + x, sin(t)*r + y)
  t1_axis = ceiling(t1*4)/4
  t2_axis = floor(t2*4)/4
  if (t1_axis <= t2_axis){
    lims_t = c(t1, t2, seq(t1_axis, t2_axis, 0.25))
  } else {
    lims_t = c(t1, t2)
  }
  add_points$xlim = range(cos(tau * lims_t)*r + x)
  add_points$ylim = range(sin(tau * lims_t)*r + y)
  add_points
}

make_semicircle = function(x1, y1, x2, y2, n, ccw=1){
  x = (x1 + x2)/2
  y = (y1 + y2)/2
  r = abs((x2 - x1)/2) + abs((y2 - y1)/2)
  if (x1 > x2) t1 = 0
  if (x1 < x2) t1 = 0.5
  if (y1 > y2) t1 = 0.25
  if (y1 < y2) t1 = 0.75
  t2 = t1 + 0.5 * ccw
  make_fraccircle(x, y, t1, t2, r, n)
}

make_point = function(adj, x, y){
  jitter = 0.03
  add_points$xlim=c(x, x)
  add_points$ylim=c(y, y)
  if (adj == "h" || adj == "o") x = x + rnorm(1, 0, jitter)
  if (adj == "v" || adj == "o") y = y + rnorm(1, 0, jitter)
  add_points$p = c(x, y)
  add_points
}

get_letter_outline = function(l){
  outline_raw = outlines[[l]]
  if (is.null(outline_raw)) outline_raw = outlines$o # TODO: remove
  outline_raw = str_split(str_trim(outline_raw), "\n", simplify = T)
  outline_raw = c(outline_raw, "n")
  
  poly_list = list()
  p = matrix(0,0,2)
  k = 0
  xlim = ylim = c()
  for (i in 1:length(outline_raw)){
    line = str_split(str_trim(outline_raw[i]), " ", simplify = T)
    type = line[1]
    p_num = as.numeric(line[-1])
    if (type == "f") p_add = do.call("make_fraccircle", as.list(p_num))
    if (type == "s") p_add = do.call("make_semicircle", as.list(p_num))
    if (type %in% c("v", "h", "o")) p_add = do.call("make_point", list(type, p_num[1], p_num[2]))
    if (type == "n"){
      k = k + 1
      poly_list[[k]] = p
      p = matrix(0,0,2)
    } else {
      p = rbind(p, p_add$p)
      xlim = range(xlim, p_add$xlim)
      ylim = range(ylim, p_add$ylim)
      # print(xlim); print(ylim)
      # print(p_add$p)
    }
  }
  # return(poly_list)
  newlims = get_letter_dims(l)
  for (pp in 1:length(poly_list)){
    p = poly_list[[pp]]
    p[,1] = normalise(p[,1], xlim, newlims[1:2])
    p[,2] = normalise(p[,2], ylim, newlims[3:4])
    poly_list[[pp]] = p
  }
  poly_list
}

normalise = function(v, old_lim, new_lim){
  (v - old_lim[1]) * diff(new_lim) / diff(old_lim) + new_lim[1]
}

plot_letter = function(l){
  poly_list = get_letter_outline(l)
  lims = get_letter_dims(l)
  graphics.off()
  x11()
  plot(NULL, xlim = lims[1:2], ylim = lims[3:4], asp = 1)
  for (pp in 1:length(poly_list)){
    points(poly_list[[pp]], pch = 16)
    polygon(poly_list[[pp]])
  }
}

# x1, y1, x2, y2
get_letter_dims = function(l){
  if (l %in% c("a", "e", "n", "o"))
    return(c(0, 3, 0, 3))
  if (l %in% c("b", "f", "h", "k", "B"))
    return(c(0, 3, 0, 6))
  if (l %in% c("G"))
    return(c(0, 4, 0, 6))
  if (l %in% c("g", "y"))
    return(c(0, 3, -3, 3))
  if (l %in% c("i"))
    return(c(0, 1, 0, 5))
  if (l %in% c("t"))
    return(c(0, 3, 0, 4))
  if (l %in% c("m"))
    return(c(0, 5, 0, 3))
  if (l %in% c("r"))
    return(c(0, 2, 0, 3))
  if (l == "\n")
    return(max(sapply(letters, get_letter_dims)[4,]) + 1)
  c(0, 0, 0, 0)
}

