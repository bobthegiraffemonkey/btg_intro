
plot_word = function(w, tabs){
  w_split = str_split(w, "", simplify = T)
  foo = get_letter_points_and_edgelist(w_split, tabs)
  p = foo$p
  E = foo$E
  graphics.off()
  x11()
  plot(NULL, xlim=range(p[,1]), ylim=range(p[,2]), asp=1)
  for (i in 1:nrow(E)){
    lines(p[E[i,],])
  }
}
