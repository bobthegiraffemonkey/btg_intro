
plot_word = function(w, indents){
  foo = get_letter_vertices_and_edgelist(w, indents)
  p = foo$p
  E = foo$E
  graphics.off()
  x11()
  plot(NULL, xlim=range(p[,1]), ylim=range(p[,2]), asp=1)
  for (i in 1:nrow(E)){
    lines(p[E[i,],])
  }
}
