
# Not actually animating yet!
animate_and_save = function(w, indents, filename, dev=TRUE){
  # Plan is to call into the plotting function and pass in dev
  # if true, plot to external window, else actually live up to
  # function name.
  
  word_split = str_split(w, "", simplify = TRUE)
  
  data = init(btg)
  data$undir_edges[,"col"]
  range(data$dir_edges[,"progress"])
  
  data = init(word_split)
  draw_graph(data$p, data$undir_edges, data$dir_edges, data$vars)
  # plot_word(word_split, indents)
}
