
# Not actually animating yet!
animate_and_save = function(w, indents, filename, dev=TRUE){
  # Plan is to call into the plotting function and pass in dev
  # if true, plot to external window, else actually live up to
  # function name.
  
  word_split = str_split(w, "", simplify = TRUE)
  data = init(word_split)
  p=data$p
  vars = data$vars
  
  data$V[,] = TRUE
  
  for (i in 1:2){
    data = update_state(data$E, data$V)
  }
  
  draw_graph(p, data$E, vars)
  # plot_word(word_split, indents)
}
