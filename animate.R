
# Not actually animating yet!
animate_and_save = function(w, indents, filename, dev=TRUE){
  # Plan is to call into the plotting function and pass in dev
  # if true, plot to external window, else actually live up to
  # function name.
  
  word_split = str_split(w, "", simplify = TRUE)
  stuff = init(word_split)
  
  for (i in 1:343){
    stuff$data = update_state(stuff$data$E, stuff$data$V)
  }
  
  draw_graph(stuff$p, stuff$data$E, stuff$vars)
  # plot_word(word_split, indents)
}
