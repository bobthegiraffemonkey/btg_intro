
# Not actually animating yet!
animate_and_save = function(w, indents, filename, dev=TRUE){
  # Plan is to call into the plotting function and pass in dev
  # if true, plot to external window, else actually live up to
  # function name.
  
  word_split = str_split(w, "", simplify = T)
  plot_word(word_split, indents)
}
