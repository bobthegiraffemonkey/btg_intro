
animate_and_save = function(w, tabs, filename){
  word_split = str_split(w, "", simplify = T)
  plot_word(word_split, tabs)
}
