
# Pass in a word as a vector of characters, and the indents for spacing on
# each line. Finds the sizes of each letter, and returns a matrix of
# coordinates of each letter's offsets in the whole word.
get_letter_offsets = function(w, indents,
                              letter_space = 0.5,
                              word_space = 1){
  offsets = matrix(0, sum(grepl("[a-zA-Z ]", w)), 2)
  line = 1
  char_pos = 0
  char_dist = indents[line]
  for (l in w){
    if (grepl("[a-zA-Z]", l)){
      char_pos = char_pos + 1
      offsets[char_pos,] = c(char_dist, 0)
      char_dist = char_dist + get_letter_dims(l)[2] + letter_space
    } else if (l == "\n") {
      line = line + 1
      char_dist = indents[line]
      offsets[,2] = offsets[,2] + get_letter_dims(l)
    } else if (l == " "){
      char_dist = char_dist + word_space - letter_space
    } else {
      stop("You passed in something invalid")
    }
  }
  offsets
}

# Takes the words and indents and spits out the points, edges as edgelist,
# and a vector of IDs for each point/edge for which poly it is contained
# in. By the nature of the graph, |V|=|E|, so IDs apply to both equally!
# This edgelist is directed.
get_letter_vertices_and_edgelist = function(w, indents){
  letter_offsets = get_letter_offsets(w, indents)
  p = matrix(0, 0, 2)
  E = p
  num_edges = 0
  poly_ids = c()
  poly_id = 0
  char_pos = 0
  for (l in w){
    # Make sure we aren't on a newline char, and get the outline.
    if (grepl("[ \n]", l)) next
    poly_list = get_letter_outline(l)
    char_pos = char_pos + 1
    for (poly in poly_list){
      # Neat way to add the offset to each point of the poly.
      poly = sweep(poly, 2, letter_offsets[char_pos,], "+")
      
      p = rbind(p, poly)
      n = nrow(poly)
      n1 = 1:n
      E_add = num_edges + cbind(n1, (n1 %% n) + 1)
      E = rbind(E, E_add)
      num_edges = num_edges + n
      poly_id = poly_id + 1
      poly_ids = c(poly_ids, rep(poly_id, n))
    }
  }
  list(p=p, E=E, id=poly_ids)
}
