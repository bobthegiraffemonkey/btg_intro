
# Pass in words as a vector of characters, and the indents for spacing on
# each line. Finds the sizes of each letter, and returns a matrix of
# coordinates of each letter's offsets in the whole text.
get_letter_offsets = function(w, indents){
  letter_space = 0.5
  extra_word_space = 1
  line_space = 7
  
  offsets = matrix(0, sum(grepl("[a-zA-Z ]", w)), 2)
  num_lines = sum(grepl("\n", w), fixed=TRUE)
  line_num = 1
  char_num = 0
  
  # Set the x coordinate to the indent of the first line,
  # and the y coordinate to the top line.
  x = indents[line_num]
  y = line_space * num_lines
  
  for (l in w){
    if (grepl("[a-zA-Z]", l)){
      char_num = char_num + 1
      offsets[char_num,] = c(x, y)
      x = x + get_letter_dims(l)[2] + letter_space
    } else if (l == "\n") {
      line_num = line_num + 1
      x = indents[line_num]
      y = y - line_space
    } else if (l == " "){
      x = x + extra_word_space
    } else {
      stop("You passed in something invalid")
    }
  }
  offsets
}

# Takes the words and indents and spits out the points, edges as edgelist,
# and a vector of IDs for each point/edge for which poly it is contained
# in. By the nature of the graph, |V| == |E|, so IDs apply to both equally.
get_letter_vertices_and_edgelist = function(w, indents){
  letter_offsets = get_letter_offsets(w, indents)
  E = p = matrix(0, 0, 2)
  num_edges = 0
  poly_ids = c()
  poly_id = 0
  char_num = 0
  
  for (l in w){
    # Make sure we are on a letter.
    if (grepl("[ \n]", l)) next
    char_num = char_num + 1
    
    # Iterate through the polygons that comprise the letter.
    for (poly in get_letter_polygons(l)){
      # Neat way to add the offset to each point of the poly.
      poly = sweep(poly, 2, letter_offsets[char_num,], "+")
      
      p = rbind(p, poly)
      n = nrow(poly)
      E_add = num_edges + cbind(c(1:(n-1), 1),
                                c(2:n, n))
      E = rbind(E, E_add)
      num_edges = num_edges + n
      poly_id = poly_id + 1
      poly_ids = c(poly_ids, rep(poly_id, n))
    }
  }
  list(p=p, E=E, id=poly_ids)
}
