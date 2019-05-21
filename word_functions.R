
get_letter_offsets = function(w, tabs){
  inter_space = 0.5
  space = 1
  offsets = matrix(0, sum(grepl("[a-zA-Z ]", w)), 2)
  j = 1
  k = 0
  x = tabs[j]
  for (l in w){
    if (grepl("[a-zA-Z]", l)){
      k = k + 1
      offsets[k,] = c(x, 0)
      x = x + get_letter_dims(l)[2] + inter_space
    } else if (l == "\n") {
      j = j + 1
      x = tabs[j]
      offsets[,2] = offsets[,2] + get_letter_dims(l)
    } else if (l == " "){
      x = x + space - inter_space
    } else {
      stop("You passed in something invalid")
    }
  }
  offsets
}

get_letter_points_and_edgelist = function(w, tabs){
  letter_offsets = get_letter_offsets(w, tabs)
  p = matrix(0, 0, 2)
  E = p
  ii = 0
  poly_ids = c()
  poly_id = 0
  ll = 0
  for (l in w){
    if (grepl("[ \n]", l)) next
    poly_list = get_letter_outline(l)
    ll = ll + 1
    for (poly in poly_list){
      n = nrow(poly)
      poly = sweep(poly, 2, letter_offsets[ll,], "+")
      p = rbind(p, poly)
      n1 = 1:n
      E_add = ii + cbind(n1, (n1 %% n) + 1)
      E = rbind(E, E_add)
      ii = ii + n
      poly_id = poly_id + 1
      poly_ids = c(poly_ids, rep(poly_id, n))
    }
  }
  list(p=p, E=E, id=poly_ids)
}




