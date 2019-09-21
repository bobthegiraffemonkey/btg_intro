
letter_vertices_and_edgelist = get_letter_vertices_and_edgelist(word, indents)
n_w = nrow(letter_vertices_and_edgelist$p)

x0 = min(letter_vertices_and_edgelist$p[,1])
x1 = max(letter_vertices_and_edgelist$p[,1])
y0 = min(letter_vertices_and_edgelist$p[,2])
y1 = max(letter_vertices_and_edgelist$p[,2])

p_random = cbind(runif(n_rand,
                       x0 - point_border_width,
                       x1 + point_border_width),
                 runif(n_rand,
                       y0 - point_border_width,
                       y1 + point_border_width))

p = rbind(letter_vertices_and_edgelist$p,
          p_random)
adj_rng = get_relative_neighbourhood_graph_as_adjlist(p)

# E = rbind(letter_vertices_and_edgelist$E,
#           get_filtered_edgelist(adj_rng, n_w))
# id = c(letter_vertices_and_edgelist$id,
#        rep(0, nrow(E)-length(letter_vertices_and_edgelist$id)))

E = get_filtered_edgelist(adj_rng, n_w)

edge_properties = c("")

