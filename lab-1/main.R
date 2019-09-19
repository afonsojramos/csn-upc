generateStrogatz <- function(x) {
  ws_graph <- watts.strogatz.game(1, 100, 4, 0.05)
  average.path.length(ws_graph)/diameter(ws_graph)
  transitivity(ws_graph)
  hist(degree(ws_graph))
}

er <- generateStrogatz(2)


l <- replicate(20, runif(sample(1:10, 1)), simplify = FALSE)
unlist(lapply(l, length))

