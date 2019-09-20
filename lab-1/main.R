library("igraph")

# Strogatz Model
generateStrogatz <- function(p) {
  ws_graph <- watts.strogatz.game(1, 1000, 4, p)
  return (c(l=average.path.length(ws_graph), c=transitivity(ws_graph)))
}

normalise <- function(l) {
  return (l/l[1])
}

ps <- 10^(seq(-4,0,0.2))
values <- mapply(generateStrogatz, ps)
len <- normalise(values["l",])
coef <- normalise(values["c",])
plot(ps,coef, ylim = c(0,1), ylab='coeff', log='x')
points(ps,len, ylim = c(0,1), ylab='coeff',pch=15)
