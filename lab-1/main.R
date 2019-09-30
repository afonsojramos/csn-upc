library('igraph')
library('emdbook')
library('tidyr')

# Strogatz Model
generateStrogatz <- function(n, p) {
  ws_graph <- watts.strogatz.game(1, n, 4, p)
  return (c(average.path.length(ws_graph), transitivity(ws_graph)))
}

normalise <- function(l) {
  return (l/l[1])
}

ps <- lseq(0.0001, 1, 14) # creation of logarithmic sequence
ns <- seq(550, 1450, 100)
args <- crossing(ps, ns)

values <- mapply(generateStrogatz, args$ns, args$ps)
lens <- normalise(apply(matrix(values[1,], nrow=length(ns)), 2, mean))
coefs <- normalise(apply(matrix(values[2,], nrow=length(ns)), 2, mean))

plot(ps, coefs, ylim = c(0, 1), xlab='p', ylab='', log='x', pch=0)
points(ps, lens, ylim = c(0, 1), pch=16)
text(x=0.001, y=0.25, labels=c('L(p) / L(0)'))
text(x=0.01, y=0.8, labels=c('C(p) / C(0)'))
