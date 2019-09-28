library('igraph')
library('emdbook')

# Strogatz Model
generateStrogatz <- function(p) {
  ws_graph <- watts.strogatz.game(1, 1000, 4, p)
  return (c(l=average.path.length(ws_graph), c=transitivity(ws_graph)))
}

normalise <- function(l) {
  return (l/l[1])
}

ps <- lseq(0.0001,1,14) # creation of logarithmic sequence
ps2 <- rep(ps, 10) # repeat the values x times
values <- mapply(generateStrogatz, ps2)
len <- normalise(values['l',])
coef <- normalise(values['c',])
plot(ps,coef, ylim = c(0,1), main = 'Title', xlab='Probability', ylab='Coefficient', log='x')
points(ps,len, ylim = c(0,1),pch=15)
