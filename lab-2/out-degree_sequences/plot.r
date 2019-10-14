library(ggplot2)
require("VGAM") # for the Riemann-zeta function

H <- function(n, a) {
  sum((1:n)^(-a))
}

file = "./data/English_out-degree_sequence.txt"
degree_sequence = read.table(file, header = FALSE)
degree_spectrum = table(degree_sequence)
x <- degree_sequence$V1

N <- length(degree_spectrum)

from = 1
to = N
steps = 20

# Create a logarithmic sequence of n to plot
n_vals = 2^seq(log2(from), log2(to), length.out=steps)
n_vals

geometric = function(k) {
  q <- 0.08885679
  (1 - q)^(k - 1) * q
}

poisson = function(k) {
  lambda <- 1.545278
  lambda^k * exp(-lambda) / (factorial(k) * (1 - exp(-lambda)))
}

zeta2 = function(k) {
  k^(-2) / zeta(2)
}

zeta_ = function(k) {
  gamma <- 1.545278
  k^(-gamma) / zeta(gamma)
}

zeta_trunc = function(k) {
  gamma <- 1.53242
  k_max <- 17775.00000
  k^(-gamma) / H(k_max, gamma)
}

altmann = function(k) {
  gamma <- 1.25637575
  delta <- 0.01143132
  
  N_s <- seq(1:N)
  c_ <- 1 / sum(N_s^(-gamma) * exp(-delta * N_s))
  
  c_ * k^(-gamma) * exp(-delta * k)
}

y_geometric = geometric(n_vals) * length(x)
y_poisson = poisson(n_vals) * length(x)
y_zeta2 = zeta2(n_vals) * length(x)
y_zeta = zeta_(n_vals) * length(x)
y_zeta_trunc = zeta_trunc(n_vals) * length(x)
y_altmann = altmann(n_vals) * length(x)

#Plot the graph
plot(x = 1:N, type="l", y = degree_spectrum, main = "Out-degree distribution", xlab = "Out-degree", ylab = "Number of nodes")
lines(x = n_vals, type="l", y = y_altmann, pch=15, lty="49", col="red")
