require("stats4") # for MLE
require("VGAM") # for the Riemann-zeta function

H <- function(n, a) {
  sum((1:n)^(-a))
}

source = read.table("list-out.txt", 
                    header = TRUE,               # this is to indicate the first line of the file contains the names of the columns instead of the real data
                    as.is = c("language","file") # this is need to have the cells treated as real strings and not as categorial data.
)

get_AIC <- function(m2logL, K, N) {
  m2logL + 2*K*N/(N-K-1) # AIC with a correction for sample size
}

#attributes(summary(mle_zeta))$m2logL

estimate_likelihoods <- function(file) {
  degree_sequence = read.table(file, header = FALSE)
  
  x <- degree_sequence$V1
  
  N <- length(x)
  k_max <- max(x)
  M <- sum(x)
  M_ <- sum(log(x))
  
  C <- 0
  for (i in 1:N) {
    for (j in 2:x[i]) {
      C <- C + log(j)
    }
  }
  
  
  print(paste("N", N, sep=" "))
  print(paste("k_max", k_max, sep=" "))
  print(paste("M", M, sep=" "))
  print(paste("N/M", N/M))
  print(paste("M/N", M/N))
  print(paste("M_", M_, sep=" "))
  print(paste("c", C, sep=" "))
  
  minus_log_likelihood_poisson <- function(lambda) {
    N*(lambda+log(1-exp(-lambda))) + C - M * log(lambda)
  }
  
  minus_log_likelihood_geometric <- function(q) {
    (N - M) * log(1 - q) - N * log(q)
  }
  
  minus_log_likelihood_zeta2 <- function() {
    2 * M_ + N * log(pi^2 / 6)
  }
  
  minus_log_likelihood_zeta <- function(gamma) {
    N * log(zeta(gamma)) + gamma * M_
  }
  
  minus_log_likelihood_zeta_trunc <- function(gamma, k_max) {
    gamma * M_ + N * log(H(k_max, gamma))
  }
  
  minus_log_likelihood_altmann <- function(gamma, delta, k_s) {
    N_s <- seq(1:N)
    c <- 1 / sum(N_s^(-gamma) * exp(-delta * N_s))
    sum(gamma * log(k_s) + delta * k_s - log(c))
  }
  
  mle_poisson <- mle(minus_log_likelihood_poisson,
                  start = list(lambda = M/N),
                  method = "L-BFGS-B",
                  lower = c(1.0000001)
  )
  
  mle_geometric <- mle(minus_log_likelihood_geometric,
                       start = list(q = N/M),
                       method = "L-BFGS-B",
                       lower = c(0.01),
                       upper = c(0.99)
  )
  
  mle_zeta2_number <- minus_log_likelihood_zeta2()
  
  mle_zeta <- mle(minus_log_likelihood_zeta,
                  start = list(gamma = 2),
                  method = "L-BFGS-B",
                  lower = c(1.0000001)
  )
  
  mle_zeta_trunc <- mle(minus_log_likelihood_zeta_trunc,
                  start = list(gamma = 2, k_max = k_max),
                  method = "L-BFGS-B",
                  lower = c(1.0000001, N)
  )
  
  mle_altmann <- mle(minus_log_likelihood_altmann,
                  start = list(gamma = 1, delta = 1),
                  fixed = list(k_s = x),
                  method = "L-BFGS-B",
                  lower = c(0.00001, 0.00001)
  )
  
  print("-----")
  print("Params:")
  print(attributes(summary(mle_poisson))$coef)
  print(attributes(summary(mle_geometric))$coef)
  print(NULL)
  print(attributes(summary(mle_zeta))$coef)
  print(attributes(summary(mle_zeta_trunc))$coef)
  print(attributes(summary(mle_altmann))$coef)
  
  aic_poisson = get_AIC(attributes(summary(mle_poisson))$m2logL, 1, N)
  aic_geometric = get_AIC(attributes(summary(mle_geometric))$m2logL, 1, N)
  aic_zeta2 = get_AIC(2 * mle_zeta2_number, 0, N)
  aic_zeta = get_AIC(attributes(summary(mle_zeta))$m2logL, 1, N)
  aic_zeta_trunc = get_AIC(attributes(summary(mle_zeta_trunc))$m2logL, 2, N)
  aic_altmann = get_AIC(attributes(summary(mle_altmann))$m2logL, 2, N)
  
  print("----")
  print("AIC without Altmann:")
  aics <- c(aic_poisson, aic_geometric, aic_zeta2, aic_zeta, aic_zeta_trunc)
  print(aics)
  aic_best = min(aics)
  print(aic_best)
  aic_diffs <- abs(aics - aic_best)
  print(aic_diffs)
  
  print("----")
  print("AIC with Altmann:")
  aics <- c(aic_poisson, aic_geometric, aic_zeta2, aic_zeta, aic_zeta_trunc, aic_altmann)
  print(aics)
  aic_best = min(aics)
  print(aic_best)
  aic_diffs <- abs(aics - aic_best)
  print(aic_diffs)
}

#estimate_likelihoods("./samples_from_discrete_distributions/data/sample_of_geometric_with_parameter_0.05.txt")
#estimate_likelihoods("./data/English_out-degree_sequence.txt")

for (x in 1:nrow(source)) {
  print("-----------------------")
  print(source$language[x])
  estimate_likelihoods(source$file[x])
}

