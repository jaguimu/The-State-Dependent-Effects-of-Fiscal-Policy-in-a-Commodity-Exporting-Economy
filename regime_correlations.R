# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
regime_correlations <- function(samples, model) {
  d <- dim(samples$Bmat); Nodraws <- d[1]; R <- d[4]
  N <- model$N; p <- model$VarLag
  pairs <- t(combn(N, 2))
  corr_draws <- array(0, c(nrow(pairs), R, Nodraws))
  for (nsim in 1:Nodraws) for (j in 1:R) {
    FF <- companion(t(samples$Bmat[nsim, , , j]), N, p, 1)$Ficom
    OMEGA <- matrix(0, N * p, N * p)
    OMEGA[1:N, 1:N] <- samples$Sigma[nsim, , , j]
    uvar <- doublej(FF, OMEGA)
    for (pp in 1:nrow(pairs)) {
      a <- pairs[pp, 1]; b <- pairs[pp, 2]
      corr_draws[pp, j, nsim] <- uvar[a, b] / (sqrt(uvar[a, a]) * sqrt(uvar[b, b]))
    }
  }
  list(corr = corr_draws, pairs = pairs)
}
