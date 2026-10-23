# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
get_Minnesota_prior <- function(p, overall_shrinkage, Y, shrink_id) {
  Y <- as.matrix(Y)
  TT <- nrow(Y); N <- ncol(Y)
  K <- N * p + 1
  if (shrink_id) {
    priormean <- rbind(matrix(0, 1, N), diag(N), matrix(0, N * (p - 1), N))
  } else {
    priormean <- matrix(0, K, N)
  }
  priorvar <- numeric(K)
  sigma_sq <- ar_residual_variances(Y, p)
  s <- 1 / sigma_sq
  for (ll in 1:p) priorvar[(2 + N * (ll - 1)):(1 + N * ll)] <- (overall_shrinkage^2) * s / (ll^2)
  priorvar[1] <- 10^2
  list(priormean = priormean, priorvar = diag(priorvar))
}

ar_residual_variances <- function(Y, p) {
  TT <- nrow(Y); N <- ncol(Y)
  sigma_sq <- numeric(N)
  for (i in 1:N) {
    Ylag_i <- mlag2(Y[, i, drop = FALSE], p)[(p + 1):TT, , drop = FALSE]
    Y_i <- Y[(p + 1):TT, i]
    alpha_i <- solve(t(Ylag_i) %*% Ylag_i) %*% (t(Ylag_i) %*% Y_i)
    e <- Y_i - Ylag_i %*% alpha_i
    sigma_sq[i] <- (1 / (TT - p + 1)) * sum(e^2)
  }
  sigma_sq
}
