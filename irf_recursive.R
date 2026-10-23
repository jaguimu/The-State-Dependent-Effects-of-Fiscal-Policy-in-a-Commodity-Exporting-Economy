# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# Cholesky identification, unit impact normalization
irf_recursive <- function(B, Sigma, N, p, shock, ir_hor) {
  Ficom <- companion(t(B), N, p, 1)$Ficom
  ch <- t(chol(Sigma))
  A <- ch %*% diag(1 / diag(ch), N)
  b1 <- A[, shock]
  irf <- matrix(0, N, ir_hor)
  irf[, 1] <- b1
  irs <- c(b1, rep(0, (p - 1) * N))
  if (ir_hor > 1) for (j in 1:(ir_hor - 1)) {
    irs <- Ficom %*% irs
    irf[, j + 1] <- irs[1:N]
  }
  irf
}
