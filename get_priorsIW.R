# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
get_priorsIW <- function(p, Y) {
  Y <- as.matrix(Y)
  TT <- nrow(Y); N <- ncol(Y)
  sigma_sq <- ar_residual_variances(Y, p)
  a <- max(N + 2, N + 2 * 8 - TT)
  list(a = a, sigma0 = diag((a - N - 1) * sigma_sq, N))
}
