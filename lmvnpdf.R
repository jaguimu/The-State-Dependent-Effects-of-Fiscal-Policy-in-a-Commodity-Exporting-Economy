# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
lmvnpdf <- function(x, mu, Sigma) {
  R <- chol(Sigma)
  z <- backsolve(R, x - mu, transpose = TRUE)
  log(exp(-0.5 * sum(z^2) - sum(log(diag(R))) - length(x) * log(2 * pi) / 2))
}
