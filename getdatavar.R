# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
getdatavar <- function(insamp, p) {
  insamp <- as.matrix(insamp)
  TT <- nrow(insamp); N <- ncol(insamp)
  X <- matrix(0, TT - p, N * p)
  for (l in 1:p) {
    z <- lag0(insamp, l)
    X[, (1 + N * (l - 1)):(l * N)] <- z[(1 + p):TT, ]
  }
  y <- insamp[(1 + p):TT, , drop = FALSE]
  Tact <- TT - p
  list(X = cbind(1, X), y = y, Tact = Tact)
}
