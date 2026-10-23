# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
companion <- function(Fi1, n, l, Intercept) {
  Fi1 <- as.matrix(Fi1)
  Ficom <- matrix(0, n * l, n * l)
  if (Intercept == 1) {
    mu <- Fi1[, 1]
    Ficom[1:n, ] <- Fi1[, 2:(n * l + 1), drop = FALSE]
  } else {
    mu <- NULL
    Ficom[1:n, ] <- Fi1
  }
  if (l >= 2) for (zz in 2:l) {
    Ficom[(1 + n * (zz - 1)):(zz * n), (1 + n * (zz - 2)):(n * (zz - 1))] <- diag(n)
  }
  list(Ficom = Ficom, mu = mu)
}
