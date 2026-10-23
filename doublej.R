# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
doublej <- function(a1, b1) {
  alpha0 <- a1
  gamma0 <- b1
  diff <- 5; ijk <- 1
  while (diff > 1e-15) {
    alpha1 <- alpha0 %*% alpha0
    gamma1 <- gamma0 + alpha0 %*% gamma0 %*% t(alpha0)
    diff <- max(abs(gamma1 - gamma0))
    gamma0 <- gamma1
    alpha0 <- alpha1
    ijk <- ijk + 1
    if (ijk > 50) message("doublej: no convergence")
  }
  gamma1
}
