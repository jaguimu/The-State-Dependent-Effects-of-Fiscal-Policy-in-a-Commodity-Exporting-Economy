# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
draw_fbeta_given_s <- function(priorvar, priormean, XX, y, p, Intercept, CheckStability, bound, SIGMA) {
  N <- nrow(y)
  s <- kronecker(diag(N), crossprod(XX))
  s2 <- as.vector(crossprod(XX, t(y)))
  KK <- ncol(XX) * N
  priorprec0 <- solve(priorvar)
  bayesprec <- kronecker(solve(SIGMA), priorprec0) + s
  bayesv <- solve(bayesprec)
  BB <- bayesv %*% (s2 + kronecker(diag(N), priorprec0) %*% as.vector(priormean))
  check <- 0
  while (check == 0) {
    drawbeta <- BB + t(chol(bayesv)) %*% rnorm(KK)
    draw <- matrix(drawbeta, KK / N, N)
    if (CheckStability == 1) {
      if (max(Mod(eigen(companion(t(draw), N, p, Intercept)$Ficom, only.values = TRUE)$values)) < bound) check <- 1
    } else {
      check <- 1
    }
  }
  draw
}
