# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# Cogley, Primiceri and Sargent (2010) persistence, unconditional variance and mean by regime
predictability_katerina_fast <- function(BETA, SIGMA, n, l, horz) {
  Noregimes <- dim(BETA)[3]
  pred <- array(0, c(n, Noregimes, horz))
  uncon_var <- array(0, c(n, n, Noregimes))
  lmean <- matrix(0, n, Noregimes)
  for (j in 1:Noregimes) {
    OMEGA <- matrix(0, n * l, n * l)
    OMEGA[1:n, 1:n] <- SIGMA[, , j]
    cp <- companion(t(BETA[, , j]), n, l, 1)
    Ficom <- cp$Ficom
    vecuvar <- solve(diag((n * l)^2) - kronecker(Ficom, Ficom), as.vector(OMEGA))
    uvar <- matrix(vecuvar, n * l, n * l)
    uncon_var[, , j] <- uvar[1:n, 1:n]
    lmeanALL <- solve(diag(n * l) - Ficom) %*% c(cp$mu, rep(0, n * (l - 1)))
    lmean[, j] <- lmeanALL[1:n]
    FF <- diag(n * l)
    for (hh in 1:horz) {
      FF <- FF %*% Ficom
      R2 <- diag(FF %*% uvar %*% t(FF)) / diag(uvar)
      pred[, j, hh] <- R2[1:n]
    }
  }
  list(pred = pred, uncon_var = uncon_var, lmean = lmean)
}
