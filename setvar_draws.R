# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# Normal-Wishart posterior draws by regime, conditional on the threshold
setvar_draws <- function(model, threshPO, Nodraws) {
  VarX <- model$VarX; VarY <- model$VarY; p <- model$VarLag; N <- model$N
  w <- regime_weights(model$state, threshPO)
  R <- ncol(w)
  priorprec <- solve(model$VarPriorVar)
  priormean <- model$VarPriorMean
  priorB <- model$VarVolPriorB
  priorA <- model$VarVolPriorA
  g1 <- t(priormean) %*% priorprec %*% priormean
  K <- N * p + 1
  out <- list(Bmat = array(0, c(Nodraws, K, N, R)), Sigma = array(0, c(Nodraws, N, N, R)),
              postpmean = array(0, c(K, N, R)), post_v = array(0, c(K, K, R)),
              postB = array(0, c(N, N, R)), postA = numeric(R), Ni = numeric(R))
  for (rr in 1:R) {
    W <- diag(w[, rr])
    Ni <- sum(w[, rr])
    postprec <- priorprec + t(VarX) %*% W %*% VarX
    post_v <- solve(postprec)
    postpmean <- post_v %*% (t(VarX) %*% W %*% VarY + priorprec %*% priormean)
    postA <- priorA + Ni
    g2 <- t(VarY) %*% W %*% VarY
    g3 <- t(postpmean) %*% postprec %*% postpmean
    postB <- priorB + g1 + g2 - g3
    postB <- 0.5 * postB + 0.5 * t(postB)
    cv <- t(chol(post_v))
    for (it in 1:Nodraws) {
      check <- 0
      while (check == 0) {
        Sigma <- iwishrnd(postB, postA)
        nu <- matrix(rnorm(K * N), K, N)
        Bdraw <- postpmean + cv %*% nu %*% chol(Sigma)
        Ficom <- companion(t(Bdraw), N, p, 1)$Ficom
        if (max(Mod(eigen(Ficom, only.values = TRUE)$values)) < model$StabilityThreshold) check <- 1
      }
      out$Bmat[it, , , rr] <- Bdraw
      out$Sigma[it, , , rr] <- Sigma
    }
    out$postpmean[, , rr] <- postpmean; out$post_v[, , rr] <- post_v
    out$postB[, , rr] <- postB; out$postA[rr] <- postA; out$Ni[rr] <- Ni
  }
  out
}
