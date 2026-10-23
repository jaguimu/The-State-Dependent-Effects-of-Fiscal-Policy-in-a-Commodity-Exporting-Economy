# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# post = 1: log posterior at the posterior mode; post = 0: log likelihood
eval_objfcn_full <- function(thresh, VarX, VarY, state, post, model, ssr) {
  thresh <- as.vector(thresh)
  R <- length(thresh) + 1
  k <- ncol(VarY)
  N <- nrow(VarY)
  p <- model$VarLag
  w <- regime_weights(state, thresh)
  priorprec <- solve(model$VarPriorVar)
  priormean <- model$VarPriorMean
  priorB <- model$VarVolPriorB
  priorA <- model$VarVolPriorA
  g1 <- t(priormean) %*% priorprec %*% priormean
  lik <- matrix(0, R, N)
  prior <- numeric(R)
  for (rr in 1:R) {
    W <- diag(w[, rr])
    Ni <- sum(w[, rr])
    if (post == 1) {
      postprec <- priorprec + t(VarX) %*% W %*% VarX
      postpmean <- solve(postprec) %*% (t(VarX) %*% W %*% VarY + priorprec %*% priormean)
      postA <- priorA + Ni
      g2 <- t(VarY) %*% W %*% VarY
      g3 <- t(postpmean) %*% postprec %*% postpmean
      postB <- priorB + g1 + g2 - g3
      postB <- 0.5 * postB + 0.5 * t(postB)
      postpmode <- as.vector(t(postpmean))
      post_sigma_mode <- postB / (postA + k + 1)
      priorvar <- kronecker(post_sigma_mode, solve(priorprec))
      prior_b <- tryCatch(lmvnpdf(postpmode, as.vector(priormean), priorvar), error = function(e) NaN)
      if (!is.na(prior_b) && prior_b == -Inf) prior_b <- -1000
      prior_s <- 0.5 * priorA * log(det(priorB)) - 0.5 * priorA * k * log(2)
      prior_s <- prior_s - 0.5 * (priorA + k + 1) * log(det(post_sigma_mode))
      prior_s <- prior_s - 0.5 * sum(diag(priorB %*% solve(post_sigma_mode)))
      prior[rr] <- (Ni / N) * (prior_b + prior_s)
      if (ssr == 1) {
        prior <- 0
        post_sigma_mode <- diag(k)
      }
    } else {
      prior <- 0
      postpmean <- solve(t(VarX) %*% W %*% VarX) %*% t(VarX) %*% W %*% VarY
      if (ssr == 1) {
        post_sigma_mode <- diag(k)
      } else {
        ehat <- t(W %*% VarY - W %*% VarX %*% postpmean)
        if (all(is.nan(postpmean))) {
          postpmean <- matrix(0, k * p + 1, k)
          ehat <- matrix(0, k, N)
        }
        post_sigma_mode <- ehat %*% t(ehat) / Ni
      }
    }
    YY <- W %*% VarY
    YYmean <- W %*% VarX %*% postpmean
    for (nn in which(w[, rr] == 1)) {
      lik[rr, nn] <- tryCatch(lmvnpdf(YY[nn, ], YYmean[nn, ], post_sigma_mode), error = function(e) NaN)
    }
  }
  sum(lik) + sum(prior)
}
