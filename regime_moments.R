# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
regime_moments <- function(samples, model, horizon) {
  d <- dim(samples$Bmat); Nodraws <- d[1]; R <- d[4]; N <- model$N
  pred <- array(0, c(N, R, horizon, Nodraws))
  uncon_var <- array(0, c(N, N, R, Nodraws))
  lmean <- array(0, c(N, R, Nodraws))
  sd <- array(0, c(N, R, Nodraws))
  for (nsim in 1:Nodraws) {
    B <- array(samples$Bmat[nsim, , , ], c(d[2], N, R))
    S <- array(samples$Sigma[nsim, , , ], c(N, N, R))
    m <- predictability_katerina_fast(B, S, N, model$VarLag, horizon)
    pred[, , , nsim] <- m$pred; uncon_var[, , , nsim] <- m$uncon_var; lmean[, , nsim] <- m$lmean
    for (j in 1:R) sd[, j, nsim] <- sqrt(diag(m$uncon_var[, , j]))
  }
  list(pred = pred, uncon_var = uncon_var, lmean = lmean, sd = sd)
}
