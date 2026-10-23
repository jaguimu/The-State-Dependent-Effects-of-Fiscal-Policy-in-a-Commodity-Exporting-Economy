# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# columns: lag, AIC, BIC, HQ
lag_selection <- function(Y, maxLag) {
  Y <- as.matrix(Y); N <- ncol(Y)
  results <- matrix(NaN, maxLag, 4, dimnames = list(NULL, c("Lag", "AIC", "BIC", "HQ")))
  for (p in 1:maxLag) {
    dv <- getdatavar(Y, p)
    B_p <- solve(t(dv$X) %*% dv$X, t(dv$X) %*% dv$y)
    resid_p <- dv$y - dv$X %*% B_p
    Sigma_p <- t(resid_p) %*% resid_p / dv$Tact
    k <- N * (N * p + 1)
    T_p <- dv$Tact
    logL <- -0.5 * T_p * (N * log(2 * pi) + log(det(Sigma_p)) + N)
    results[p, ] <- c(p, -2 * logL / T_p + 2 * k / T_p, -2 * logL / T_p + k * log(T_p) / T_p,
                      -2 * logL / T_p + 2 * k * log(log(T_p)) / T_p)
  }
  results
}
