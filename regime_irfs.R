# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
regime_irfs <- function(samples, p, shock, ir_hor, burn) {
  d <- dim(samples$Bmat); Nodraws <- d[1]; N <- d[3]; R <- d[4]
  irs <- array(0, c(N, ir_hor, Nodraws, R))
  for (rr in 1:R) for (it in 1:Nodraws) {
    irs[, , it, rr] <- irf_recursive(samples$Bmat[it, , , rr], samples$Sigma[it, , , rr], N, p, shock, ir_hor)
  }
  irf_bands(irs[, , (burn + 1):Nodraws, , drop = FALSE])
}
