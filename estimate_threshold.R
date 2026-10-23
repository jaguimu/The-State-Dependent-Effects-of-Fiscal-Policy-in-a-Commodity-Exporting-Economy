# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
estimate_threshold <- function(model, K, Ngridpoints) {
  gam_values <- seq(min(model$state), max(model$state), length.out = Ngridpoints)
  if (K == 2) {
    posterior <- numeric(Ngridpoints)
    for (i in 1:Ngridpoints) posterior[i] <- eval_objfcn_full(gam_values[i], model$VarX, model$VarY, model$state, 1, model, 0)
    threshPO <- gam_values[which.max(posterior)]
  } else if (K == 3) {
    posterior <- matrix(NaN, Ngridpoints, Ngridpoints)
    for (i in 1:Ngridpoints) for (j in i:Ngridpoints) {
      posterior[i, j] <- eval_objfcn_full(c(gam_values[i], gam_values[j]), model$VarX, model$VarY, model$state, 1, model, 0)
    }
    ix <- which.max(posterior)
    threshPO <- c(gam_values[(ix - 1) %% Ngridpoints + 1], gam_values[(ix - 1) %/% Ngridpoints + 1])
  }
  list(thr = threshPO, posterior = posterior, gam = gam_values)
}
