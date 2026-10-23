# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# Y: T x N endogenous variables, p: lags, s: T x 1 state variable (lagged one period inside)
setup_model <- function(Y, p, s) {
  Y <- as.matrix(Y)
  model <- list(Y = t(Y), N = ncol(Y), VarLag = p)
  dv <- getdatavar(Y, p)
  model$VarX <- dv$X; model$VarY <- dv$y; model$VarT <- dv$Tact
  model$T <- model$VarT
  model$VarOverall_shrinkage <- 1
  mp <- get_Minnesota_prior(p, model$VarOverall_shrinkage, model$VarY, 0)
  model$VarPriorMean <- mp$priormean; model$VarPriorVar <- mp$priorvar
  model$VarBB0 <- solve(t(model$VarX) %*% model$VarX) %*% (t(model$VarX) %*% model$VarY)
  resid <- model$VarY - model$VarX %*% model$VarBB0
  model$sigma0 <- t(resid) %*% resid / model$T
  model$Resid <- t(resid)
  iw <- get_priorsIW(p, model$VarY)
  model$VarVolPriorA <- iw$a; model$VarVolPriorB <- iw$sigma0
  d <- 1
  model$state <- s[(p + 1 - d):(length(s) - d)]
  model$StabilityThreshold <- 0.9999
  model$VarCheckStability <- 1
  model
}
