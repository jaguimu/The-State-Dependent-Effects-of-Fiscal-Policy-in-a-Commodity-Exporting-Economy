# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# Fixed-coefficient Bayesian VAR, Gibbs sampler
linear_irfs <- function(model, shock, ir_hor, Nodraws) {
  p <- model$VarLag; k <- model$N
  estimSigma <- model$sigma0
  irs <- array(0, c(k, ir_hor, Nodraws))
  for (it in 1:Nodraws) {
    BB <- draw_fbeta_given_s(model$VarPriorVar, model$VarPriorMean, model$VarX, t(model$VarY), p, 1,
                             model$VarCheckStability, model$StabilityThreshold, estimSigma)
    Resid <- t(transform_fbeta(model$VarX, model$VarY, BB))
    bayesalpha <- model$VarVolPriorA + model$VarT
    bayesgamma <- model$VarVolPriorB + Resid %*% t(Resid)
    estimSigma <- iwishrnd(bayesgamma, bayesalpha)
    irs[, , it] <- irf_recursive(BB, estimSigma, k, p, shock, ir_hor)
  }
  irf_bands(irs)
}
