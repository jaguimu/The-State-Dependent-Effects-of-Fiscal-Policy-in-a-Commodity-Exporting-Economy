# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# Inverse Wishart draw with scale S and df degrees of freedom (as MATLAB iwishrnd)
iwishrnd <- function(S, df) {
  W <- stats::rWishart(1, df, solve(S))[, , 1]
  Sig <- solve(W)
  0.5 * (Sig + t(Sig))
}
