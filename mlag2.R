# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
mlag2 <- function(X, p) {
  X <- as.matrix(X)
  Traw <- nrow(X); N <- ncol(X)
  Xlag <- matrix(0, Traw, N * p)
  for (ii in 1:p) Xlag[(p + 1):Traw, (N * (ii - 1) + 1):(N * ii)] <- X[(p + 1 - ii):(Traw - ii), 1:N]
  Xlag
}
