# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
regime_weights <- function(state, threshPO) {
  R <- length(threshPO) + 1
  w <- matrix(0, length(state), R)
  w[, 1] <- as.numeric(state <= threshPO[1])
  w[, R] <- as.numeric(state > threshPO[R - 1])
  if (R > 2) for (rr in 2:(R - 1)) w[, rr] <- as.numeric(state > threshPO[rr - 1] & state <= threshPO[rr])
  w
}
