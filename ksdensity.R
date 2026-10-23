# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# Normal kernel density with the default bandwidth of MATLAB ksdensity
ksdensity <- function(x, xi) {
  x <- as.vector(x); n <- length(x)
  sig <- median(abs(x - median(x))) / 0.6745
  if (sig <= 0) sig <- max(x) - min(x)
  h <- sig * (4 / (3 * n))^(1 / 5)
  sapply(xi, function(u) mean(dnorm((u - x) / h)) / h)
}
