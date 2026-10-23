# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# median and 68% bands over draws (dimension 3)
irf_bands <- function(irs) {
  d <- dim(irs)
  if (length(d) == 3) irs <- array(irs, c(d, 1))
  d <- dim(irs)
  Neff <- d[3]
  idx_low <- max(1, round(0.16 * Neff))
  idx_high <- min(Neff, round(0.84 * Neff))
  med <- lo <- hi <- array(0, c(d[1], d[2], d[4]))
  for (r in 1:d[4]) for (i in 1:d[1]) for (j in 1:d[2]) {
    v <- sort(irs[i, j, , r])
    med[i, j, r] <- median(v); lo[i, j, r] <- v[idx_low]; hi[i, j, r] <- v[idx_high]
  }
  if (d[4] == 1) list(med = med[, , 1], lo = lo[, , 1], hi = hi[, , 1]) else list(med = med, lo = lo, hi = hi)
}
