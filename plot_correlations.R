# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
plot_correlations <- function(file, corr, pairs, varnames, Regimes) {
  open_png(file, 1800, 1100)
  par(mfrow = c(2, 3), mar = c(2, 3, 2, 1), family = "serif", bg = "white")
  for (pp in 1:nrow(pairs)) {
    X <- t(corr[pp, , ])
    draw_boxes(X, Regimes, sprintf("%s - %s", varnames[pairs[pp, 1]], varnames[pairs[pp, 2]]))
  }
  invisible(dev.off())
}
