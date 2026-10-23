# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# pred: N x R x horizon x Nodraws
plot_persistence <- function(file, pred, order, names, hlist, Regimes) {
  R <- length(Regimes)
  open_png(file, 1800, 1500)
  par(mfrow = c(length(order), length(hlist)), mar = c(2, 3, 2, 1), family = "serif", bg = "white")
  for (r in seq_along(order)) for (h in hlist) {
    X <- t(pred[order[r], 1:R, h, ])
    draw_boxes(X, Regimes, sprintf("%s, h=%d", names[r], h))
  }
  invisible(dev.off())
}
