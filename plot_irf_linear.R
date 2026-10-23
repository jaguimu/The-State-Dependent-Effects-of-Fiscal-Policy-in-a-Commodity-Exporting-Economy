# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
plot_irf_linear <- function(file, irf, vars, titles) {
  H <- ncol(irf$med); hor <- 0:(H - 1)
  open_png(file, 1600, 760)
  par(mfrow = c(1, length(vars)), mar = c(4, 4.5, 2, 1), family = "serif", bg = "white")
  for (vv in seq_along(vars)) {
    idx <- vars[vv]
    yl <- range(c(irf$lo[idx, ], irf$hi[idx, ], 0))
    plot(hor, irf$med[idx, ], type = "n", xlim = c(0, H - 1), ylim = yl, xaxs = "i",
         xlab = "Quarters", ylab = if (vv == 1) "Response" else "", main = titles[vv])
    grid(col = "grey85", lty = 1)
    polygon(c(hor, rev(hor)), c(irf$hi[idx, ], rev(irf$lo[idx, ])), col = alpha_col(c(0.20, 0.45, 0.85), 0.25), border = NA)
    lines(hor, irf$med[idx, ], col = rgb(0, 0.20, 0.55), lwd = 2.2)
    abline(h = 0, lty = 2, lwd = 0.75)
    box(lwd = 1)
  }
  invisible(dev.off())
}
