# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
plot_irf_regimes <- function(file, irf, vars, titles, regnames) {
  H <- dim(irf$med)[2]; hor <- 0:(H - 1)
  cols <- list(c(0, 0.20, 0.55), c(0.75, 0.05, 0.05))
  open_png(file, 2100, 630)
  par(mfrow = c(1, length(vars)), mar = c(3, 4.5, 2, 1), bg = "white")
  for (vv in seq_along(vars)) {
    nn <- vars[vv]
    yl <- range(c(irf$lo[nn, , ], irf$hi[nn, , ], 0))
    plot(hor, irf$med[nn, , 1], type = "n", xlim = c(0, H - 1), ylim = yl, xaxs = "i",
         xlab = "", ylab = "Response", main = titles[vv])
    grid(col = "grey85", lty = 1)
    for (rr in 1:2) polygon(c(hor, rev(hor)), c(irf$lo[nn, , rr], rev(irf$hi[nn, , rr])), col = alpha_col(cols[[rr]], 0.2), border = NA)
    for (rr in 1:2) lines(hor, irf$med[nn, , rr], col = rgb(cols[[rr]][1], cols[[rr]][2], cols[[rr]][3]), lwd = 2.2)
    abline(h = 0, lty = 2, lwd = 0.75)
    box(lwd = 1)
    if (vv == 1) legend("topright", legend = regnames, col = sapply(cols, function(c) rgb(c[1], c[2], c[3])), lwd = 2.2, bty = "n", cex = 0.9)
  }
  invisible(dev.off())
}
