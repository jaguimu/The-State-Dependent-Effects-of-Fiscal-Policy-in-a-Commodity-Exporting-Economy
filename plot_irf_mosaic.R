# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
plot_irf_mosaic <- function(file, irf, vars, titles, regnames) {
  H <- dim(irf$med)[2]; R <- dim(irf$med)[3]; hor <- 0:(H - 1)
  col <- c(0.13, 0.29, 0.53)
  open_png(file, 1275, 1425)
  par(mfrow = c(R, 2), mar = c(4, 6, 2, 1), family = "serif", bg = "white")
  for (rr in 1:R) for (kk in seq_along(vars)) {
    vv <- vars[kk]
    yl <- range(c(irf$lo[vv, , rr], irf$hi[vv, , rr], 0))
    plot(hor, irf$med[vv, , rr], type = "n", xlim = c(0, H - 1), ylim = yl, xaxs = "i",
         xlab = if (rr == R) "Quarters" else "", ylab = "Response", main = if (rr == 1) titles[kk] else "")
    if (kk == 1) mtext(regnames[rr], side = 2, line = 4, font = 2, cex = 0.8)
    grid(col = "grey85", lty = 1)
    polygon(c(hor, rev(hor)), c(irf$hi[vv, , rr], rev(irf$lo[vv, , rr])), col = alpha_col(col, 0.2), border = NA)
    lines(hor, irf$med[vv, , rr], col = rgb(col[1], col[2], col[3]), lwd = 1.8)
    abline(h = 0, lty = 2, col = "grey50")
    box(lwd = 1)
  }
  invisible(dev.off())
}
