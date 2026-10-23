# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
plot_regime_series <- function(file, dates, series, names, dates_state, regimes, colors, alpha, nrow, ncol) {
  open_png(file, 1500, 950)
  par(mfrow = c(nrow, ncol), mar = c(4, 4, 2, 1), bg = "white")
  for (vv in seq_along(series)) {
    y <- series[[vv]]
    pad <- 0.05 * (max(y) - min(y) + .Machine$double.eps)
    ylo <- min(y) - pad; yhi <- max(y) + pad
    plot(dates, y, type = "n", xlim = range(dates), ylim = c(ylo, yhi), xaxs = "i", yaxs = "i",
         xlab = "Year", ylab = "Value", main = names[vv], cex.main = 1.1, cex.lab = 0.9, cex.axis = 0.9)
    grid(col = "grey85", lty = 1)
    for (g in seq_along(regimes)) {
      reg <- regimes[[g]]
      in_block <- FALSE
      for (tt in seq_along(reg)) {
        if (reg[tt] && !in_block) { in_block <- TRUE; block_start <- dates_state[tt] }
        if (in_block && (tt == length(reg) || !reg[tt + 1])) {
          in_block <- FALSE
          block_end <- dates_state[tt]
          polygon(c(block_start, block_end, block_end, block_start), c(ylo, ylo, yhi, yhi),
                  col = alpha_col(colors[[g]], alpha), border = NA)
        }
      }
    }
    lines(dates, y, col = rgb(0, 0.25, 0.55), lwd = 1.6)
    abline(h = 0, lty = 2, lwd = 0.6)
    box(lwd = 1)
  }
  invisible(dev.off())
}
