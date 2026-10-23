# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# draws: N x 2 x Nodraws
plot_densities <- function(file, draws, gridd, names) {
  c1 <- rgb(0, 0, 0.85); c2 <- rgb(0.75, 0, 0)
  open_png(file, 1400, 1050)
  par(mfrow = c(2, 2), mar = c(3, 3, 2, 1), family = "serif", bg = "white")
  for (jj in 1:dim(draws)[1]) {
    s1 <- ksdensity(sort(draws[jj, 1, ]), gridd[jj, ])
    s2 <- ksdensity(sort(draws[jj, 2, ]), gridd[jj, ])
    plot(gridd[jj, ], s1, type = "n", ylim = range(c(s1, s2)), xaxs = "i", yaxs = "i", xlab = "", ylab = "", main = names[jj])
    grid(col = "grey85", lty = 1)
    lines(gridd[jj, ], s1, col = c1, lwd = 1.5)
    lines(gridd[jj, ], s2, col = c2, lwd = 1.5)
    box(lwd = 1)
    legend("topright", legend = c("Low Regime", "High Regime"), col = c(c1, c2), lwd = 1.5, bg = "white", cex = 0.8)
  }
  invisible(dev.off())
}
