# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# Box statistics as in MATLAB boxplot: prctile quartiles, whiskers at 1.5 IQR
box_stats <- function(x) {
  q <- quantile(x, c(0.25, 0.5, 0.75), type = 5, names = FALSE)
  iq <- q[3] - q[1]
  c(min(x[x >= q[1] - 1.5 * iq]), q[1], q[2], q[3], max(x[x <= q[3] + 1.5 * iq]))
}

draw_boxes <- function(X, labels, main, ylim = NULL) {
  st <- apply(X, 2, box_stats)
  z <- list(stats = st, n = rep(nrow(X), ncol(X)), names = labels)
  bxp(z, boxcol = rgb(0.55, 0, 0), medcol = rgb(0.85, 0.2, 0.1), whiskcol = rgb(0.55, 0, 0), staplecol = rgb(0.55, 0, 0),
      whisklty = 2, medlwd = 1.5, boxfill = "white", outline = FALSE, main = main, ylim = ylim, boxwex = 0.5)
  grid(nx = NA, ny = NULL, col = "grey85", lty = 1)
}
