# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
alpha_col <- function(col, a) rgb(col[1], col[2], col[3], a)

open_png <- function(file, width, height) png(paste0(file, ".png"), width = width, height = height, res = 150)

panel_axes <- function() {
  box(lwd = 1)
  grid(col = "grey85", lty = 1)
}
