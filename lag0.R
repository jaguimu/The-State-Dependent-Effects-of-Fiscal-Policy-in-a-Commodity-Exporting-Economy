# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
lag0 <- function(x, p) {
  x <- as.matrix(x)
  R <- nrow(x); C <- ncol(x)
  rbind(matrix(0, p, C), x[1:(R - p), , drop = FALSE])
}
