# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
load_data <- function(file) {
  D <- read.csv(file, stringsAsFactors = FALSE)
  D$date <- D$year + (D$quarter - 1) / 4
  D
}

sample_window <- function(D, y0, q0, y1, q1) {
  D[D$date >= y0 + (q0 - 1) / 4 & D$date <= y1 + (q1 - 1) / 4, ]
}
