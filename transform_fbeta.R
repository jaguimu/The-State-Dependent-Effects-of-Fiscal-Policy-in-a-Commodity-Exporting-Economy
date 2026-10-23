# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
transform_fbeta <- function(X, y, BB) y - X %*% BB
