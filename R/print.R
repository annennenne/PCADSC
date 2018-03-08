#'@import pander pander
#'@export
print.CEInfo <- function(x) {
  d <- x$d
  outtab <- matrix(round(c(x$xVals, x$y.obs), 2), 2, d + 1, byrow = TRUE,
                   dimnames = c(list(c("Joint cumulative eigenvalues",
                                       "Cumulative difference in eigenvalues"),
                                     paste("PCs: ", 0:d, sep = ""))))
  cat(pander(outtab, justify = paste(c("l", rep("r", d + 1)), collapse = ""),
      keep.trailing.zeros = TRUE))
}


print.angleInfo <- function(x) {


}
