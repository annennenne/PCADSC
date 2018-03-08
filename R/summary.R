#'@import pander pander
#'@export
summary.CEInfo <- function(x) {
  if (is.null(x)) {
    cat("No cumulative eigenvalue (CE) information available")
  } else {
  y.min <- apply(x$y.sim, 1, quantile, probs=0.025)
  y.max <- apply(x$y.sim, 1, quantile, probs=0.975)
  outsideCI <- any(x$y.obs < x$y.min | x$y.obs > x$y.max)

  restab <- matrix(c("Number of variables:",
                     "Number of permutation iterations:",
                     "Pointwise 95% CI exceeded:",
                     "Kolmogorov-Smirnov test p-value:",
                     "Cramer-von Mises test p-value:",
                     x$d, x$B, outsideCI, x$KS.pvalue,
                     x$CvM.pvalue), 5, 2)

  cat("Cumulative eigenvalue (CE) information")
  cat(pander(restab, justify = "lr"))
  }
}
