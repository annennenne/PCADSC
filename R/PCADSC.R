#' @title Compute the elements used for PCADSC
#' @description Calculate standardized PCA loadings and accumulated variance contributions
#' seperately on two parts of a dataset in order to perform non-parametric data structure
#' comparisons.
#'
#' @param data A dataset, either a \code{data.frame} or a \code{matrix} with variables
#' in columns and observations in rows. WHAT TO DO ABOUT DATA.TABLES?
#'
#' @param splitBy A grouping variable with two levels defining the two groups within the
#' dataset whose data structures we wish to compare. If \code{splitBy} has more than
#' two levels, only the first two levels are used.
#'
#' @param var The variable names in \code{data} to include in the PCADSC. If \code{NULL}
#' (the default), all variables except for \code{splitBy} are used.
#'
#' @param covCO Not implemented yet (WHAT DID I HAVE IN MIND HERE? CUMMULATED VARIANCE
#' CONTRIBUTION CUT-OFF, MAYBE? SHOULDN'T BE IN THE OBJECT THOUGH, SHOULD RATHER
#' BE IN THE PLOTTING FUNCTION)
#'
#' @return A list including the following objects: ...
#'
#' @examples #DO something
#'
#' @export
PCADSC <- function(data, splitBy, var=NULL, covCO=NULL) {
  if (is.null(var)) var <- setdiff(names(data), splitBy)
  splitLevels <- unique(data[, splitBy])
  data1 <- data[data[, splitBy]==splitLevels[1], ]
  data2 <- data[data[, splitBy]==splitLevels[2], ]
  res1 <- pcaLoad(data1, var)
  res2 <- pcaLoad(data2, var)
  load1 <- res1$loadings
  load2 <- res2$loadings
  n1 <- nrow(load1)
  n2 <- nrow(load2)
  nObs1 <- res1$nObs
  nObs2 <- res2$nObs
  pcaFrame <- as.data.frame(rbind(load1, load2))
  pcaFrame$group <- c(rep(as.character(splitLevels[1]), n1),
                      rep(as.character(splitLevels[2]), n2))

  out <- list(pcaFrame=pcaFrame, splitBy=splitBy, splitLevels=splitLevels,
              varNames=var, n1=n1, n2=n2, nObs1=nObs1, nObs2=nObs2)
  class(out) <- "PCADSC"
  out
}
