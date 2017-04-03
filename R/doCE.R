#' @title Compute cumulative eigenvalue information
#'
#' @description Computes the information that is needed in order to make a \code{\link{CEPlot}}
#' from a \code{PCADSC} or \code{pcaRes} object. Typically, this function is called on a partial
#' \code{PCADSC} object in order to add \code{CEInfo} (see examples).
#'
#' @param x Either a \code{PCADSC} or a \code{pcaRes} object.
#'
#' @param ... If \code{doCE} is called on a \code{pcaRes} object, the full dataset must also
#' be supplied (as \code{data}), as well as the number of resampling steps (\code{B}).
#'
#' @examples
#' #load iris data
#' data(iris)
#'
#' #Define grouping variable, grouping the observations by whether their species is
#' #Setosa or not
#' iris$group <- "setosa"
#' iris$group[iris$Species != "setosa"] <- "non-setosa"
#'
#' #make a partial PCADSC object, splitting the data by "group"
#' irisPCADSC <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")),
#'    doCE = FALSE)
#'
#' #No CEInfo available
#' irisPCADSC$CEInfo
#'
#' #Add and show CEInfo
#' irisPCADSC <- doCE(irisPCADSC)
#' irisPCADSC$CEInfo
#'
#' @seealso \code{\link{CEPlot}}, \code{\link{PCADSC}}
#'
#' @export
doCE <- function(x, ...) {
  UseMethod("doCE")
}

#x: pcaRes
#' @export
doCE.pcaRes <- function(x, data, B, ...) {

  #check whether B is positive
  if (B < 1) {
    stop("B must be positive.")
  }

  #Unpack x
  n1 <- x$n1
  nBoth <- x$nBoth
  d <- x$d
  vars <- x$vars

  #Calculate cumulative sums for the observed data
  ceResObs <- calcCum(x)
  xVals <- ceResObs$x
  y.obs <- ceResObs$y
  KS.obs <- ceResObs$KS
  CvM.obs <- ceResObs$CvM

  #Calculate cumulative sums for the randomly partitioned data
  y.sim   <- matrix(0, d+1, B)
  KS.sim  <- rep(0, B)
  CvM.sim <- rep(0, B)

  for (i in 1:B) {
    splitVar <- rep("2", nBoth)
    ii <- sample(1:nBoth, n1)
    splitVar[ii] <- "1"
    data$splitVar <- factor(splitVar)
    thisCEres <- calcCum(doPCA(data, "splitVar", c("1", "2"), vars, doBoth = FALSE),
                         x = xVals)
    y.sim[,i]  <- thisCEres$y
    KS.sim[i]  <- thisCEres$KS
    CvM.sim[i] <- thisCEres$CvM
  }

  #Calculate p-values
  KS.pvalue <- mean(KS.sim >= KS.obs)
  CvM.pvalue <- mean(CvM.sim >= CvM.obs)

  #Pack and return output
  out <- list(d = d, B = B, xVals = xVals, y.obs = y.obs, y.sim = y.sim,
              KS.obs = KS.obs, KS.pvalue = KS.pvalue,
              CvM.obs = CvM.obs, CvM.pvalue = CvM.pvalue)
  class(out) <- "CEInfo"
  out
}

#' @export
doCE.PCADSC <- function(x, ...) {
  x$CEInfo <- doCE(x$pcaRes, x$data, x$B)
  x
}


################Not exported below##################################################

#Calculate cumulative sum of eigenvalue differences (and, if x is NULL, also
#just a cumulative eigenvalue sum for the joint eigenvalues)
calcCum <- function(pcaRes, x = NULL) {
  eigen1 <- pcaRes$eigen1
  eigen2 <- pcaRes$eigen2
  if (is.null(x)) eigenBoth <- pcaRes$eigenBoth

  if (is.null(x)) x <- c(0,cumsum(eigenBoth))
  y   <- c(0,cumsum(eigen1-eigen2))
  KS  <- max(abs(y))
  CvM <- sum(x*(y^2))

  list(x = x, y = y, KS = KS, CvM = CvM)
}
