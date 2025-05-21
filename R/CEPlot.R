#' @title Cumulative eigenvalue plot
#'
#' @description Produce a cumulative eigenvalue (CE) plot from a full or partial \code{PCADSC} object,
#' as obtained from a call to \code{\link{PCADSC}}. In either case, this \code{PCADSC} object must have a
#' non-\code{NULL} \code{CEInfo} slot (see examples). The CE plot compares the eigenvalues obtained
#' from PCA performed separately and jointly on two datasets that consist of different observations
#' of the same variables.
#'
#' @details In the x-coordinates, cumulative differences in eigenvalues are shown,
#' while the y-coordinates are the cumulative sum of the joint eigenvalues. The plot may be annotated
#' with Kolmogorov-Smirnov and Cramer-von Mises tests evaluated by permutation tests, testing
#' the null hypothesis of no difference in eigenvalues. The plot also features a number of cumulative
#' simulated cumulative eigenvalue curves as dashed lines. Moreover, a shaded
#' area presents pointwise 95 \% confidence bands for the cumulative difference, also obtained using
#' the permutation test.
#'
#' @param x x A \code{PCADSC} or \code{angleInfo} object, as produced by \code{\link{PCADSC}} or
#' \code{\link{doAngle}}, respectively.
#'
#' @param nDraw A positive integer. The number of simulated cumulative eigenvalue curves that should
#' be added to the plot.
#'
#' @param addTestResults A boolean. If \code{TRUE} (default) the plot is annotated with p-values
#' from Kolmogorov-Smirnov and Cramer-von Mises tests (see details).
#'
#' @examples
#' #load iris data
#' data(iris)
#'
#' #Define grouping variable, grouping the observations by whether their species is
#' #Setosa or not
#' iris$group <- "setosa"
#' iris$group[iris$Species != "setosa"] <- "non-setosa"
#' iris$Species <- NULL
#'
#' \dontrun{
#' #make a PCADSC object, splitting the data by "group"
#' irisPCADSC <- PCADSC(iris, "group")
#'
#' #make a partial PCADSC object from iris and fill out CEInfo in the next call
#' irisPCADSC2 <- PCADSC(iris, "group", doCE = FALSE)
#' irisPCADSC2 <- doCE(irisPCADSC2)
#'
#' #make a CE plot
#' CEPlot(irisPCADSC)
#' CEPlot(irisPCADSC2)
#' }
#'
#' #Only do CE information and use less resamplings for a faster runtime
#' irisPCADSC_fast <- PCADSC(iris, "group", doAngle = FALSE, doChroma = FALSE,
#'   B = 1000)
#' CEPlot(irisPCADSC_fast)
#'
#' @seealso \code{\link{PCADSC}}, \code{\link{doCE}}
#'
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes_string geom_polygon geom_line scale_x_continuous
#' scale_y_continuous theme_bw theme element_blank xlab ylab geom_line
#' scale_linetype_manual annotate unit ggtitle
#' @export
CEPlot <- function(x, nDraw = NULL, addTestResults = TRUE) {

  #Check whether x has a valid class
  objName <- deparse(substitute(x))
  if ("PCADSC" %in% class(x)) {
    if (!is.null(x$CEInfo)) {
      obj <- x$CEInfo
    } else {
      stop(paste(objName, "does not contain any CE information.",
                 "Please call doCE() on", objName, "before making a CEPlot."))
    }
  } else if ("CEInfo" %in% class(x)) {
    obj <- x
  } else {
    stop(paste(objName, "must be of class PCADSC or CEInfo."))
  }

  #Unpack CEInfo object
  xVals <- obj$xVals
  y.obs <- obj$y.obs
  y.sim <- obj$y.sim
  KS.obs <- obj$KS.obs
  KS.pvalue <- obj$KS.pvalue
  CvM.obs <- obj$CvM.obs
  CvM.pvalue <- obj$CvM.pvalue
  B <- obj$B
  d <- obj$d
  splitLevels <- obj$splitLevels

  #Make CE plot
  if(is.null(nDraw)) nDraw <- min(B, 20)
  y.min <- apply(y.sim, 1, quantile, probs=0.025)
  y.max <- apply(y.sim, 1, quantile, probs=0.975)
  y.sim.small <- y.sim[, 1:nDraw]
  ceF <- data.frame(x = rep(xVals, ncol(y.sim.small)),
                    y = c(y.sim.small),
                    type = c(rep("sim", ncol(y.sim.small)*nrow(y.sim.small))),
                    run = c(rep(1:ncol(y.sim.small), each = length(xVals))))
  yMaxVal <- 1.01*max(c(y.obs,as.vector(y.sim)))
  yMinVal <- 1.01*min(c(y.obs,as.vector(y.sim)))
  yBreaks <- round(c(0 + yMaxVal * c(1/3, 2/3, 3/3), 0,
                     0 - yMaxVal * c(1/3, 2/3, 3/3)),1)

    ggout <- ggplot(ceF, aes_string(x = "x", y = "y")) +
      annotate(geom = "polygon", x = c(xVals, rev(xVals)), y = c(y.min, rev(y.max)),
               fill = "aliceblue") +
     geom_line(aes_string(group = "run", linetype = factor("run")), col = "grey", size = 0.1) +
      scale_x_continuous(limits = c(xVals[1],xVals[d+1]), breaks = 0:length(xVals)) +
      scale_y_continuous(limits = c(yMinVal, yMaxVal),
                         breaks = yBreaks) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      xlab("Cumulative sum of joint eigenvalues") +
      ylab("Cumulative difference in eigenvalues") +
      scale_linetype_manual(guide = FALSE, values = rep(1:5, 5*ceiling(nDraw/5))) +
      annotate(geom = "line", x = xVals, y = y.obs, size = 1) +
      ggtitle(paste("Cumulative eigenvalue difference:", splitLevels[1],
                    "-", splitLevels[2]))


    if (addTestResults) {
      ggout <- ggout  + annotate(geom = "label", label =
                                   paste(paste(c("KS:   p = ","CvM: p = "),
                                                 round(c(KS.pvalue,CvM.pvalue),3),sep=""),
                                         collapse = "\n"),
                                 x = -Inf, y = Inf, hjust = "left", vjust = "top",
                                 label.r = unit(0, "lines"),
                                 label.padding = unit(1, "lines"))
    }
    return(ggout)
}


