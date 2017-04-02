#' @title Cumulative eigenvalue plot
#'
#' @description Compares eigenvalues by cumulative Bland-Altman type plots.
#' Kolmogorov-Smirnov and Cramer-von Mises tests evaluated by permutation tests
#'
#' @param x ...
#'
#' @param nDraw ...
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
#' #make a PCADSC object, splitting the data by "group"
#' irisPCADSC <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")))
#'
#' #make a CE plot
#' CEPlot(irisPCADSC)
#'
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes_string geom_polygon geom_line scale_x_continuous
#' scale_y_continuous theme_bw theme element_blank xlab ylab geom_line
#' scale_linetype_manual annotate unit
#' @export
CEPlot <- function(x, nDraw = NULL) {

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

    ggplot(ceF, aes_string(x = "x", y = "y")) +
      annotate(geom = "polygon", x = c(xVals, rev(xVals)), y = c(y.min, rev(y.max)),
               fill = "aliceblue") +
     geom_line(aes_string(group = "run", linetype = factor("run")), col = "grey", size = 0) +
      scale_x_continuous(limits = c(xVals[1],xVals[d+1]), breaks = 0:length(xVals)) +
      scale_y_continuous(limits = c(yMinVal, yMaxVal),
                         breaks = yBreaks) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      xlab("Cumulative sum of joint eigenvalues") +
      ylab("Cumulative difference in eigenvalues") +
      scale_linetype_manual(guide = FALSE, values = rep(1:5, 5*ceiling(nDraw/5))) +
      annotate(geom = "label", label = paste(paste(c("KS:   p = ","CvM: p = "),
                                                  round(c(KS.pvalue,CvM.pvalue),3),sep=""),
                                            collapse = "\n"),
               x = -Inf, y = Inf, hjust = "left", vjust = "top",
               label.r = unit(0, "lines"),
               label.padding = unit(1, "lines")) +
      annotate(geom = "line", x = xVals, y = y.obs, size = 1)
}


