#' @title Angle plot
#'
#' @description Produce an angle plot from a full or partial \code{PCADSC} object, as obtained
#' from a call to \code{\link{PCADSC}}. In either case, this \code{PCADSC} object must have a
#' non-\code{NULL} \code{anleInfo} slot (see examples). The angle plot compares the eigenvalue-
#' and loading patterns from PCA performed on two datasets that consist of different observations
#' of the same variables.
#'
#' @param x A \code{PCADSC} or \code{angleInfo} object, as produced by \code{\link{PCADSC}} or
#' \code{\link{doAngle}}, respectively.
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
#' #make a full PCADSC object, splitting the data by "group"
#' irisPCADSC <- PCADSC(iris, "group")
#'
#' #make a partial PCADSC object from iris and fill out angleInfo in the next call
#' irisPCADSC2 <- PCADSC(iris, "group", doAngle = FALSE)
#' irisPCADSC2 <- doAngle(irisPCADSC2)
#'
#' #make an angle plot
#' anglePlot(irisPCADSC)
#' anglePlot(irisPCADSC2)
#' }
#'
#' #Only do angle information for a faster run-time
#' irisPCADSC_fast <- PCADSC(iris, "group", doCE = FALSE, doChroma = FALSE)
#' anglePlot(irisPCADSC_fast)
#'
#' @seealso \code{\link{PCADSC}}, \code{\link{doAngle}}
#'
#' @importFrom ggplot2 ggplot aes_string scale_x_continuous scale_y_continuous
#' theme_bw theme element_blank xlab ylab geom_segment unit scale_color_manual
#' geom_polygon
#' @importFrom grid arrow
#' @export
anglePlot <- function(x) {

  #graphical settings
  arrow.len <- 0.05

  #Check whether x has a valid class
  objName <- deparse(substitute(x))
  if ("PCADSC" %in% class(x)) {
    if (!is.null(x$angleInfo)) {
      obj <- x$angleInfo
    } else {
      stop(paste(objName, "does not contain any angle information.",
                 "Please call doAngle() on", objName, "before making an anglePlot."))
    }
  } else if ("angleInfo" %in% class(x)) {
    obj <- x
  } else {
    stop(paste(objName, "must be of class PCADSC or angleInfo."))
  }

  #Unpack angleInfo object
  aF <- obj$aF
  d <- obj$d
  splitLevels <- obj$splitLevels

  # length in confidence regions
  len <- sqrt(obj$eigenBoth / obj$eigenBoth[1])

  # Make data frame for adding 95pct confidence intervals
  cR <- data.frame(g=NULL,x=NULL,y=NULL)
  for (i in 1:d) for (j in 1:d) {
    if (i==j) {
      theta <- seq(0,quantile(obj$angles.sim[i,j,],0.95),length.out = 100)
    } else {
      theta <- seq(quantile(obj$angles.sim[i,j,],0.025),
                   quantile(obj$angles.sim[i,j,],0.975),length.out = 100)
    }
    cR <- rbind(cR,
                data.frame(g=paste(c(i,j,"1st"),collapse="."),
                           x=c(i,i+len[i]*cos(theta)*cos(pi/4+theta/2)),
                           y=c(j,j+len[i]*cos(theta)*sin(pi/4+theta/2))),
                data.frame(g=paste(c(i,j,"2nd"),collapse="."),
                           x=c(i,i+len[j]*cos(theta)*cos(pi/4-theta/2)),
                           y=c(j,j+len[j]*cos(theta)*sin(pi/4-theta/2))))
  }

  #Make anglePlot
  ggplot(cR,aes_string(group = "g", x = "x", y = "y")) +
    geom_polygon(fill = "gray85") +
    geom_segment(aes_string(x = "x", y = "y", col = "type",
                            xend = "xend", yend = "yend"),
                 aF,arrow = arrow(length = unit(arrow.len, "inch")),inherit.aes = FALSE) +
    scale_x_continuous(limits = c(0.5,d+0.5), breaks = 1:d) +
    scale_y_continuous(limits = c(0.5,d+0.5), breaks = 1:d) +
    scale_color_manual(values = c("blue", "red"), guide = FALSE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab(paste("PCs for", splitLevels[1])) +
    ylab(paste("PCs for", splitLevels[2]))
  }
