#' @title Compute angle information
#'
#' @description Computes the information that is needed in order to make an \code{\link{anglePlot}}
#' from a \code{PCADSC} or \code{pcaRes} object. Typically, this function is called on a partial
#' \code{PCADSC} object in order to add \code{angleInfo} (see examples).
#'
#' @param x Either a \code{PCADSC} or a \code{pcaRes} object.
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
#'    doAngle = FALSE)
#'
#' #No angleInfo available
#' irisPCADSC$angleInfo
#'
#' #Add and show angleInfo
#' irisPCADSC <- doAngle(irisPCADSC)
#' irisPCADSC$angleInfo
#'
#' @seealso \code{\link{anglePlot}}, \code{\link{PCADSC}}
#'
#' @export
doAngle <- function(x) {
  UseMethod("doAngle")
}

#x: pcaRes
#' @export
doAngle.pcaRes <- function(x) {
  load1 <- x$load1
  load2 <- x$load2
  eigen1 <- x$eigen1
  eigen2 <- x$eigen2
  n1 <- x$n1
  n2 <- x$n2
  d <- x$d
  splitBy <- x$splitBy
  splitLevels <- x$splitLevels
  vars <- x$vars

  #make angleFrame for plotting

  # find angles
  angles <- matrix(0, d, d)
  for (i in 1:d) for (j in 1:d) angles[i,j] <- asin(max(0,min(1,abs(sum(load1[,i]*load2[,j])))))

  #Calculate vector lengths
  max.eigen <- max(c(eigen1[1],eigen2[1]))
  len1 <- len2 <- matrix(NA, d, d)

  for (i in 1:d) for (j in 1:d) {
    len1[i,j] <- sqrt(eigen1[i]/max.eigen)*abs(sum(load1[,i]*load2[,j]))
    len2[i,j] <- sqrt(eigen2[j]/max.eigen)*abs(sum(load1[,i]*load2[,j]))
  }

  #Store in data.frame ready for plotting
  aF <- data.frame(x = rep(1:d, 2*d),
                   y = rep(rep(1:d, each = d),2),
                   xend = c(rep(1:d, d) + c(len1)*cos((pi-c(angles))/2),
                            rep(1:d, d) + c(len2)*cos(c(angles)/2)),
                   yend = c(rep(1:d, each = d) + c(len1)*sin((pi-c(angles))/2),
                            rep(1:d, each = d) + c(len2)*sin(c(angles)/2)),
                   type = rep(c("1st", "2nd"), each = d*d))

  #pack and return output
  out <- list(aF = aF, splitLevels = splitLevels, d = d)
  class(out) <- "angleInfo"
  out
}

#' @export
doAngle.PCADSC <- function(x) {
  x$angleInfo <- doAngle(x$pcaRes)
  x
}
