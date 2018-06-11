#' @title Compute angle information
#'
#' @description Computes the information that is needed in order to make an \code{\link{anglePlot}}
#' from a \code{PCADSC} or \code{pcaRes} object. Typically, this function is called on a partial
#' \code{PCADSC} object in order to add \code{angleInfo} (see examples).
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
#' iris$Species <- NULL
#'
#'\dontrun{
#' #make a partial PCADSC object, splitting the data by "group"
#' irisPCADSC <- PCADSC(iris, "group", doAngle = FALSE)
#'
#' #No angleInfo available
#' irisPCADSC$angleInfo
#'
#' #Add and show angleInfo
#' irisPCADSC <- doAngle(irisPCADSC)
#' irisPCADSC$angleInfo
#' }
#'
#' #Make a partial PCADSC object and only add angle information for a
#' #faster runtime
#' irisPCADSC_fast <- PCADSC(iris, "group", doAngle = FALSE,
#'   doChroma = FALSE, doCE = FALSE)
#' irisPCADSC_fast <- doAngle(irisPCADSC_fast, B = 100)
#' irisPCADSC_fast$angleInfo
#'
#' @seealso \code{\link{anglePlot}}, \code{\link{PCADSC}}
#'
#' @export
doAngle <- function(x, ...) {
  UseMethod("doAngle")
}

#x: pcaRes
#' @export
doAngle.pcaRes <- function(x, data, B, ...) {
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
  angles <- acos(abs(t(load1)%*%load2))

  #Calculate vector lengths
  max.eigen <- max(c(eigen1[1],eigen2[1]))
  len1 <- len2 <- matrix(NA, d, d)

  for (i in 1:d) for (j in 1:d) {
    len1[i,j] <- sqrt(eigen1[i]/max.eigen)*abs(sum(load1[,i]*load2[,j]))
    len2[i,j] <- sqrt(eigen2[j]/max.eigen)*abs(sum(load1[,i]*load2[,j]))
  }

  #Store in data.frame ready for plotting
  aF <- data.frame(x=NULL,y=NULL,xend=NULL,yend=NULL,type=NULL)
  for (i in 1:d) for (j in 1:d) {
    aF <- rbind(aF,
                data.frame(x=c(i,i),y=c(j,j),
                           xend=c(i+len1[i,j]*cos(pi/4+angles[i,j]/2),i+len2[i,j]*cos(pi/4-angles[i,j]/2)),
                           yend=c(j+len1[i,j]*sin(pi/4+angles[i,j]/2),j+len2[i,j]*sin(pi/4-angles[i,j]/2)),
                           type=c("1st","2nd"))
                )
  }

  # -------------------------------------------------------------------
  # -------------------------------------------------------------------
  # The following is change from corresponding simulation part of doCE
  splitLevels <- x$splitLevels

  #check whether B is positive
  if (B < 1) {
    stop("B must be positive.")
  }

  #Unpack x
  n1 <- x$n1
  nBoth <- x$nBoth
  d <- x$d
  vars <- x$vars

  #Calculate cumulative sums for the randomly partitioned data
  angles.sim   <- array(0, dim=c(d,d,B))
  for (i in 1:B) {
    splitVar <- rep("2", nBoth)
    splitVar[sample(1:nBoth, n1)] <- "1"
    data$splitVar <- factor(splitVar)
    myPCA <- doPCA(data, "splitVar", c("1", "2"), vars, doBoth = FALSE)
    angles.sim[,,i] <- acos(abs(t(myPCA$load1)%*%myPCA$load2))
  }

  # length in confidence regions
  len <- sqrt(x$eigenBoth / x$eigenBoth[1])

  # Make data frame for visualizing confidence region
  pval <- matrix(0,d,d); colnames(pval) <- colnames(angles); rownames(pval) <- rownames(angles)
  cR <- data.frame(g=NULL,quantile=NULL,x=NULL,y=NULL)
  for (i in 1:d) for (j in 1:d) {
    if (i==j) {
      pval[i,j] <- mean(angles[i,j]<=angles.sim[i,j,])
    } else {
      pval[i,j] <- mean(angles[i,j]>=angles.sim[i,j,])
    }
    my.q <- quantile(angles.sim[i,j,],seq(0,1,0.05))
    for (k in 1:20) {
      cR <- rbind(cR,
                  data.frame(g=paste(c(i,j,"1st"),collapse="."),
                             quantile=ifelse(i==j,k,21-k),
                             x=c(i,i+len[i]*cos(my.q[k+0:1])*cos(pi/4+my.q[k+0:1]/2)),
                             y=c(j,j+len[i]*cos(my.q[k+0:1])*sin(pi/4+my.q[k+0:1]/2))),
                  data.frame(g=paste(c(i,j,"2nd"),collapse="."),
                             quantile=ifelse(i==j,k,21-k),
                             x=c(i,i+len[j]*cos(my.q[k+0:1])*cos(pi/4-my.q[k+0:1]/2)),
                             y=c(j,j+len[j]*cos(my.q[k+0:1])*sin(pi/4-my.q[k+0:1]/2))))
    }
  }
  cR$quantile <- factor(cR$quantile)

  # -------------------------------------------------------------------
  # -------------------------------------------------------------------

  #pack and return output
  out <- list(aF = aF, splitLevels = splitLevels, cR = cR, pvalue=pval, d = d)
  class(out) <- "angleInfo"
  out
}

#' @export
doAngle.PCADSC <- function(x, ...) {
  if ("B" %in% names(list(...))) b <- list(...)$B
  else b <- x$B

  x$angleInfo <- doAngle(x$pcaRes, x$data, b)
  x
}
