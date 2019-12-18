#' @title Compute chroma information
#'
#' @description Computes the information that is needed in order to make a \code{\link{chromaPlot}}
#' from a \code{PCADSC} or \code{pcaRes} object. Typically, this function is called on a partial
#' \code{PCADSC} object in order to add \code{chromaInfo} (see examples).
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
#' iris$Species <- NULL
#'
#' \dontrun{
#' #make a partial PCADSC object, splitting the data by "group"
#' irisPCADSC <- PCADSC(iris, "group", doChroma = FALSE)
#'
#' #No chromaInfo available
#' irisPCADSC$chromaInfo
#'
#' #Add and show chromaInfo
#' irisPCADSC <- doChroma(irisPCADSC)
#' irisPCADSC$chromaInfo
#' }
#'
#' #Make a partial PCADSC object and only add chroma information for a
#' #faster runtime
#' irisPCADSC_fast <- PCADSC(iris, "group", doAngle = FALSE,
#'   doChroma = FALSE, doCE = FALSE)
#' irisPCADSC_fast <- doChroma(irisPCADSC_fast)
#' irisPCADSC_fast$chromaInfo
#'
#' @seealso \code{\link{chromaPlot}}, \code{\link{PCADSC}}
#'
#' @export
doChroma <- function(x) {
  UseMethod("doChroma")
}

#x: pcaRes
#' @export
doChroma.pcaRes <- function(x) {
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

  #make chromaFrame for plotting
  cF1 <- chromaFrame(load1, eigen1, d, vars)
  cF2 <- chromaFrame(load2, eigen2, d, vars)
  cF <- as.data.frame(rbind(cF1, cF2))
  cF$group <- rep(splitLevels, each = d^2)

  #pack and return output
  out <- list(splitLevels = splitLevels, n1 = n1, n2 = n2, splitBy = splitBy,
              cF = cF, vars = vars)
  class(out) <- "chromaInfo"
  out
}

#' @export
doChroma.PCADSC <- function(x) {
  x$chromaInfo <- doChroma(x$pcaRes)
  x
}




################Not exported below##################################################

#Compute standardized PCA loadings and cummulative variance contributions,
#pack it up for plotting in a data.frame
#' @importFrom stats na.omit princomp
#' @importFrom reshape2 melt
chromaFrame <- function(loadings, eigenvalues, d, vars) {
 #p <- pca
 # p <- stats::princomp(sapply(stats::na.omit(data[, vars]), as.numeric))
  px <- round(matrix(c(loadings), d, dimnames=list(vars, 1:d)), 4)

  for (i in 1:d) { #standardize
    px[, i] <- abs(px[, i]/sum(abs(px[, i])))
  }

  #cummulative variance contributions and
  #principal component variance constribution
  pcvc <- eigenvalues
  pcvc <- pcvc/sum(pcvc)
  cpcvc <- cvc <- cumsum(pcvc)
  cvc <- paste(round(cvc*100, 2), "%")
  varpct <- paste(round(pcvc*100, 2), "%")

  #combine
  pxx <- melt(px)
  pxx$varpct_raw <- pxx$cvc_raw <- pxx$cvc <- pxx$varpct <- rep(NA, nrow(pxx))
  for (i in 1:d) {
    thisComp <- which(pxx$Var2==i)
    pxx$cvc[thisComp[1]] <- cvc[i]
    pxx$varpct[thisComp[1]] <- varpct[i]
    pxx$cvc_raw[thisComp] <- cpcvc[i]
    pxx$varpct_raw[thisComp] <- pcvc[i]
  }

  colnames(pxx) <- c("var", "comp", "loading", "varpct", "cvc", "cvc_raw", "varpct_raw")
  pxx
}
