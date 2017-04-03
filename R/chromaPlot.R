#' @title Chroma plot
#'
#' @description Produce a chroma plot from a full or partial \code{PCADSC} object, as obtained
#' from a call to \code{\link{PCADSC}}. In either case, this \code{PCADSC} object must have a
#' non-\code{NULL} \code{chromaInfo} slot (see examples). The chroma plot compares the loading
#' patterns from PCA conducted on two datasets consisting of different observations of the
#' same variables.
#'
#' @details The plot consists of one display for each of the two datasets. The two displays both
#' consist of a number of vertical bars. Each vertical bar represents a principal component and the
#'  width of each colored section (chroma) within the bar corresponds to the normalized PCA loading
#'  vector of that component. The bars can be annotated with the (cumulative) variance contributions
#'  of the components (see \code{varAnnotation}).
#'
#' @param x Either a \code{PCADSC} object or a \code{chromaInfo} object, as produced
#' by \code{\link{PCADSC}} and \code{\link{doChroma}}.
#'
#' @param varLabels A vector of character string labels for the variables used in
#' \code{pcadscObj}. If non-\code{NULL}, these labels appear in the plot instead of the
#' variable names.
#'
#' @param cvCO A numeric in the interval \eqn{[0,1]} where the default, \code{1}, corresponds
#' to no cut-off value. If a value smaller than 1, only the first \eqn{n} components are plotted,
#' where \eqn{n} is the the lowest possible number, such that the cumulative variance contribution
#' of the first \eqn{n} components exceeds \code{cvCO} for both datasets. Note that setting
#' \code{covCO} will overrule the argument \code{useComps}.
#'
#' @param splitLabels Labels for the two categories of the splitting variable used
#' to create the \code{PCADSC} object, \code{x}, given as a named list (see examples).
#' These labels will appear in the headers of the two PCADSC plots.
#' If \code{NULL} (the default), the original levels of the splitting variable
#' are used.
#'
#' @param varAnnotation If \code{"cum"} (the default), cummulated explained variance
#' percentages are annotated to the right of the bars for each component. If \code{"raw"},
#' the non-cummulated percentages of explained variance are added instead. If \code{NULL},
#' no annotation is added. Note that \code{"cum"} is only allowed if \code{useComps} is
#' non-\code{NULL}.
#'
#' @param useComps A vector of integers with the indexes of the principal component
#' that should be included in the plot.
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
#' #make a partial PCADSC object from iris and fill out chromaInfo in the next call
#' irisPCADSC2 <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")),
#'    doChroma = FALSE)
#' irisPCADSC2 <- doChroma(irisPCADSC2)
#'
#' #make a CE plot
#' CEPlot(irisPCADSC)
#' CEPlot(irisPCADSC2)
#'
#' #make a chroma plot
#' chromaPlot(irisPCADSC)
#' chromaPlot(irisPCADSC)
#'
#' #Change the labels of the splitting variable
#' chromaPlot(irisPCADSC, splitLabels = list("non-setosa" = "Not Setosa",
#'     "setosa" = "Setosa"))
#'
#'
#' #Only plot components 1 and 4 and remove annotated variances
#' chromaPlot(irisPCADSC, useComps = c(1,4), varAnnotation = "no")
#'
#' #Only plot the first components responsible for explaining 80 percent variance
#' chromaPlot(irisPCADSC, cvCO = 0.8)
#'
#' #Change variable labels
#' chromaPlot(irisPCADSC, varLabels = c("Sepal length", "Sepal width", "Petal length",
#'    "Petal width"))
#'
#' @seealso \code{\link{PCADSC}}, \code{\link{doChroma}}
#'
#' @importFrom methods setMethod
#' @importFrom ggplot2 qplot ggplot aes_string geom_bar coord_flip scale_x_reverse
#'             scale_y_continuous geom_text xlab ylab theme facet_wrap
#'             as_labeller scale_fill_discrete theme_bw
#'
#'@export
chromaPlot <- function(x, varLabels = NULL, cvCO = 1, splitLabels = NULL,
                        varAnnotation = "cum", useComps = NULL) {

  #Check whether x has a valid class
  objName <- deparse(substitute(x))
  if ("PCADSC" %in% class(x)) {
    if (!is.null(x$chromaInfo)) {
      obj <- x$chromaInfo
    } else {
     stop(paste(objName, "does not contain any chroma information.",
                 "Please call doChroma() on", objName, "before making a chromaPlot."))
    }
  } else if ("chromaInfo" %in% class(x)) {
    obj <- x
  } else {
    stop(paste(objName, "must be of class PCADSC or chromaInfo."))
  }

  splitLevels <- obj$splitLevels
  nCat1 <- obj$n1
  nCat2 <- obj$n2
  splitBy <- obj$splitBy
  pcaFrame <- obj$cF

  if (cvCO > 1 | cvCO < 0) {
    stop("An invalid value of cvCO was supplied. cvCO must be in the interval [0,1]")
  }
  if (cvCO != 1) {
    maxUseComp <- min(pcaFrame$comp[pcaFrame$cvc_raw > cvCO])
    pcaFrame <- pcaFrame[pcaFrame$comp <= maxUseComp, ]
    if (!is.null(useComps)) {
      warning(paste("Both cvCO and useComps arguments were supplied at the same time.",
                    "These arguments are incompatible and thus only the cvCO argument",
                    "was used."))
      useComps <- NULL
    }
  }
  if (!is.null(useComps)) {
    pcaFrame <- pcaFrame[pcaFrame$comp %in% useComps, ]
    if (!is.null(varAnnotation) && varAnnotation != "no" && varAnnotation == "cum") {
      warning(paste("Annotation by cummulated variances is not allowed when",
                    "not all components are plotted. Therefore, raw variance",
                    "contribution percentages are shown instead."))
      varAnnotation <- "raw"
    }
  }
  comps <- unique(pcaFrame$comp)
  nComp <- length(comps)

  if (is.null(varAnnotation)) {
    varAnnotation <- "no"
  }
  if (varAnnotation == "cum") {
    pcaFrame$ann <- pcaFrame$cvc
  } else if (varAnnotation == "raw") {
    pcaFrame$ann <- pcaFrame$varpct
  }

  if (is.null(varLabels)) {
    varLabels <- obj$vars
  }
  if (is.null(splitLabels)) {
    splitLabels <- splitLevels
  } else {
    sl1 <- splitLabels[[which(names(splitLabels)==splitLevels[1])]]
    sl2 <- splitLabels[[which(names(splitLabels)==splitLevels[2])]]
    splitLabels <- c(sl1, sl2)
  }



  facetLabels <- c(paste(splitLabels[1], ", n = ", nCat1, sep=""),
                   paste(splitLabels[2], ", n = ", nCat2, sep=""))
  names(facetLabels) <- splitLevels

  #browser()
  #note: aes_string is used for devtools::check() to stop complaining
  #about undocumented variables. It is not necessary at all.
  p <- ggplot(pcaFrame, aes_string(x="comp", y="loading", fill="var")) +
    geom_bar(stat="identity") +
    coord_flip() +
    scale_x_reverse(breaks = unique(pcaFrame$comp)) +
    xlab("Principal component") +
    ylab("Standardized loading") +
    facet_wrap(~ group, ncol=2,
               labeller=as_labeller(facetLabels)) +
    scale_fill_discrete(name=NULL, labels=varLabels) +
    theme_bw() +
    theme(legend.position="bottom")
  if (varAnnotation == "no") {
    return(p + scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1)))
  } else {
    p +
      scale_y_continuous(limits=c(0, 1.4),
                         breaks=c(0, 0.25, 0.5, 0.75, 1)) +
      geom_text(aes_string(label="ann"), y=1.2, cex=4, na.rm=T)
  }
}


