#' @title Chroma plot
#'
#' @description ??
#'
#' @param x Either a \code{PCADSC} object or a \code{chromaInfo} object, as produced
#' by \code{\link{PCADSC}} and \code{\link{doChroma}}.
#'
#' @param varLabels A vector of character string labels for the variables used in
#' \code{pcadscObj}. If non-null, these labels appear in the plot instead of the
#' variable names. Note that they must be listed in the same order as the variables
#' in \code{pcadscObj} and this order can be expected by calling
#' \code{pcadscObj$varNames}. If \code{NULL} (the default), these variable
#' names are used.
#'
#' @param covCO Variance contribution cut-off
#'
#' @param splitLabels Labels for the two categories of the splitting variable used
#' to create the PCADSC object, \code{pcadscObj}, given as a named list (see examples).
#' These labels will appear in the headers of the two PCADSC plots.
#' If \code{NULL} (the default), the original levels of the splitting variable
#' are used.
#'
#' @param varAnnotation If \code{"cum"} (the default), cummulated explained variance
#' percentages are annotated to the right of the bars for each component. If \code{"raw"},
#' the non-cummulated percentages of explained variance are added instead. If \code{NULL},
#' no annotation is added. Note that \code{"cum"} is only allowed if all components
#' are plottet (see \code{useComp}).
#'
#' @param useComps A vector of integers with the numbers of the principal component
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
#' #make a chroma plot
#' chromaPlot(irisPCADSC)
#'
#'
#' @importFrom methods setMethod
#' @importFrom ggplot2 qplot ggplot aes_string geom_bar coord_flip scale_x_reverse
#'             scale_y_continuous geom_text xlab ylab theme facet_wrap
#'             as_labeller scale_fill_discrete theme_bw
#'
#'@export
chromaPlot <- function(x, varLabels=NULL, covCO=NULL, splitLabels=NULL,
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

  if (is.null(covCO)) covCO <- 1
  if (covCO != 1) {
    maxUseComp <- min(pcaFrame$comp[pcaFrame$cvc_raw > covCO])
    pcaFrame <- pcaFrame[pcaFrame$comp <= maxUseComp, ]
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


