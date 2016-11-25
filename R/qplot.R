#' @title Plot a PCADSC object
#'
#' @description Given af PCADSC object, \code{pcadscObj}, (as produced by \code{\link{PCADSC}}), a data
#' structure comparison plot is generated in the style of  \code{ggplot2}.
#'
#' @param pcadscObj A PCADSC object (see \code{\link{PCADSC}}).
#'
#' @param varLabels A vector of character string labels for the variables used in
#' \code{pcadscObj}. If non-null, these labels appear in the plot instead of the
#' variable names. Note that they must be listed in the same order as the variables
#' in \code{pcadscObj} and this order can be expected by calling
#' \code{pcadscObj$varNames}. If \code{NULL} (the default), these variable
#' names are used.
#'
#' @param covCO Not implemented, something like a variance contribution cut-off, maybe?
#'
#' @param splitLabels Labels for the two categories of the splitting variable used
#' to create the PCADSC object, \code{pcadscObj}, given as a named list (see examples).
#' These labels will appear in the headers of the two PCADSC plots.
#' If \code{NULL} (the default), the original levels of the splitting variable
#' are used.
#'
#' @return A PCADSC plot.
#'
#' @examples
#' #Example showing how to use splitLabels
#'
#' @seealso \code{\link{PCADSC}}
#' @importFrom ggplot2 ggplot qplot
#' @export
qplot.PCADSC <- function(pcadscObj, varLabels=NULL, covCO=NULL,
                         splitLabels=NULL, ...) {
  splitLevels <- pcadscObj$splitLevels
  nCat1 <- pcadscObj$nObs1
  nCat2 <- pcadscObj$nObs2
  splitBy <- pcadscObj$splitBy
  pcaFrame <- pcadscObj$pcaFrame

  if (is.null(varLabels)) {
    varLabels <- pcadscObj$varNames
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

  ggplot(pcaFrame, aes(x=comp, y=loading, fill=var)) +
    geom_bar(stat="identity") +
    coord_flip() +
    scale_x_reverse(breaks=c(1, seq(10, 50, 10))) +
    scale_y_continuous(limits=c(0, 1.4),
                       breaks=c(0, 0.25, 0.5, 0.75, 1)) +
    geom_text(aes(label=cvc), y=1.2, cex=4, na.rm=T) +
    xlab("Principal component") +
    ylab("Standardized loading") +
    theme(legend.position="bottom") +
    facet_wrap(~ group, ncol=2,
               labeller=as_labeller(facetLabels)) +
    scale_fill_discrete(name=NULL, labels=varLabels)
}
