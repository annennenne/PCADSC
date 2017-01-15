setGeneric("qplot")
setGeneric("plot")

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
#' @importFrom methods setMethod
#' @importFrom ggplot2 qplot ggplot aes_string geom_bar coord_flip scale_x_reverse
#'             scale_y_continuous geom_text xlab ylab theme facet_wrap
#'             as_labeller scale_fill_discrete theme_bw
#'
#'@describeIn PCADSC Plot a PCADSC object
#'@include PCADSC.R
#'@export
setMethod("qplot","PCADSC",
          function(x, varLabels=NULL, covCO=NULL, splitLabels=NULL) {

  pcadscObj <- x
  splitLevels <- pcadscObj@splitLevels
  nCat1 <- pcadscObj@nObs1
  nCat2 <- pcadscObj@nObs2
  splitBy <- pcadscObj@splitBy
  pcaFrame <- pcadscObj@pcaFrame

  if (is.null(varLabels)) {
    varLabels <- pcadscObj@varNames
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
  ggplot(pcaFrame, aes_string(x="comp", y="loading", fill="var")) +
    geom_bar(stat="identity") +
    coord_flip() +
    scale_x_reverse() +
    scale_y_continuous(limits=c(0, 1.4),
                       breaks=c(0, 0.25, 0.5, 0.75, 1)) +
    geom_text(aes_string(label="cvc"), y=1.2, cex=4, na.rm=T) +
    xlab("Principal component") +
    ylab("Standardized loading") +
    theme(legend.position="bottom") +
    facet_wrap(~ group, ncol=2,
               labeller=as_labeller(facetLabels)) +
    scale_fill_discrete(name=NULL, labels=varLabels) +
    theme_bw()
}
)


#'@rdname qplot,PCADSC-method
setMethod("plot", "PCADSC", function(x, y = NULL, ...) qplot(x, ...))
