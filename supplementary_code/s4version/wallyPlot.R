#' @title Wally plot for a PCADSC object
#'
#' @description Creates a Wally plot for a \code{\link{PCADSC}} object,
#'  i.e. a grid of plots where the grouping variable for the PCADSC procedure
#'  has been replaced by random grouping in all but one (the top-left) plot.
#'  Thereby, the plot can be used to access the variability of PCADSC results.
#'
#' @param PCADSCobj A PCADSC object, as created by \code{\link{makePCADSC}}.
#' @param data The dataset used to generate the PCADSC object.
#' @param nrow The number of rows of plots in the outputted Wally plot.
#' @param ncol The number of columns of plo0ts in the outputted Wally plot.
#' @param covCO A cummulated variance cut-off limit. This parameter controls how
#' many components are illustrated in each of the PCADSC plots.
#' @param varAnnotation ...
#' @param useComps ...
#'
#' @importFrom ggplot2 theme xlab ylab element_blank
#' @export
wallyPlot <- function(PCADSCobj, data, nrow, ncol, covCO = 1,
                         varAnnotation = "cum", useComps = NULL) {
  vars <- PCADSCobj@varNames
  nObs1 <- PCADSCobj@nObs1

  nTotal <- nrow * ncol
  plots <- list()
  plots[[1]] <- qplot(PCADSCobj, covCO = covCO, varAnnotation = varAnnotation,
                      useComps = useComps) +
    theme(legend.position = "none") + xlab("") + ylab("") +
    theme(axis.text = element_blank(), axis.ticks = element_blank())
  n <- nrow(data)

  for (i in 2:nTotal) {
    grp1 <- sample(1:n, nObs1)
    data$grp <- "Group 2"
    data$grp[grp1] <- "Group 1"
    plots[[i]] <- qplot(makePCADSC(data, splitBy = "grp", var = vars),
                      covCO = covCO, varAnnotation = varAnnotation,
                      useComps = useComps) +
      theme(legend.position = "none") + xlab("") + ylab("") +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  }
  do.call("grid.arrange", c(plots, ncol = ncol, nrow = nrow))
}
