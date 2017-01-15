setGeneric("print")

#' @title Print a PCADSC object
#' @description Print an overview of the results of PCADSC, as represented by a PCADSC object generated
#' by \code{\link{makePCADSC}}.
#'
#' @param x A PCADSC object
#' @param ... For internal use only
#'
#' @seealso \code{\link{makePCADSC}} \code{\link{plotPCADSC}}
#'
#' @examples
#'
#' #load iris data
#' data(iris)
#'
#' #Define grouping variable, grouping the observations by whether their species is
#' #Setosa or not
#' iris$group <- "setosa"
#' iris$group[iris$Species != "setosa"] <- "non-setosa"
#'
#' #make a PCADSC object, splitting the data by "group"
#' irisPCADSC <- makePCADSC(iris, "group",
#'                         var=setdiff(names(iris), c("group", "Species")))
#' print(irisPCADSC)
#'
#' @importFrom pander pander
#' @importFrom methods setMethod
#' @export
setMethod("print", "PCADSC", function(x, ...) {
  pcadscObj <- x
  frame <- pcadscObj@pcaFrame
  splitBy <- pcadscObj@splitBy
  splitLevels <- pcadscObj@splitLevels
  nObs1 <- pcadscObj@nObs1
  nObs2 <- pcadscObj@nObs2
  cvc1 <- cumsum(unique(frame$pcvc[frame$group == splitLevels[1]]))
  cvc2 <- cumsum(unique(frame$pcvc[frame$group == splitLevels[2]]))
  nImportant1 <- min(which(cvc1 > 0.95))
  nImportant2 <- min(which(cvc2 > 0.95))
  name1 <- paste("\"", splitLevels[1], "\"", sep="")
  name2 <- paste("\"", splitLevels[2], "\"", sep="")

  nComp <- max(frame$comp)
  var1_g1 <- var1_g2 <- var2_g1 <- var2_g2 <- rep(NA, nComp)
  for (i in 1:nComp) {
    f1 <- frame[frame$comp== i & frame$group == splitLevels[1], c("var", "loading")]
    var_g1 <- as.character(f1[order(f1$loading, decreasing = TRUE), "var"])
    f2 <- frame[frame$comp== i & frame$group == splitLevels[2], c("var", "loading")]
    var_g2 <- as.character(f2[order(f2$loading, decreasing = TRUE), "var"])
    var1_g1[i] <- var_g1[1]
    var2_g1[i] <- var_g1[2]
    var1_g2[i] <- var_g2[1]
    var2_g2[i] <- var_g2[2]
  }
  maxImportant <- max(nImportant1, nImportant2)
  loadFrame <- data.frame(component = 1:maxImportant,
                          g1 <- paste(var1_g1, var2_g1, sep=", ")[1:maxImportant],
                          g2 <- paste(var1_g2, var2_g2, sep=", ")[1:maxImportant])
  names(loadFrame) <- c("component", splitLevels)
  cat(paste(
    paste("PCADSC results with data splitting by", pcadscObj@splitBy),
    "",
    paste("Number of observations in group ", name1, ": ", nObs1, sep=""),
    paste("Number of observations in group ", name2, ": ", nObs2, sep=""),
    paste("Number of principal components needed to explain 95% variance:",
          nImportant1, "in group", name1, "and", nImportant2, "in group", name2),
    "",
    "Main loading contributors:",
    sep="\n"))
  cat(pander(loadFrame))
}
)

#'@export
setMethod("show", "PCADSC", function(object) print(object))
