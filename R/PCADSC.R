#' A PCADSC object
#'
#' Blabla
#'
#' @examples
#' #load iris data
#'   data(iris)
#'
#' #Define grouping variable, grouping the observations by whether their species is
#' #Setosa or not
#'  iris$group <- "setosa"
#'  iris$group[iris$Species != "setosa"] <- "non-setosa"
#'
#' #make a PCADSC object, splitting the data by "group"
#'  irisPCADSC <- makePCADSC(iris, "group",
#'                         var=setdiff(names(iris), c("group", "Species")))
#' #plot it
#'  qplot(irisPCADSC)
#'
#' #print it
#'  print(irisPCADSC)
#'
#' @importFrom methods setClass
#' @export
setClass("PCADSC",
                   slots = list(pcaFrame = "data.frame", splitBy = "character",
                                splitLevels = "character", varNames = "character",
                                n1 = "numeric", n2 = "numeric", nObs1 = "numeric",
                                nObs2 = "numeric"))
