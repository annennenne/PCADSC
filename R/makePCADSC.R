#' @title Compute the elements used for PCADSC
#' @description Calculate standardized PCA loadings and accumulated variance contributions
#' separately on two parts of a dataset in order to perform non-parametric data structure
#' comparisons.
#'
#' @param data A dataset, either a \code{data.frame} or a \code{matrix} with variables
#' in columns and observations in rows. WHAT TO DO ABOUT DATA.TABLES AND ALSO TIBBLES. BOTH WILL BE
#' PROBLEMATIC AS OF NOW.
#'
#' @param splitBy A grouping variable with two levels defining the two groups within the
#' dataset whose data structures we wish to compare. If \code{splitBy} has more than
#' two levels, only the first two levels are used.
#'
#' @param var The variable names in \code{data} to include in the PCADSC. If \code{NULL}
#' (the default), all variables except for \code{splitBy} are used.
#'
#' @param covCO Not implemented yet (WHAT DID I HAVE IN MIND HERE? CUMMULATED VARIANCE
#' CONTRIBUTION CUT-OFF, MAYBE? SHOULDN'T BE IN THE OBJECT THOUGH, SHOULD RATHER
#' BE IN THE PLOTTING FUNCTION)
#'
#' @return A list including the following objects: ...
#'
#' @details Blablabla.
#' Note that \code{\link[tibble]{tibble}}s and \code{\link[data.table]{data.table}}s are
#' accepted as input, but they are instantly converted to \code{\link{data.frame}}s. Future
#' releases might include specific implementation for these data representations.
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
#' qplot(irisPCADSC)
#' print(irisPCADSC)
#'
#' @seealso \code{\link{PCADSC}}, \code{\link{print,PCADSC-method}},
#'  \code{\link{qplot,PCADSC-method}}
#'
#' @export
makePCADSC <- function(data, splitBy, var=NULL, covCO=NULL) {
  #define var
  if (is.null(var)) var <- setdiff(names(data), splitBy)

  #If data is tibble, data.table or matrix, convert it to data.frame
  #If data is neither, throw error
  if (any(class(data) %in% c("data.table", "tbl", "tbl_df", "matrix"))) {
    data <- as.data.frame(data)
    message("Note: The data was converted to a data.frame in order for PCADSC to run.")
  }
  if (class(data) != "data.frame") {
    stop("The inputted data must be of type data.frame, data.table, tibble or matrix.")
  }

  #check if all variables are numeric
  isNum <- sapply(data[, var], "is.numeric")
  if (!all(isNum)) {
    stop(paste("All variables must be numeric for PCA decomposition",
               "to be meaningful. The following non-numeric variables",
               "were found:", paste(names(isNum[!isNum]), collapse = ", ")))
  }

  #check if splitBy has more/less than two levels
  splitLevels <- unique(data[, splitBy])
  if (length(splitLevels) != 2) {
    stop(paste("splitLevels must have exactly two levels,",
               "but it was found to have", length(splitLevels),
               "levels."))
  }

  #split and standardize data
  data1 <- stdData(data[data[, splitBy]==splitLevels[1], var])
  data2 <- stdData(data[data[, splitBy]==splitLevels[2], var])

  #do PCA
  res1 <- loadComp(data1, var)
  res2 <- loadComp(data2, var)
  load1 <- res1$loadings
  load2 <- res2$loadings
  n1 <- nrow(load1)
  n2 <- nrow(load2)
  nObs1 <- res1$nObs
  nObs2 <- res2$nObs
  pcaFrame <- as.data.frame(rbind(load1, load2))
  pcaFrame$group <- c(rep(as.character(splitLevels[1]), n1),
                      rep(as.character(splitLevels[2]), n2))

  out <- PCADSC(pcaFrame=pcaFrame, splitBy=splitBy, splitLevels=splitLevels,
                varNames=var, n1=n1, n2=n2, nObs1=nObs1, nObs2=nObs2)
  out
}




################Not exported below##################################################

#Standardize each variable in a dataset (subtract mean, divide by SD)
#' @importFrom stats sd
stdData <- function(data) {
  as.data.frame(lapply(data, function(x) (x - mean(x))/sd(x)))
}
