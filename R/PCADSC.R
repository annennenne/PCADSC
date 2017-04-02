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
#' @param vars The variable names in \code{data} to include in the PCADSC. If \code{NULL}
#' (the default), all variables except for \code{splitBy} are used.
#'
#' @param doCE ...
#'
#' @param doAngle ...
#'
#' @param doChroma ...
#'
#' @param B ...
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
#' irisPCADSC <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")))
#'
#'
#' @seealso \code{\link{doCE}}, \code{\link{doAngle}}, \code{\link{doChroma}}
#'
#' @export
PCADSC <- function(data, splitBy, vars=NULL, doCE = TRUE,
                   doAngle = TRUE, doChroma = TRUE,
                   B = 1000) {
  #define var
  if (is.null(vars)) vars <- setdiff(names(data), splitBy)

  #TO DO:
  #-  check if any variables are essentially empty. This will cause an error
  #   as we then divide by zero when standardizing
  #-  Make sure it deals with factor splitBy varibales correctly
  #-  Deal with missing values


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
  isNum <- sapply(data[, vars], "is.numeric")
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

  #split data, standardize, perform PCA
  pcaRes <- doPCA(data, splitBy, splitLevels, vars)

  #Do CE, angle and chroma preperation steps
  CEInfo <- NULL
  angleInfo <- NULL
  chromaInfo <- NULL

  if (doCE) CEInfo <- doCE(pcaRes, data, B)
  if (doAngle) angleInfo <- doAngle(pcaRes)
  if (doChroma) chromaInfo <- doChroma(pcaRes)

  out <- list(pcaRes = pcaRes, CEInfo = CEInfo, angleInfo = angleInfo,
              chromaInfo = chromaInfo, data = data, splitBy = splitBy,
              vars = vars, B = B)
  class(out) <- "PCADSC"
  out
}




################Not exported below##################################################

#Standardize each variable in a dataset (subtract mean, divide by SD)
#' @importFrom stats sd
stdData <- function(data) {
  as.data.frame(lapply(data, function(x) (x - mean(x))/sd(x)))
}


#split data, standardize, perform PCA
#' @importFrom stats prcomp
doPCA <- function(data, splitBy, splitLevels, vars, doBoth = TRUE) {
  #Initialize "both" values at NULL
  dataBoth <- loadBoth <- eigenBoth <- NULL

  #split and standardize data
  data1 <- stdData(data[data[, splitBy]==splitLevels[1], vars])
  data2 <- stdData(data[data[, splitBy]==splitLevels[2], vars])
  if (doBoth) dataBoth <- stdData(data[, vars])

  #do PCA
  #Note: No centering as we already did that when standardizing
  pca1 <- prcomp(data1, center = FALSE)
  pca2 <- prcomp(data2, center = FALSE)
  if (doBoth) pcaBoth <- prcomp(dataBoth, center = FALSE)
  load1 <- pca1$rotation
  load2 <- pca2$rotation
  if (doBoth) loadBoth <- pcaBoth$rotation
  eigen1 <- pca1$sdev^2
  eigen2 <- pca2$sdev^2
  if (doBoth) eigenBoth <- pcaBoth$sdev^2

  out <- list(load1 = load1, load2 = load2, loadBoth = loadBoth,
              eigen1 = eigen1, eigen2 = eigen2, eigenBoth = eigenBoth,
              d = ncol(data1), n1 = nrow(data1), n2 = nrow(data2),
              nBoth = nrow(data), splitBy = splitBy,
              splitLevels = splitLevels, vars = vars)
  class(out) <- "pcaRes"
  out
}
