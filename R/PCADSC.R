#' @title Compute the elements used for PCADSC
#'
#' @description Principal Component Analysis-based Data Structure Comparison tools that
#' prepare a dataset for various diagnostic plots for comparing data structures. More
#' specifically, \code{PCADSC} performs PCA on two subsets of a dataset in order to
#' compare the structures of these datasets, e.g. to assess whether they can be analyzed pooled
#' or not. The results of the PCAs are then manipulated in various
#' ways and stored for easy plotting using the three PCADSC plotting tools, the \code{\link{CEPlot}},
#' the \code{\link{anglePlot}} and the \code{\link{chromaPlot}}.
#'
#' @details PCADSC presents a suite of non-parametric, visual tools for comparing the structures of
#' two subsets of a dataset. These tools are all based on PCA (principal component analysis) and
#' thus they can be interpreted as comparisons of the covariance matrices of the two (sub)datasets.
#' \code{PCADSC} performs PCA using singular value decomposition for increased numerical precision.
#' Before performing PCA on the full dataset and the two subsets, all variables within each such
#' dataset are standardized.
#'
#' @return An object of class \code{PCADSC}, which is a named list with the following entries:
#' \describe{
#'   \item{pcaRes}{The results of the PCAs performed on the first subset, the second subset and
#'   the full subset and also information about the data splitting.}
#'   \item{CEInfo}{The information needed for making a cumulative eigenvalue plot
#'   (see \code{\link{CEPlot}}).}
#'   \item{angleInfo}{The information needed for making an angle plot
#'   (see \code{\link{anglePlot}}).}
#'   \item{chromaInfo}{The information needed for making a chroma plot
#'   (see \code{\link{chromaPlot}}).}
#'   \item{data}{The original (full) dataset.}
#'   \item{splitBy}{The name of the variable that splits the dataset in two.}
#'   \item{vars}{The names of the variables in the dataset that should be used for PCA.}
#'   \item{B}{The number of resamplings performed for the \code{CEInfo}.}
#'   }
#'
#' @param data A dataset, either a \code{data.frame} or a \code{matrix} with variables
#' in columns and observations in rows. Note that \code{\link[tibble]{tibble}}s and
#' \code{\link[data.table]{data.table}}s are accepted as input, but they are instantly
#' converted to \code{\link{data.frame}}s. Future releases might include specific implementation
#' for these data representations.
#'
#' @param splitBy The name of a grouping variable with two levels defining the two groups within the
#' dataset whose data structures we wish to compare.
#'
#' @param vars The variable names in \code{data} to include in the PCADSC. If \code{NULL}
#' (the default), all variables except for \code{splitBy} are used.
#'
#' @param doCE Logical. Should the cumulative eigenvalue plot information be computed?
#'
#' @param doAngle Logical. Should the angle plot information be computed?
#'
#' @param doChroma Logical. Should the chroma plot information be computed?
#'
#' @param B A positive integer. The number of resampling steps performed in the cumulative
#' eigenvalue step, if relevant.
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
#' #Make a full PCADSC object, splitting the data by "group"
#' irisPCADSC <- PCADSC(iris, "group")
#'
#' #The three plotting functions can now be called on irisPCADSC:
#' CEPlot(irisPCADSC)
#' anglePlot(irisPCADSC)
#' chromaPlot(irisPCADSC)
#'
#' #Make a partial PCADSC object with no angle plot information and add
#' #angle plot information afterwards:
#' irisPCADSC2 <- PCADSC(iris, "group", doAngle = FALSE)
#' irisPCADSC2 <- doAngle(irisPCADSC)
#' }
#'
#' #Make a partial PCADSC obejct with no plotting (angle/CE/chroma)
#' #information:
#' irisPCADSC_minimal <- PCADSC(iris, "group", doAngle = FALSE,
#'   doCE = FALSE, doChroma = FALSE)
#'
#' @seealso \code{\link{doCE}}, \code{\link{doAngle}}, \code{\link{doChroma}},
#' \code{\link{CEPlot}}, \code{\link{anglePlot}}, \code{\link{chromaPlot}}
#'
#' @export
PCADSC <- function(data, splitBy, vars=NULL, doCE = TRUE,
                   doAngle = TRUE, doChroma = TRUE,
                   B = 10000) {
  #define var
  if (is.null(vars)) vars <- setdiff(names(data), splitBy)

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
  #check if all variables are non-missing
  isNA <- sapply(data[, c(vars, splitBy)], "is.na")
  if (any(isNA)) {
    stop(paste("All variables must be non-missing for PCA decomposition",
               "to be meaningful. The following variables contained missing",
               "information:", paste(names(isNA), collapse = ", ")))
  }
  #check if all variables are non-empty
  isEmpty <- sapply(data[, vars], function(x) length(unique(x)) <= 1)
  if (any(isEmpty)) {
    stop(paste("All variables must take more than one unique value for PCA decomposition",
               "to be meaningful. The following variables contained only a single unique",
               "value:", paste(names(isEmpty), collapse = ", ")))
  }



  #convert splitBy into a factor variable
  data[, splitBy] <- factor(data[, splitBy])

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
  if (doAngle) angleInfo <- doAngle(pcaRes, data, B)                  # CHANGED to include simulations
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
