#' @title Cumulative eigenvalue plot
#'
#' @description Compares eigenvalues by cumulative Bland-Altman type plots. Kolmogorov-Smirnov and Cramer-von Mises tests evaluated by permutation tests
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
#' @param make.plot Should a plot be generated?
#'
#' @param B Number of simulations
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
#' cumeigen(iris, "group",var=setdiff(names(iris), c("group", "Species")))
#'
#' @importFrom graphics polygon matlines lines
#' @export
cumeigen <- function(data,splitBy,var=NULL,B=1000,make.plot=TRUE) {
  #define var
  if (is.null(var)) var <- setdiff(names(data), splitBy)

  #TO DO:
  #-  check if any variables are essentially empty. This will cause an error
  #   as we then divide by zero when standardizing
  #-  Make sure it deals with factor splitBy varibales correctly

  #If data is tibble, data.table or matrix, convert it to matrix
  #If data is neither, throw error
  if (any(class(data) %in% c("data.table", "tbl", "tbl_df", "matrix"))) {
    data <- as.data.frame(data)
    message("Note: The data was converted to a data.frame in order for cumeigen to run.")
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

  # my stdData
  stddata <- function(data) {
    as.data.frame(lapply(data, function(x) (x - mean(x))/sd(x)))
  }

  # joint x values
  n <- nrow(data)
  d <- length(var)
  x <- c(0,cumsum(eigen(1/(n-1)*t(as.matrix(stddata(data[,var])))%*%as.matrix(stddata(data[,var])),only.values=TRUE)$values))

  #split and standardize data
  data1 <- as.matrix(stddata(data[data[, splitBy]==splitLevels[1], var]))
  data2 <- as.matrix(stddata(data[data[, splitBy]==splitLevels[2], var]))
  n1 <- nrow(data1)
  n2 <- nrow(data2)

  # observed y values
  y1 <- eigen(1/(n1-1)*t(data1)%*%data1,only.values=TRUE)$values
  y2 <- eigen(1/(n2-1)*t(data2)%*%data2,only.values=TRUE)$values
  y.obs   <- c(0,cumsum(y1-y2))
  KS.obs  <- max(abs(y.obs))
  CvM.obs <- sum(x*(y.obs^2))

  # simulate partitions
  y.sim   <- matrix(0,d+1,B)
  KS.sim  <- rep(0,B)
  CvM.sim <- rep(0,B)
  for (i in 1:B) {
    ii <- sample(1:n,n1)
    data1 <- as.matrix(stddata(data[ii,var]))
    data2 <- as.matrix(stddata(data[-ii,var]))
    y1 <- eigen(1/(n1-1)*t(data1)%*%data1,only.values=TRUE)$values
    y2 <- eigen(1/(n2-1)*t(data2)%*%data2,only.values=TRUE)$values
    y.sim[,i]  <- c(0,cumsum(y1-y2))
    KS.sim[i]  <- max(abs(y.sim[,i]))
    CvM.sim[i] <- sum(x*(y.sim[,i]^2))
  }

  # p-values
  KS.pvalue <- mean(KS.sim >= KS.obs)
  CvM.pvalue <- mean(CvM.sim >= CvM.obs)

  # make plot
  if (make.plot) {
    y.min <- apply(y.sim,1,quantile,probs=0.025)
    y.max <- apply(y.sim,1,quantile,probs=0.975)
    plot(c(x[1],x[d+1]),c(1.01*min(c(y.obs,as.vector(y.sim))),1.01*max(c(y.obs,as.vector(y.sim)))),type="n",
         xlab="Cumulated joint eigenvalues",ylab="Cumulated difference of eigenvalues")
    polygon(c(x,rev(x)),c(y.min,rev(y.max)),col="aliceblue",border=NA)
    matlines(x,y.sim[,1:min(20,B)],col="gray")
    lines(x,y.obs,lwd=2)
    legend("topleft",paste(c("KS: p=","CvM: p="),round(c(KS.pvalue,CvM.pvalue),3),sep=""))
  }

  # return
  return(list(KS.pvalue=KS.pvalue,CvM.pvalue=CvM.pvalue))
}
