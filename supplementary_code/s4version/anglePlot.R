#' @title Angle plot
#'
#' @description Compares eigenvalues and eigenvectors form two datasets. Kolmogorov-Smirnov and Cramer-von Mises tests evaluated by permutation tests might be implemented later.
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
#' @param arrow.len Length of arrow on plot
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
#' anglePlot(iris, "group",var=setdiff(names(iris), c("group", "Species")))
#'
#' @importFrom graphics axis arrows
#' @importFrom ggplot2 ggplot aes_string scale_x_continuous scale_y_continuous
#' theme_bw theme element_blank xlab ylab geom_segment unit scale_color_manual
#' @importFrom grid arrow
#' @export
anglePlot <- function(data,splitBy,var=NULL,B=1000,make.plot=TRUE,arrow.len=0.05) {
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

  #split and standardize data
  data1 <- as.matrix(stddata(data[data[, splitBy]==splitLevels[1], var]))
  data2 <- as.matrix(stddata(data[data[, splitBy]==splitLevels[2], var]))
  n1 <- nrow(data1)
  n2 <- nrow(data2)
  d  <- ncol(data1)

  # eigen decomposition
  eigen1 <- eigen(1/n1*t(as.matrix(data1))%*%as.matrix(data1))
  eigen2 <- eigen(1/n2*t(as.matrix(data2))%*%as.matrix(data2))

  # find angles
  angles <- matrix(0,d,d)
  for (i in 1:d) for (j in 1:d) angles[i,j] <- asin(max(0,min(1,abs(sum(eigen1$vectors[,i]*
                                                                          eigen2$vectors[,j])))))
  # Return angles if no plot is required
  if (!make.plot) {
    return(angles)
  }

  #Calculate vector lengths
  max.eigen <- max(c(eigen1$values[1],eigen2$values[1]))
  len1 <- len2 <- matrix(NA, d, d)

  for (i in 1:d) for (j in 1:d) {
    len1[i,j] <- sqrt(eigen1$values[i]/max.eigen)*abs(sum(eigen1$vectors[,i]*
                                                            eigen2$vectors[,j]))
    len2[i,j] <- sqrt(eigen2$values[j]/max.eigen)*abs(sum(eigen1$vectors[,i]*
                                                            eigen2$vectors[,j]))
  }

  #Plot results
  pF <- data.frame(x = rep(1:d, 2*d),
                   y = rep(rep(1:d, each = d),2),
                   xend = c(rep(1:d, d) + c(len1)*cos((pi-c(angles))/2),
                            rep(1:d, d) + c(len2)*cos(c(angles)/2)),
                   yend = c(rep(1:d, each = d) + c(len1)*sin((pi-c(angles))/2),
                            rep(1:d, each = d) + c(len2)*sin(c(angles)/2)),
                   type = rep(c("1st", "2nd"), each = d*d))

  ggplot(pF[, , drop = F], aes_string(x = "x", y = "y", col = "type",
                                      xend = "xend", yend = "yend")) +
    scale_x_continuous(limits = c(0.5,d+0.5), breaks = 1:d) +
    scale_y_continuous(limits = c(0.5,d+0.5), breaks = 1:d) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab("Dataset 1: PCs") +
    ylab("Dataset 2: PCs") +
    geom_segment(arrow = arrow(length = unit(arrow.len, "inch"))) +
    scale_color_manual(values = c("blue", "red"), guide = FALSE)


  # make plot
  # if (make.plot) {
  #    plot(c(0.5,d+0.5),c(0.5,d+0.5),type="n",xlab="dataset 1: PC's",ylab="dataset 2: PC's",axes=FALSE)
  #    axis(1,1:d)
  #    axis(2,1:d)
  #    max.eigen <- max(c(eigen1$values[1],eigen2$values[1]))
  #    for (i in 1:d) for (j in 1:d) {
  #      # plot PC1's in coordinate system of PC2
  #      len <- sqrt(eigen1$values[i]/max.eigen)*abs(sum(eigen1$vectors[,i]*eigen2$vectors[,j]))
  #      arrows(i,j,i+len*cos((pi-angles[i,j])/2),j+len*sin((pi-angles[i,j])/2),length=arrow.len,col="blue")
  #    }
  #    for (i in 1:d) for (j in 1:d) {
  #      # plot PC2's in coordinate system of PC1
  #      len <- sqrt(eigen2$values[j]/max.eigen)*abs(sum(eigen1$vectors[,i]*eigen2$vectors[,j]))
  #      arrows(i,j,i+len*cos(angles[i,j]/2),j+len*sin(angles[i,j]/2),length=arrow.len,col="red")
  #    }
  #  }


}
