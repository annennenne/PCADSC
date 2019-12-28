##Author: Bo Markussen##
#' Cumulative eigenvalues
#'
#' @description Compares eigenvalues by cumulative Bland-Altman type plots. Kolmogorov-Smirnov and Cramer-von Mises tests evaluated by permutation tests
#'
#' @param data1 First dataset
#' @param data2 Second dataset
#' @param standardize Should variables be standardized?
#' @param make.plot Should a plot be generated?
#' @param B Number of simulations
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
#' #Make cumulative eigenvalues
#'  cumeigen(iris[iris$group == "setosa", 1:4], iris[iris$group == "non-setosa", 1:4])
#'
#' @export
# cumulated differences
cumeigen <- function(data1,data2,standardize=TRUE,make.plot=FALSE,B=1000) {
  # standardize
  data1 <- MuMIn::stdize(as.matrix(data1),scale=standardize)
  data2 <- MuMIn::stdize(as.matrix(data2),scale=standardize)
  data12 <- MuMIn::stdize(as.matrix(rbind(data1,data2)),scale=standardize)

  # eigen decomposition
  n1 <- nrow(data1)
  n2 <- nrow(data2)
  n12 <- nrow(data12)
  d12 <- ncol(data12)
  eigen1 <- eigen(1/(n1-1)*t(data1)%*%data1)
  eigen2 <- eigen(1/(n2-1)*t(data2)%*%data2)
  eigen12 <- eigen(1/(n12-1)*t(data12)%*%data12)

  # cumulated differences:
  # 1) start from smallest eigenvalues
  # 4) code assumes that n12 >= d12. This also makes mathematically most sense.

  # consequtive d12 values come from same variable
  y1 <- eigen1$values[d12:1]
  y2 <- eigen2$values[d12:1]
  x12 <- eigen12$values[d12:1]

  # Statistics
  KS <- max(abs(cumsum(y1-y2)))
  CvM <- sum(x12*(cumsum(y1-y2)^2))
  # make plot
  if (make.plot) plot(cumsum(as.vector(x12)),cumsum(as.vector(y1-y2)),type="b")

  # return
  return(list(xx=cumsum(as.vector(x12)),diffs=cumsum(as.vector(y1-y2))))
}

# test
data("iris")
str(iris)
data1 <- subset(iris,Species=="setosa")[,1:4]
data2 <- subset(iris,Species=="versicolor")[,1:4]
cumdiff(data1,data2,make.plot = TRUE)

# simulate data
x <- matrix(rnorm(20*100),20,100)
basis <- eigen(x%*%t(x)/100)$vectors
x <- matrix(rnorm(20*100),20,100)
basis2 <- eigen(x%*%t(x)/100)$vectors

plot(c(0,20),c(-1,1),type="n")
for (reps in 1:100) {
  x1 <- matrix(rnorm(100*20),100,20)%*%basis
  x2 <- matrix(rnorm(100*20),100,20)%*%basis
  res <- cumdiff(x1,x2)
  lines(res$xx[1:20],res$diffs[1:20],type="l",col="lightgray",lty=2)
}
x1 <- matrix(rnorm(100*20),100,20)%*%basis
x2 <- matrix(rnorm(100*20),100,20)%*%basis2
res <- cumdiff(x1,x2)
lines(res$xx[1:20],res$diffs[1:20],type="l",col=2,lwd=2)

