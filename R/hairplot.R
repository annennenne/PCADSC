#' Hairplot
#'
#' @description ????
#'
#' @param data1 ???
#' @param data2 ???
#' @param standardize ???
#' @param make.plot ????
#' @param arrow.len ???
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
#' #Make hairplot
#'  hairplot(iris[iris$group == "setosa", 1:4], iris[iris$group == "non-setosa", 1:4])
#'
#' @importFrom graphics axis arrows
#' @export
hairplot <- function(data1,data2,standardize=TRUE,make.plot=TRUE,arrow.len=0.05) {
  # stadardize
  data1 <- MuMIn::stdize(as.matrix(data1),scale=standardize)
  data2 <- MuMIn::stdize(as.matrix(data2),scale=standardize)

  # eigen decomposition
  d <-  ncol(data1)
  n1 <- nrow(data1)
  n2 <- nrow(data2)
  eigen1 <- eigen(1/n1*t(data1)%*%data1)
  eigen2 <- eigen(1/n2*t(data2)%*%data2)

  # find angles
  angles <- matrix(0,d,d)
  for (i in 1:d) for (j in 1:d) angles[i,j] <- asin(max(0,min(1,abs(sum(eigen1$vectors[,i]*eigen2$vectors[,j])))))

  # make plot
  if (make.plot) {
    plot(c(0.5,d+0.5),c(0.5,d+0.5),type="n",xlab="dataset 1: PC's",ylab="dataset 2: PC's",axes=FALSE)
    axis(1,1:d)
    axis(2,1:d)
    pi <- 2*asin(1)
    for (i in 1:d) for (j in 1:d) {
      len <- sqrt(eigen1$values[i]/eigen1$values[1]*eigen2$values[j]/eigen2$values[1])
      arrows(i,j,i+len*cos(angles[i,j]/2),j+len*sin(angles[i,j]/2),length=arrow.len)
    }
  }

  # return
  return(angles)
}


