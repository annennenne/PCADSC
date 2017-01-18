##Author: Bo Markussen##

# cumulated differences
cumdiff <- function(data1,data2,standardize=TRUE,make.plot=FALSE) {
  # standardize
  data1 <- MuMIn::stdize(as.matrix(data1),scale=standardize)
  data2 <- MuMIn::stdize(as.matrix(data2),scale=standardize)
  data12 <- MuMIn::stdize(as.matrix(rbind(data1,data2)),scale=standardize)

  # eigen decomposition
  n1 <- nrow(data1)
  n2 <- nrow(data2)
  n12 <- nrow(data12)
  d12 <- ncol(data12)
  eigen1 <- eigen(1/n1*t(data1)%*%data1) #n1-1 here?
  eigen2 <- eigen(1/n2*t(data2)%*%data2) #n2-1 here?
  eigen12 <- eigen(1/n12*t(data12)%*%data12)

  # cumulated differences:
  # 1) start from smallest eigenvalues
  # 2) remove negative signs; the non-determancy wrt. sign presumably makes signs quite arbitrary
  # 3) cumulate aross eigenvectors within variables
  # 4) code assumes that n12 >= d12. This also makes mathematically most sense.

  # consequtive d12 values come from same variable
  y1 <- abs(t(eigen1$vectors[,d12:1])*sqrt(eigen1$values[d12:1]))
  y2 <- abs(t(eigen2$vectors[,d12:1])*sqrt(eigen2$values[d12:1]))
  x12 <- t(eigen12$vectors[,d12:1]^2)*eigen12$values[d12:1]

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
cumdiff(data1,data2)

# simulate data
x <- matrix(rnorm(20*100),20,100)
basis <- eigen(x%*%t(x)/100)$vectors
x <- matrix(rnorm(20*100),20,100)
basis2 <- eigen(x%*%t(x)/100)$vectors

plot(c(0,1),c(-3,3),type="n")
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

