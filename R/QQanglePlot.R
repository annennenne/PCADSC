#' @title QQ-plot for anglePlot
#'
#' @description This is a first attempt to make a quantile-quantile like plot for the
#' angles in the anglePlot. The length of the arrows will be visualized as size of the
#' points showing the observed p-values.
#'
#' @param x A \code{PCADSC} object with angle information simulated under the null.
#'
#' @seealso \code{\link{anglePlot}}, \code{\link{PCADSC}}
#'
#' @export
QQanglePlot <- function(x) {
  # grab parameter values from object
  d <- x$pcaRes$d
  B <- dim(x$angleInfo$angles.sim)[3]

  # find arrow lengths
  len <- apply(matrix(sqrt((x$angleInfo$aF$xend-x$angleInfo$aF$x)^2+
                           (x$angleInfo$aF$yend-x$angleInfo$aF$y)^2),
                      2,d*d),2,max)

  # find ranks, changed them along the diagonal, and recode as matrix
  pvalues <- apply(x$angleInfo$angles.sim,c(1,2),rank)
  for (i in 1:d) pvalues[,i,i] <- B+1-pvalues[,i,i]
  dim(pvalues) <- c(B,d*d)

  # sort within simulations, and renormalize to p-values
  pvalues <- apply(pvalues,1,sort)/B

  # take out 20 (or B when B<20) random samples
  pvalues.random <- data.frame(pvalue=c(pvalues[,sample(1:B,min(B,20))]),
                               group=rep(1:min(B,20),each=d*d))

  # plot observed p-values against median of simulated p-values
  pvalues.sim <- apply(pvalues,1,function(y){quantile(y,probs = c(0.025,0.5,0.975))})
  pvalues.obs <- pmax(1/(2*B),sort(c(x$angleInfo$pvalue)))
  ggplot() +
    geom_polygon(aes(x=c(1/pvalues.sim[2,],rev(1/pvalues.sim[2,])),
                     y=c(1/pvalues.sim[1,],rev(1/pvalues.sim[3,]))),
                 fill = "aliceblue") +
    geom_line(aes(x=rep(1/pvalues.sim[2,],min(B,20)),y=1/pvalue,group=group),data=pvalues.random,
              col="gray") +
    geom_point(aes(x=1/pvalues.sim[2,],y=1/pvalues.obs,size=len[order(c(x$angleInfo$pvalue))])) +
    scale_x_log10() + scale_y_log10() +
    labs(x="median of inverse p-values under the null",y="observed inverse p-values",
         size="importance") +
    theme_light()
}
