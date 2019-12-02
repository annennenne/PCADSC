#' @title QQ-plot for anglePlot
#'
#' @description Quantile-quantile plot for the observed anglePlot p-values against
#' the null hypothesis of equal structure. Size of points display the length of the
#' arrows in the anglePlot. Also shown is pointwise 95pct confidence interval under
#' the null together with 20 random samples under the null. Finally, the p-value for
#' one-sided Kolmogorov-Smirnov test on the logaritmic scale is given.
#'
#' @param x A \code{PCADSC} object with angle information simulated under the null.
#'
#' @seealso \code{\link{anglePlot}}, \code{\link{PCADSC}}
#'
#' @importFrom ggplot2 ggplot geom_polygon geom_line geom_point
#' scale_x_log10 scale_y_log10 labs ggtitle theme_light
#' @export
QQanglePlot <- function(x) {
  # grab parameter values from object
  d <- x$pcaRes$d
  B <- dim(x$angleInfo$angles.sim)[3]

  # find ranks, changed them along the diagonal, and recode as matrix
  pvalues <- apply(x$angleInfo$angles.sim,c(1,2),rank)
  for (i in 1:d) pvalues[,i,i] <- B+1-pvalues[,i,i]
  dim(pvalues) <- c(B,d*d)

  # sort within simulations, renormalize to p-values, and find quantiles
  pvalues          <- apply(pvalues,1,sort)/B
  pvalues.quantile <- apply(pvalues,1,function(y){quantile(y,probs = c(0.025,0.5,0.975))})

  # take out 20 (or B when B<20) random samples
  pvalues.random <- data.frame(pvalue=c(pvalues[,sample(1:B,min(B,20))]),
                               median=rep(pvalues.quantile[2,],times=min(B,20)),
                               group=rep(1:min(B,20),each=d*d))

  # data frame with point information
  # we define importance as arrow lengths
  point.df <- data.frame(importance=sqrt((x$angleInfo$aF$xend-x$angleInfo$aF$x)^2+(x$angleInfo$aF$yend-x$angleInfo$aF$y)^2),
                         type=rep(c("A on B","B on A"),times=d*d),
                         pvalue=rep(pmax(1/(2*B),x$angleInfo$pvalue),each=2))
  point.df <- point.df[c(-1,0)+2*rep(order(x$angleInfo$pvalue),each=2),]
  point.df <- cbind(point.df,data.frame(median=rep(pvalues.quantile[2,],each=2)))

  # One-sided Kolmogorov-Smirnov test
  ks.obs    <- max(log10(point.df$median/point.df$pvalue))
  ks.pvalue <- mean(apply(pvalues,2,function(y){max(log10(pvalues.quantile[2,]/y))}) >= ks.obs)

  # plot observed p-values against median of simulated p-values
  ggplot() +
    geom_polygon(aes(x=c(1/pvalues.quantile[2,],rev(1/pvalues.quantile[2,])),
                     y=c(1/pvalues.quantile[1,],rev(1/pvalues.quantile[3,]))),
                 fill = "aliceblue") +
    geom_line(aes(x=1/median,y=1/pvalue,group=group),data=pvalues.random,
              col="gray") +
    geom_point(aes(x=1/median,y=1/pvalue,size=importance,col=type),data=point.df,alpha=1,shape=16) +
    scale_x_log10() + scale_y_log10() +
    labs(x="median of inverse p-values under the null",y="observed inverse p-values") +
    ggtitle(paste0("One-sided Kolmogorov-Smirnov test: p=",ks.pvalue)) +
    theme_light()
}
