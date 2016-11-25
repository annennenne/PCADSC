#Compute standardized PCA loadings and
# cummulated variance contributions from
# a dataset.
#varCO option is not yet implemented
#vars: variable names that should be included

#' importFrom stats na.omit princomp
loadComp <- function(data, vars, varCO=NULL) {
  if (is.null(varCO)) {
    varCO <- 1
  }
  n <- length(vars)
  p <- princomp(sapply(na.omit(data[, vars]), as.numeric))
  px <- round(matrix(c(p$loadings), n,
                     dimnames=list(vars, 1:n)), 4)

  for (i in 1:n) { #standardize
    px[, i] <- abs(px[, i]/sum(abs(px[, i])))
  }

  #cummulative variance contributions and
  #principal component variance constribution
  pcvc <- p$sdev^2
  pcvc <- pcvc/sum(pcvc)
  cpcvc <- cumsum(pcvc)
  cvc <- cumsum(pcvc)
  cvc <- paste(round(cvc*100, 2), "%")

  #combine
  pxx <- melt(px)
  pxx$cvc <- pxx$pcvc <- rep(NA, nrow(pxx))
  for (i in 1:n) {
    thisComp <- which(pxx$Var2==i)
    pxx$cvc[thisComp[1]] <- cvc[i]
    pxx$pcvc[thisComp] <- pcvc[i]
  }

  colnames(pxx) <- c("var", "comp", "loading", "pcvc", "cvc")
  list(loadings=pxx, nObs=p$n.obs)
}
