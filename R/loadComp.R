#Compute standardized PCA loadings and
# cummulated variance contributions from
# a dataset.
#varCO option is not yet implemented
#vars: variable names that should be included

#' @importFrom stats na.omit princomp
#' @importFrom reshape2 melt
loadComp <- function(data, vars = NULL, varCO=NULL) {
  if (is.null(varCO)) {
    varCO <- 1
  }

  if (is.null(vars)) {
    vars <- names(data)
  }

  n <- length(vars)
  p <- stats::princomp(sapply(stats::na.omit(data[, vars]), as.numeric))
  px <- round(matrix(c(p$loadings), n,
                     dimnames=list(vars, 1:n)), 4)

  for (i in 1:n) { #standardize
    px[, i] <- abs(px[, i]/sum(abs(px[, i])))
  }

  #cummulative variance contributions and
  #principal component variance constribution
  pcvc <- p$sdev^2
  pcvc <- pcvc/sum(pcvc)
  cpcvc <- cvc <- cumsum(pcvc)
  cvc <- paste(round(cvc*100, 2), "%")
  varpct <- paste(round(pcvc*100, 2), "%")

  #combine
  pxx <- reshape2::melt(px)
  pxx$varpct_raw <- pxx$cvc_raw <- pxx$cvc <- pxx$varpct <- rep(NA, nrow(pxx))
  for (i in 1:n) {
    thisComp <- which(pxx$Var2==i)
    pxx$cvc[thisComp[1]] <- cvc[i]
    pxx$varpct[thisComp[1]] <- varpct[i]
    pxx$cvc_raw[thisComp] <- cpcvc[i]
    pxx$varpct_raw[thisComp] <- pcvc[i]
  }

  colnames(pxx) <- c("var", "comp", "loading", "varpct", "cvc", "cvc_raw", "varpct_raw")
  list(loadings=pxx, nObs=p$n.obs)
}
