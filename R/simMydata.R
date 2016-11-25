#Simulate data, divide it into two groups and bias
# one of the groups
#lavaObj: lvm-object from which to simulate
#nObs: Total number of observations
#n1: Number of observations in first group
#n2: Number of observations in second group
#affectDegree: percentage of variables to be biased
# among the variables in the second group
#biasSize: mean of biasing error terms
#varSize: variance of biasing error terms
#nClusters: Number of clusters in which bias is inflicted.
# Variables in the same cluster reseve the same
# biasing error term

#' import lava
#' importFrom stats simulate rnorm var
simMyData <- function(lavaObj=NULL, nObs=NULL, n1=NULL,
                      n2=NULL, biasSize=NULL, affectDegree=NULL,
                      varSize=NULL, nClusters=NULL) {
  if (is.null(nObs) & !is.null(n1) & !is.null(n2)) {
    nObs <- n1 + n2
  }
  if (!is.null(nObs) & is.null(n2)) {
    n2 <- nObs - n1 #obs: check if pos.
  } #more n-checking here

  if (is.null(varSize)) varSize <- 0
  if (is.null(biasSize)) biasSize <- 0
  if (is.null(affectDegree)) affectDegree <- 0

  if (is.null(lavaObj)) {
    m <- lvm() #m8d
    regression(m) <- c(SICK_TOTAL, Q36n, Q30n) ~ HEALTH
    regression(m) <- STRESS ~ Q05 + Q08.yes + Q03.temp
    regression(m) <- c(DEMANDS, CONTENTS, RELATIONS, SITUATION) ~ SEG_KOEN +
      SEG_KOEN2 + VIDEN_GRAD.high + VIDEN_GRAD.medium +
      ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
    regression(m) <- c(S1.1, S1.2, S1.3, S1.4, S1.5) ~ DEMANDS
    regression(m) <- c(S2.1, S2.2, S2.3, S2.4, S2.5, S2.6, S2.7, S2.8) ~ CONTENTS
    regression(m) <- c(S3.1, S3.2, S3.3, S3.4, S3.5, S3.7, S3.8, S3.9, S3.10, S3.11, S3.12) ~ RELATIONS
    regression(m) <- c(S5.1, S5.2, S5.3, S5.4, S5.5) ~ SITUATION
    regression(m) <- HEALTH ~ DEMANDS + CONTENTS + RELATIONS + SITUATION +
      SEG_KOEN + SEG_KOEN2
    regression(m) <- HEALTH ~ Q42.daily + Q42.sometimes + Q42.former + BMI_CAT.obese +
      ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55 +
      FAM_STATUS.cp + FAM_STATUS.ncp + FAM_STATUS.cnp
    regression(m) <- c(MDI, STRESS) ~ HEALTH
    regression(m) <- ANS_AAR ~ ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55
    latent(m) <- ~ HEALTH + DEMANDS + CONTENTS + RELATIONS + SITUATION
    regression(m) <- STRESS ~ ANS_AAR

  } else m <- lavaObj
  if (is.null(latent(m))) latent(m) <- "NULLLATVARDROPME"
  data <- simulate(m, nObs)
  data <- data[, which(!(names(data) %in% latent(m)))]
  splitInd <- sample(nObs, n2, replace=F)
  data1 <- data[-splitInd, ]
  data2 <- data[splitInd, ]
  if (affectDegree > 0) {
    nVar <- ncol(data2)
    nBias <- floor(nVar*affectDegree)
    affVar <- sample(nVar, nBias, replace=F)
    groupSize <- floor(length(affVar)/nClusters)
    for (i in 1:nClusters) {  #nBias) {
      if (i == nClusters) {
        thisGroup <- ((i-1)*groupSize + 1):length(affVar)
      } else thisGroup <- ((i-1)*groupSize + 1):(i*groupSize)
      theseVars <- data2[, thisGroup]
      sign <- sample(c(-1, 1), 1)
      bias <- sign*rnorm(n2, mean = biasSize, sd = sqrt(varSize))
      addBias <- function(x) x + bias
      data2[, thisGroup] <- sapply(theseVars, addBias)
    }
  }
  data <- rbind(data1, data2)
  data$group <- c(rep("Group 1", n1), rep("Group 2", n2))
  data
}

