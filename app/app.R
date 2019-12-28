library(shiny)
library(ggplot2)
thisTheme <- theme_bw() + theme(text = element_text(size=12))
theme_set(thisTheme)
library(reshape2)
library(lava)

#Investigate overall response distributions stratified by 
#modifications by use of PCA
pcaLoad <- function(data, vars, varCO=NULL) {
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

#Split data, call pca load, return data.frame ready
#for plotting. TO DO: should return an s3 object instead
#of just a list. Implement covariance cutoff (covCO) option.
pcaObjGen <- function(data, var, splitBy, covCO=NULL) {
   splitLevels <- unique(data[, splitBy])
  data1 <- data[data[, splitBy]==splitLevels[1], ]
  data2 <- data[data[, splitBy]==splitLevels[2], ]
  res1 <- pcaLoad(data1, var)
  res2 <- pcaLoad(data2, var)
  load1 <- res1$loadings
  load2 <- res2$loadings
  n1 <- nrow(load1) 
  n2 <- nrow(load2)
  nObs1 <- res1$nObs
  nObs2 <- res2$nObs
  pcaFrame <- as.data.frame(rbind(load1, load2))
  pcaFrame$group <- c(rep(as.character(splitLevels[1]), n1),
                      rep(as.character(splitLevels[2]), n2))

  list(pcaFrame=pcaFrame, splitBy=splitBy, splitLevels=splitLevels, 
       varNames=var, n1=n1, n2=n2, nObs1=nObs1, nObs2=nObs2)  
}

pcaPlot <- function(pcaObj, varLabels=NULL, covCO=NULL,
                    splitLabels=NULL) {
  splitLevels <- pcaObj$splitLevels
  nCat1 <- pcaObj$nObs1
  nCat2 <- pcaObj$nObs2
  splitBy <- pcaObj$splitBy
  pcaFrame <- pcaObj$pcaFrame
  
  if (is.null(varLabels)) {
    varLabels <- pcaObj$varNames
  }
  if (is.null(splitLabels)) {
    splitLabels <- splitLevels
  } else {
    sl1 <- splitLabels[[which(names(splitLabels)==splitLevels[1])]]
    sl2 <- splitLabels[[which(names(splitLabels)==splitLevels[2])]]
    splitLabels <- c(sl1, sl2)
  }
  
  facetLabels <- c(paste(splitLabels[1], ", n = ", nCat1, sep=""),
                   paste(splitLabels[2], ", n = ", nCat2, sep=""))
  names(facetLabels) <- splitLevels
  
  ggplot(pcaFrame, aes(x=comp, y=loading, fill=var)) +
    geom_bar(stat="identity") +
    coord_flip() +
    scale_x_reverse(breaks=c(1, seq(10, 50, 10))) +
    scale_y_continuous(limits=c(0, 1.4), 
                       breaks=c(0, 0.25, 0.5, 0.75, 1)) +
    geom_text(aes(label=cvc), y=1.2, cex=4, na.rm=T) +
    xlab("Principal component") +
    ylab("Standardized loading") +
    theme(legend.position="bottom") +
    facet_wrap(~ group, ncol=2,
               labeller=as_labeller(facetLabels)) + 
    scale_fill_discrete(name=NULL, labels=varLabels)
}

simMyData <- function(lavaObj=NULL, nObs=NULL, n1=NULL,
                      n2=NULL, biasSize=NULL, affectDegree=NULL,
                      varSize=NULL, nClusters=NULL) {
  if (is.null(nObs) & !is.null(n1) & !is.null(n2)) {
    nObs <- n1 + n2
  }
  if (!is.null(nObs) & is.null(n2)) {
    n2 <- nObs - n1 #obs: check if pos.
  } #add more n-checking here
  
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
  if (affectDegree > 0) {
    splitInd <- sample(nObs, n2, replace=F)
    data1 <- data[-splitInd, ]
    data2 <- data[splitInd, ]
    nVar <- ncol(data2)
    nBias <- floor(nVar*affectDegree)
    affVar <- sample(nVar, nBias, replace=F)
    groupSize <- floor(length(affVar)/nClusters)
   for (i in 1:nClusters) {       
      if (i == nClusters) {
        thisGroup <- ((i-1)*groupSize + 1):length(affVar)
      } else thisGroup <- ((i-1)*groupSize + 1):(i*groupSize)
      theseVars <- data2[, thisGroup]
      sign <- sample(c(-1, 1), 1)
      bias <- sign*rnorm(n2, mean = biasSize, sd = sqrt(varSize))
      addBias <- function(x) x + bias
      data2[, thisGroup] <- sapply(theseVars, addBias)
    }
    data <- rbind(data1, data2)
    data$group <- c(rep("Group 1", n1), rep("Group 2", n2))
  } else data$group <- 1
  data
}

m <- lvm()
regression(m) <- c(DEMANDS, CONTENTS, RELATIONS, SITUATION) ~ VIDEN_GRAD.high + 
  VIDEN_GRAD.medium + ARB_TYPE.client + ARB_TYPE.customer + ARB_TYPE.knowledge
regression(m) <- c(S1.1, S1.2, S1.3, S1.4, S1.5) ~ DEMANDS
regression(m) <- c(S2.1, S2.2, S2.3, S2.4, S2.5, S2.6, S2.7, S2.8) ~ CONTENTS
regression(m) <- c(S3.1, S3.2, S3.3, S3.4, S3.5, S3.7, S3.8, S3.9, 
                    S3.10, S3.11, S3.12) ~ RELATIONS
regression(m) <- c(S5.1, S5.2, S5.3, S5.4, S5.5) ~ SITUATION
latent(m) <- ~ DEMANDS + CONTENTS + RELATIONS + SITUATION
variance(m) <- S1.5 ~ S1.2
variance(m) <- S2.5 ~ S2.8
variance(m) <- S2.3 ~ S2.1
variance(m) <- S2.1 ~ S2.2
variance(m) <- S2.3 ~ S2.5
variance(m) <- S3.2 ~ S3.1 + S3.3
variance(m) <- S3.1 ~ S3.3
variance(m) <- S3.10 ~ S3.9 + S3.11
variance(m) <- S3.5 ~ S3.7
variance(m) <- S5.2 ~ S5.1
variance(m) <- S5.4 ~ S5.2
variance(m) <- CONTENTS ~ RELATIONS
variance(m) <- CONTENTS ~ SITUATION
variance(m) <- RELATIONS ~ SITUATION
regression(m) <- c(SICK_TOTAL_t, Q36n, Q30n_t) ~ HEALTH
regression(m) <- STRESS ~ Q05 + Q08.yes + Q03.temp
regression(m) <- HEALTH ~ Q42.daily + Q42.sometimes + Q42.former + 
  BMI_CAT.obese + ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55 + 
  FAM_STATUS.cp + FAM_STATUS.ncp + FAM_STATUS.cnp
regression(m) <- c(MDI_t, STRESS) ~ HEALTH
regression(m) <- ANS_AAR_t ~ ALDERGRP2.35 + ALDERGRP2.45 + ALDERGRP2.55
regression(m) <- STRESS ~ ANS_AAR_t
regression(m) <- HEALTH ~ Q43_t
latent(m) <- ~ HEALTH
variance(m) <- STRESS ~ MDI_t
regression(m) <- c(HEALTH, DEMANDS, CONTENTS, SITUATION, 
                    RELATIONS) ~ SEG_KOEN + SEG_KOEN2
regression(m) <- HEALTH ~ DEMANDS + CONTENTS + SITUATION + RELATIONS

#Number of observed variables
nVar <- length(endogenous(m)) + length(exogenous(m))


#User interface
ui <- fluidPage(
  headerPanel(list(h1("PCA-based data structure comparison"), 
              h2("- An interactive simulation study"))),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bias", "Bias size", min=0, max=20, value=1),
      sliderInput("affD", "Percentage of biased variables", min=0, 
                  max=1, value=0.5),
      sliderInput("varS", "Size of variance of bias", min=0, max=20,
                  value=10),
      uiOutput("clusterSlider"),
      sliderInput("n1", "Number of observations in first group",
                  min=10, max=10000, value=337),
      sliderInput("n2", "Number of observations in second group", 
                  min=10, max=10000, value=2807),
      actionButton("do", "Simulate!")
    ),
    mainPanel(
      plotOutput("plot", width=800, height=1200)
    )
  )
)

#Server 
server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    do <- input$do #no function, just updates the plot
                  #whenever the users clicks on the "Simulate!" button
    biasSize <- input$bias
    affectDegree <- input$affD
    varSize <- input$varS
    nClusters <- ifelse(is.null(input$nClust), 3, input$nClust)
    data <- simMyData(lavaObj=m, n1=input$n1, n2=input$n2, 
                      biasSize=biasSize,
                      affectDegree=affectDegree, varSize=varSize,
                      nClusters=nClusters)
    useVars <- names(data)[which(names(data)!="group")]
    pcaObj <- pcaObjGen(data, var=useVars, splitBy="group")
    pcaPlot(pcaObj)
  })
  
  output$clusterSlider <- renderUI({
    nMax <- floor(input$affD*nVar)
    sliderInput("nClust", "Number of clusters", min=1, 
                max=nMax, value=min(3, nMax))
  })
}

#runApp(list(ui=ui, server=server)) #for testing
shinyApp(ui=ui, server=server)
