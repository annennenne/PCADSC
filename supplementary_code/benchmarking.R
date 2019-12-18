#Angle plot
microbenchmark({
  data(iris)
  iris$group <- "setosa"
  iris$group[iris$Species != "setosa"] <- "non-setosa"

  irisPCADSC <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")))
  irisPCADSC2 <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")), doAngle = FALSE)
  irisPCADSC2 <- doAngle(irisPCADSC2)
  anglePlot(irisPCADSC)
  anglePlot(irisPCADSC2)
  }, times = 10)
#min       lq     mean   median       uq      max neval
#4.061592 4.085913 4.364346 4.210754 4.329815 5.858282    10


#CE plot
microbenchmark({
  data(iris)
  iris$group <- "setosa"
  iris$group[iris$Species != "setosa"] <- "non-setosa"

  irisPCADSC <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")))
  irisPCADSC2 <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")),
                        doCE = FALSE)
  irisPCADSC2 <- doCE(irisPCADSC2)

  CEPlot(irisPCADSC)
  CEPlot(irisPCADSC2)
}, times = 10)
#min      lq     mean   median       uq      max neval
#3.612281 3.70354 3.852662 3.841499 3.892452 4.158875    10

#Chroma plot
microbenchmark({
  data(iris)
  iris$group <- "setosa"
  iris$group[iris$Species != "setosa"] <- "non-setosa"

  irisPCADSC <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")))
  irisPCADSC2 <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")),
                        doChroma = FALSE)
  irisPCADSC2 <- doChroma(irisPCADSC2)

  chromaPlot(irisPCADSC)
  chromaPlot(irisPCADSC)

  chromaPlot(irisPCADSC, splitLabels = list("non-setosa" = "Not Setosa",
                                            "setosa" = "Setosa"))

  chromaPlot(irisPCADSC, useComps = c(1,4), varAnnotation = "no")
  chromaPlot(irisPCADSC, cvCO = 0.8)
  chromaPlot(irisPCADSC, varLabels = c("Sepal length", "Sepal width", "Petal length",
                                       "Petal width"))
})

#do Angle
microbenchmark({
#' #load iris data
#' data(iris)
#'
#' #Define grouping variable, grouping the observations by whether their species is
#' #Setosa or not
#' iris$group <- "setosa"
#' iris$group[iris$Species != "setosa"] <- "non-setosa"
#'
#' #make a partial PCADSC object, splitting the data by "group"
#' irisPCADSC <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")),
#'    doAngle = FALSE)
#'
#' #No angleInfo available
#' irisPCADSC$angleInfo
#'
#' #Add and show angleInfo
#' irisPCADSC <- doAngle(irisPCADSC)
#' irisPCADSC$angleInfo
#' }
#'
})

#doCE
microbenchmark({

#' #load iris data
#' data(iris)
#'
#' #Define grouping variable, grouping the observations by whether their species is
#' #Setosa or not
#' iris$group <- "setosa"
#' iris$group[iris$Species != "setosa"] <- "non-setosa"
#'
#' #make a partial PCADSC object, splitting the data by "group"
#' irisPCADSC <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")),
#'    doCE = FALSE)
#'
#' #No CEInfo available
#' irisPCADSC$CEInfo
#'
#' #Add and show CEInfo
#' irisPCADSC <- doCE(irisPCADSC)
#' irisPCADSC$CEInfo
#' }
#'
})

#doChroma
microbenchmark({
#' #load iris data
#' data(iris)
#'
#' #Define grouping variable, grouping the observations by whether their species is
#' #Setosa or not
#' iris$group <- "setosa"
#' iris$group[iris$Species != "setosa"] <- "non-setosa"
#'
#' #make a partial PCADSC object, splitting the data by "group"
#' irisPCADSC <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")),
#'    doChroma = FALSE)
#'
#' #No chromaInfo available
#' irisPCADSC$chromaInfo
#'
#' #Add and show chromaInfo
#' irisPCADSC <- doChroma(irisPCADSC)
#' irisPCADSC$chromaInfo
#' }
#'
})

#PCADSC
microbenchmark({

#' #load iris data
#' data(iris)
#'
#' #Define grouping variable, grouping the observations by whether their species is
#' #Setosa or not
#' iris$group <- "setosa"
#' iris$group[iris$Species != "setosa"] <- "non-setosa"
#'
#' #Make a full PCADSC object, splitting the data by "group"
#' irisPCADSC <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")))
#'
#' #The three plotting functions can now be called on irisPCADSC:
#' CEPlot(irisPCADSC)
#' anglePlot(irisPCADSC)
#' chromaPlot(irisPCADSC)
#'
#' #Make a partial PCADSC object with no angle plot information and add
#' #angle plot information afterwards:
#' irisPCADSC2 <- PCADSC(iris, "group", setdiff(names(iris), c("group", "Species")),
#'    doAngle = FALSE)
#' irisPCADSC2 <- doAngle(irisPCADSC)
#' }
})
