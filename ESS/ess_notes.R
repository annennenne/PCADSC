#Load data
data <- read.csv("P:/PCADSC/data/ESS/ESS6.csv",
                 na.strings = c(".", ".c", ".d", ".b"))

useVars <- c("cntry", "stflife", "happy", "fltsd", "fltdpr", "enjlf", "wrhpp", "fltanx",
             "fltpcfl", "dclvlf", "lchshcp", "accdng", "tmimdng", "tmabdng",
             "tmendng","dngval", "sedirlf", "optftr", "lotsgot", "pstvms", "flrms",
             "wrbknrm", "deaimpp", "flteeff", "slprl", "cldgng", "enrglot", "ppltrst",
             "pplfair", "pplhlp", "pplahlp", "flclpla", "inprdsc", "flapppl", "rehlppl",
             "fltlnl")
#
#anData <- na.omit(data[data$cntry %in% c("DK", "BG"), useVars])
anData <- na.omit(data[data$cntry %in% c("DK", "SE"), useVars])


anData$cntry <- as.character(anData$cntry)

sMat <- list()
sMat$var <- names(anData)[-1]
sMat$scale <- c(rep("Evaluative wellbeing", 2),
               rep("Emotional wellbeing", 6),
               rep("Functioning", 14),
               rep("Vitality", 4),
               rep("Community wellbeing", 5),
               rep("Supportive relationships", 4))


#Evaluative wellbeing
sapply(anData[, sMat$var[sMat$scale == "Evaluative wellbeing"]], range) #same number of levels
anData$EvaluativeWellbeing <- rowMeans(anData[, sMat$var[sMat$scale == "Evaluative wellbeing"]])

#Emotional wellbeing
sapply(anData[, sMat$var[sMat$scale == "Emotional wellbeing"]], range) #same number of levels
for (v in sMat$var[sMat$scale == "Emotional wellbeing"])  {
  anData[, v] <- anData[, v] - 1
}
for (v in c("fltsd", "fltdpr", "fltanx")) {
  anData[, v] <- 3 - anData[, v]
}
anData$EmotionalWellbeing <- (10/3)*rowMeans(anData[, sMat$var[sMat$scale == "Emotional wellbeing"]])

#Functioning
sapply(anData[, sMat$var[sMat$scale == "Functioning"]], range)
for (v in setdiff(sMat$var[sMat$scale == "Functioning"],
                  c("tmimdng", "tmabdng", "tmendng", "sedirlf", "deaimpp")))  {
  anData[, v] <- (anData[, v]-1)*2.5
}
for (v in c("dclvlf", "accdng", "dngval", "optftr", "lotsgot", "pstvms")) {
  anData[, v] <- 10 - anData[, v]
}
anData$Functioning <- rowMeans(anData[, sMat$var[sMat$scale == "Functioning"]])

#Vitality
sapply(anData[, sMat$var[sMat$scale == "Vitality"]], range)
for (v in sMat$var[sMat$scale == "Vitality"])  {
  anData[, v] <- anData[, v] - 1
}
for (v in c("flteeff", "slprl", "cldgng")) {
  anData[, v] <- 3 - anData[, v]
}
anData$Vitality <- (10/3)*rowMeans(anData[, sMat$var[sMat$scale == "Vitality"]])

#Community wellbeing
sapply(anData[, sMat$var[sMat$scale == "Community wellbeing"]], range)
anData$pplahlp <- (10/6)*anData$pplahlp
anData$flclpla <- 10 - ((10/4)*(anData$flclpla - 1))
anData$CommunityWellbeing <- rowMeans(anData[, sMat$var[sMat$scale == "Community wellbeing"]])

#Supportive relationships
sapply(anData[, sMat$var[sMat$scale == "Supportive relationships"]], range)
anData$inprdsc <- anData$inprdsc * (10/6)
anData$rehlppl <- anData$rehlppl * (10/6)
anData$fltlnl <- 10 - ((anData$fltlnl - 1) * (10/3))
anData$SupportiveRelationships <- rowMeans(anData[, sMat$var[sMat$scale ==
                                                               "Supportive relationships"]])

data_SEDK <- anData

anData <- na.omit(data[data$cntry %in% c("DK", "BG"), useVars])

anData$cntry <- as.character(anData$cntry)

sMat <- list()
sMat$var <- names(anData)[-1]
sMat$scale <- c(rep("Evaluative wellbeing", 2),
                rep("Emotional wellbeing", 6),
                rep("Functioning", 14),
                rep("Vitality", 4),
                rep("Community wellbeing", 5),
                rep("Supportive relationships", 4))


#Evaluative wellbeing
sapply(anData[, sMat$var[sMat$scale == "Evaluative wellbeing"]], range) #same number of levels
anData$EvaluativeWellbeing <- rowMeans(anData[, sMat$var[sMat$scale == "Evaluative wellbeing"]])

#Emotional wellbeing
sapply(anData[, sMat$var[sMat$scale == "Emotional wellbeing"]], range) #same number of levels
for (v in sMat$var[sMat$scale == "Emotional wellbeing"])  {
  anData[, v] <- anData[, v] - 1
}
for (v in c("fltsd", "fltdpr", "fltanx")) {
  anData[, v] <- 3 - anData[, v]
}
anData$EmotionalWellbeing <- (10/3)*rowMeans(anData[, sMat$var[sMat$scale == "Emotional wellbeing"]])

#Functioning
sapply(anData[, sMat$var[sMat$scale == "Functioning"]], range)
for (v in setdiff(sMat$var[sMat$scale == "Functioning"],
                  c("tmimdng", "tmabdng", "tmendng", "sedirlf", "deaimpp")))  {
  anData[, v] <- (anData[, v]-1)*2.5
}
for (v in c("dclvlf", "accdng", "dngval", "optftr", "lotsgot", "pstvms")) {
  anData[, v] <- 10 - anData[, v]
}
anData$Functioning <- rowMeans(anData[, sMat$var[sMat$scale == "Functioning"]])

#Vitality
sapply(anData[, sMat$var[sMat$scale == "Vitality"]], range)
for (v in sMat$var[sMat$scale == "Vitality"])  {
  anData[, v] <- anData[, v] - 1
}
for (v in c("flteeff", "slprl", "cldgng")) {
  anData[, v] <- 3 - anData[, v]
}
anData$Vitality <- (10/3)*rowMeans(anData[, sMat$var[sMat$scale == "Vitality"]])

#Community wellbeing
sapply(anData[, sMat$var[sMat$scale == "Community wellbeing"]], range)
anData$pplahlp <- (10/6)*anData$pplahlp
anData$flclpla <- 10 - ((10/4)*(anData$flclpla - 1))
anData$CommunityWellbeing <- rowMeans(anData[, sMat$var[sMat$scale == "Community wellbeing"]])

#Supportive relationships
sapply(anData[, sMat$var[sMat$scale == "Supportive relationships"]], range)
anData$inprdsc <- anData$inprdsc * (10/6)
anData$rehlppl <- anData$rehlppl * (10/6)
anData$fltlnl <- 10 - ((anData$fltlnl - 1) * (10/3))
anData$SupportiveRelationships <- rowMeans(anData[, sMat$var[sMat$scale ==
                                                               "Supportive relationships"]])

data_BGDK <- anData

#Summarize data
uVs <- c("EvaluativeWellbeing",
         "EmotionalWellbeing",
         "Functioning",
         "Vitality",
         "CommunityWellbeing",
         "SupportiveRelationships")
nU <- length(uVs)

m <- data.frame(var = uVs,
                Q1_DK = rep(NA, nU),
                M_DK = rep(NA, nU),
                Q3_DK = rep(NA, nU),
                Q1_BG = rep(NA, nU),
                M_BG = rep(NA, nU),
                Q3_BG = rep(NA, nU),
                Q1_SE = rep(NA, nU),
                M_SE = rep(NA, nU),
                Q3_SE = rep(NA, nU))

m[, "Q1_DK"] <-sapply(1:nU, function(x) quantile(data_BGDK[data_BGDK$cntry == "DK", uVs[x]], 0.25))
m[, "M_DK"] <-sapply(1:nU, function(x) quantile(data_BGDK[data_BGDK$cntry == "DK", uVs[x]], 0.5))
m[, "Q3_DK"] <-sapply(1:nU, function(x) quantile(data_BGDK[data_BGDK$cntry == "DK", uVs[x]], 0.75))
m[, "Q1_BG"] <-sapply(1:nU, function(x) quantile(data_BGDK[data_BGDK$cntry == "BG", uVs[x]], 0.25))
m[, "M_BG"] <-sapply(1:nU, function(x) quantile(data_BGDK[data_BGDK$cntry == "BG", uVs[x]], 0.5))
m[, "Q3_BG"] <-sapply(1:nU, function(x) quantile(data_BGDK[data_BGDK$cntry == "BG", uVs[x]], 0.75))
m[, "Q1_SE"] <-sapply(1:nU, function(x) quantile(data_SEDK[data_SEDK$cntry == "SE", uVs[x]], 0.25))
m[, "M_SE"] <-sapply(1:nU, function(x) quantile(data_SEDK[data_SEDK$cntry == "SE", uVs[x]], 0.5))
m[, "Q3_SE"] <-sapply(1:nU, function(x) quantile(data_SEDK[data_SEDK$cntry == "SE", uVs[x]], 0.75))

library(xtable)
xtable(m, digits = 2)

#Do PCADSC
library(PCADSC)
library(ggplot2)
library(gridExtra)

set.seed(1234)
a1 <- PCADSC(data_SEDK, "cntry",  c("EvaluativeWellbeing",
                               "EmotionalWellbeing",
                               "Functioning",
                               "Vitality",
                               "CommunityWellbeing",
                               "SupportiveRelationships"))


p1 <- chromaPlot(a1)

ggsave(plot = p1, file = "./article/essDKSEpancake.pdf",
       width = 8, height = 5)

set.seed(123424)
a2 <- PCADSC(data_BGDK, "cntry",  c("EvaluativeWellbeing",
                                       "EmotionalWellbeing",
                                       "Functioning",
                                       "Vitality",
                                       "CommunityWellbeing",
                                       "SupportiveRelationships"))


p2 <- chromaPlot(a2)

ggsave(plot = p2, file = "./article/essDKBGpancake.pdf",
       width = 8, height = 5)

p2c <- chromaPlot(a2, useComps = 2:4)

ggsave(plot = p2c, file = "./article/essDKBGpancake234.pdf",
       width = 8, height = 5)


#Do cumeigen stuff:
CEPlot(a1) +
  scale_y_continuous(breaks = round(seq(-0.6, 0.6, 0.1),1), limits = c(-0.3, 0.3))

ggsave(file = "./article/essDKSEce.pdf", width = 8, height = 5)

CEPlot(a2) +
  scale_y_continuous(breaks = round(seq(-0.3, 0.5, 0.1),1), limits = c(-0.2, 0.5))

ggsave(file = "./article/essDKBGce.pdf", width = 8, height = 5)



#Do hairplot:

anglePlot(a1)
ggsave(file = "./article/essDKSEhair.pdf", width = 8, height = 5)

anglePlot(a2)
ggsave(file = "./article/essDKBGhair.pdf", width = 8, height = 5)


#Data overview
#library(dataMaid)
#clean(anData, useVars = c("cntry", "EvaluativeWellbeing",
#                                     "EmotionalWellbeing",
#                                     "Functioning",
#                                     "Vitality",
#                                     "CommunityWellbeing",
#                                     "SupportiveRelationships"), replace = TRUE)
