#Load data
data <- read.csv("P:/PCADSC/data/ESS/ESS6.csv",
                 na.strings = c(".", ".c", ".d", ".b"))

useVars <- c("cntry", "stflife", "happy", "fltsd", "fltdpr", "enjlf", "wrhpp", "fltanx",
             "fltpcfl", "dclvlf", "lchshcp", "accdng", "tmimdng", "tmabdng",
             "tmendng","dngval", "sedirlf", "optftr", "lotsgot", "pstvms", "flrms",
             "wrbknrm", "deaimpp", "flteeff", "slprl", "cldgng", "enrglot", "ppltrst",
             "pplfair", "pplhlp", "pplahlp", "flclpla", "inprdsc", "flapppl", "rehlppl",
             "fltlnl")

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
anData$EmotionalWellbeing <- rowMeans(anData[, sMat$var[sMat$scale == "Emotional wellbeing"]])

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
anData$Vitality <- rowMeans(anData[, sMat$var[sMat$scale == "Vitality"]])

#Community vellbeing
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


#Do PCADSC
library(PCADSC)
a <- makePCADSC(anData, "cntry",  c("EvaluativeWellbeing",
                               "EmotionalWellbeing",
                               "Functioning",
                               "Vitality",
                               "CommunityWellbeing",
                               "SupportiveRelationships"))


p1 <- qplot(a)

ggsave(plot = p1, file = "./article/essPCADSC.pdf",
       width = 8, height = 4)

set.seed(1234)
p2 <- wallyPCADSC(a, anData, nrow = 4, ncol = 2)

ggsave(plot = p2, file = "./article/essWallyPCADSC.pdf",
       width = 8, height = 10)

