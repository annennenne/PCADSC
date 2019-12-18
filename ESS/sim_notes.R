library(MASS)
library(PCADSC)
library(ggplot2)
library(gridExtra)

#Simulate data from covariance matrices
#Note: Names (S1 and S2) are opposite of that in the article.

#set.seed(11234)
set.seed(170618)

S1 <- diag(6)
S1[c(1, 2, 3), 1] <- S1[1, c(1, 2, 3)] <- c(1, 0.2, 0.1)
S1[2, 3] <- S1[3, 2] <- 0.1
S1[c(4, 5, 6), 4] <- S1[4, c(4, 5, 6)] <- c(1, 0.3, 0.1)
S1[5, 6] <- S1[6, 5] <- 0.2

S2 <- diag(6)
S2[lower.tri(S2)] <- c(0, 0, 0, 0, 0,
                                    0, 0, 0, 0,
                                    0.7, 0, 0,
                                    0, 0,
                                    0.4)
S2[upper.tri(S2)] <- t(S2)[upper.tri(t(S2))]


d1 <- as.data.frame(mvrnorm(500, rep(0, 6), S1))
d2 <- as.data.frame(mvrnorm(500, rep(0, 6), S2))

dF1 <- rbind(d1, d2)
dF1$group <- rep(c("Group 1", "Group 2"), each = 500)

dF2 <- as.data.frame(mvrnorm(1000, rep(0,6), S2))
dF2$group <- rep(c("Group 1", "Group 2"), each = 500)

#Do PCADSC
po1 <- PCADSC(dF1, "group")
po2 <- PCADSC(dF2, "group")

CE1 <- CEPlot(po1) +
  scale_y_continuous(breaks = round(seq(-0.6, 0.6, 0.1),1), limits = c(-0.6, 0.3)) +
  ggtitle("Dataset B", subtitle = "Cumulative difference in eigenvalues: Group 1 - Group 2") +
  theme(panel.border = element_blank(),
        axis.line=element_line(),
        plot.subtitle = element_text(size = 11, hjust = -0.11),
        plot.title = element_text(hjust = 0.5)) +
  ylab("")

CE2 <- CEPlot(po2) +
  scale_y_continuous(breaks = round(seq(-0.6, 0.6, 0.1),1), limits = c(-0.2, 0.2)) +
  ggtitle("Dataset A", subtitle = "Cumulative difference in eigenvalues: Group 1 - Group 2") +
  theme(panel.border = element_blank(),
        axis.line=element_line(),
        plot.subtitle = element_text(size = 11, hjust = -0.11),
        plot.title = element_text(hjust = 0.5)) +
  ylab("")

angle1 <- anglePlot(po1) +
  theme(panel.border = element_blank(),
        axis.line=element_line(),
        plot.subtitle = element_text(size = 11, hjust = -0.03),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Dataset B", subtitle = "PCs for Group 2") +
  ylab("")

angle2 <- anglePlot(po2) +
  theme(panel.border = element_blank(),
        axis.line=element_line(),
        plot.subtitle = element_text(size = 11, hjust = -0.03),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Dataset A", subtitle = "PCs for Group 2") +
  ylab("")

chroma1 <- chromaPlot(po1) +
  theme(axis.line=element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("Dataset B")

chroma2 <- chromaPlot(po2) +
  theme(axis.line=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Dataset A")


#ggsave(plot = CE1, file = "./article/simCE1.pdf",
 #      width = 8, height = 5)
#ggsave(plot = CE2, file = "./article/simCE2.pdf",
#       width = 8, height = 5)

#ggsave(plot = grid.arrange(CE2, CE1, nrow = 2), file = "./article/PlosOne/Fig2.eps",
#       device = "eps", dpi = 400, width = 7, height = 8)



#ggsave(plot = grid.arrange(angle2, angle1, nrow = 2), file = "./article/PlosOne/Fig3.eps",
#       device = "eps", dpi = 400, width = 7, height = 8)

#ggsave(plot = grid.arrange(chroma2, chroma1, nrow = 2), file = "./article/PlosOne/Fig4.eps",
#       device = "eps", dpi = 400, width = 7, height = 8)


#CE2b <- CE2 + ggtitle("Simulated data A")
#ggsave(plot = CE2b, file = "P:/PCADSC/CSP/CEsim.pdf",
#       width = 8, height = 6)


#angle1b <- angle1 + ggtitle("Simulated data B")
#ggsave(plot = angle1b, file = "P:/PCADSC/CSP/anglesim.pdf",
#       width = 8, height = 6)

#chroma1b <- chroma1 + ggtitle("Simulated data B")
#ggsave(plot = chroma1b, file = "P:/PCADSC/CSP/chromasim.pdf",
#       width = 8, height = 6)


#ggsave(plot = angle1, file = "./article/simAngle1.pdf",
#       width = 8, height = 5)

#ggsave(plot = angle2, file = "./article/simAngle2.pdf",
#       width = 8, height = 5)

#ggsave(plot = chroma1, file = "./article/simChroma1.pdf",
 #      width = 8, height = 5)
#ggsave(plot = chroma2, file = "./article/simChroma2.pdf",
#       width = 8, height = 5)

#P:/PCADSC/R/
ggsave(plot = grid.arrange(CE2, CE1, nrow = 2),
       file = "article/Old versions/MPS/Figure2_v3.pdf",
       dpi = 400, width = 7, height = 8)

ggsave(plot = grid.arrange(angle2, angle1, nrow = 2),
       file = "article/Old versions/MPS/Figure3_v3.pdf",
       dpi = 400, width = 7, height = 8)

ggsave(plot = grid.arrange(chroma2, chroma1, nrow = 2),
       file = "article/Old versions/MPS/Figure4_v3.pdf",
       dpi = 400, width = 7, height = 8)

