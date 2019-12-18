ggplot(pcaFrame, aes(x=comp, y=loading, fill=var)) +
  geom_bar(stat="identity", aes(width =  pcvc)) +
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
  scale_fill_discrete(name=NULL, labels=varLabels) +
  theme_bw()

ggplot(pcaFrame, aes(x=comp, y=loading, fill=var)) +
  geom_freqpoly(stat = "identity")

ggplot(pcaFrame[pcaFrame$group=="non-setosa",], aes(x=factor(cs),
                                                    y=loading, fill = var,
                                                    width = pcvc)) +
  geom_histogram(stat = "identity", position = position_fill(), col ="white") +
  scale_x_discrete(breaks = c(0, 0.25, 0.5,0.75, 1))
scale_x_reverse() +
  coord_flip()
+
  #  breaks = c(0,pcaFrame[pcaFrame$group=="setosa",]$cs) )+
  #coord_flip()  +
  scale_x_continuous( limits = c(0,1))
+
  #scale_x_reverse(breaks = seq(0, 1, 0.1)) +#breaks=c(1, seq(0.10, 0.50, 0.10))) +
  scale_y_continuous(limits=c(0, 1.4),
                     breaks=c(0, 0.25, 0.5, 0.75, 1)) +
  geom_text(aes(label=cvc), y=1.2, cex=4, na.rm=T) +
  xlab("Principal component") +
  ylab("Standardized loading") +
  theme(legend.position="bottom") +
  facet_wrap(~ group, ncol=2,
             labeller=as_labeller(facetLabels)) +
  scale_fill_discrete(name=NULL, labels=varLabels) +
  theme_bw()


pcaFrame$cs <- round(c(rep(cumsum(pcaFrame$pcvc[c(1, 5, 9, 13)]), each = 4),
                       rep(cumsum(pcaFrame$pcvc[c(17, 21, 25, 29)]), each = 4)), 2)


ggplot(pcaFrame[pcaFrame$group=="non-setosa",], aes(fill = var)) +
  #geom_rect(aes(xmin = shift(cumsum(loading), fill = 0),
  #             xmax = shift(cumsum(loading), type = "lead", fill = 1),
  #            ymin = shift(cs, fill = 0),
  #           ymax = shift(cs, fill = 1, type = "lead")))
  geom_rect(aes(xmin = shift(cumsum(loading), fill = 0),
                xmax = shift(cumsum(loading), type = "lead", fill = 1),
                ymin = rep(shift(unique(cs), fill = 0), each = 4),
                ymax = rep(shift(unique(cs), fill = 1, type = "lead"), each = 4)))



with(pcaFrame[pcaFrame$group=="non-setosa",],
     data.frame(xmin = shift(cumsum(loading), fill = 0),
                xmax = shift(cumsum(loading), type = "lead", fill = 1),
                ymin = rep(shift(unique(cs), fill = 0), each = 4),
                ymax = rep(shift(unique(cs), fill = 1, type = "lead"), each = 4)))

pf <- pcaFrame[pcaFrame$group=="non-setosa",]
boxFrame <- data.frame(xmin = ,
                       xmax = ,
                       ymin = ,
                       ymax = ,
                       comp = ,
                       var = ,
                       group = ,
                       cvc = )
)
#4 is nComp
for (i in 1:4) {
  boxFrame[boxFrame$comp == i, ] <- cumsum(boxFrame)
}
}
