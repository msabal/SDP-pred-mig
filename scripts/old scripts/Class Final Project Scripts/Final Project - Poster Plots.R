

#### SABAL FINAL PROJECT ####

#### PLOTS for poster ####

# Run R scripts for both Constant and Energy-dependent Ocean Predation. Then make plots.


setwd("C:/Users/Megan/Desktop/")
pdf("T.FITNESS.pdf", width=6, height=4.5)
curve(T.FITNESS(X, m=5), xlim=c(0, 10), ylab="probability of surviving to adult",
      xlab="X(T, L10), energy reserves at time T in last location", main="Terminal Fitness Function",
      xname = "X")
dev.off()

setwd("C:/Users/Megan/Desktop/")
pdf("MASS.DEP.pdf", width=6, height=4.5)
curve(MASS.PRED.OCEAN(X, n = -7), xlim=c(0, 10), ylab="probability of predation (B)",
      xlab="X(T, L10), energy reserves at time T in last location", main="Mass-dependent ocean predation",
      xname = "X")
dev.off()

setwd("C:/Users/Megan/Desktop/")
pdf("TRACKS.CON.pdf", width=7.5, height=11)
indiv.constant
dev.off()


setwd("C:/Users/Megan/Desktop/")
pdf("TRACKS.DEP.pdf", width=7.5, height=11)
indiv.mass
dev.off()



#### DECISION MATRIX PLOTS WITH BOTH SCENARIOS
# decision matrix plots for best behaviors at each location
mass.dep1_5 <- ggplot(data=subset(df.all, L %in% c(1,2,3,4,5)), aes(x=Time, y=X, fill=Best.beh)) + geom_tile(color="gray90") + facet_wrap(~ L, ncol = 1) +
  scale_fill_manual(values = c("cornflowerblue", "gold", "limegreen")) + coord_equal() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(strip.background = element_blank(), strip.text = element_blank(), legend.position = "none") +
  theme(axis.text = element_text(size=5))
mass.dep1_5

mass.dep6_10 <- ggplot(data=subset(df.all, L %in% c(6,7,8,9,10)), aes(x=Time, y=X, fill=Best.beh)) + geom_tile(color="gray90") + facet_wrap(~ L, ncol = 1) +
  scale_fill_manual(values = c("cornflowerblue", "gold", "limegreen")) + coord_equal() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(strip.background = element_blank(), strip.text = element_blank(), legend.position = "none") +
  theme(axis.text = element_text(size=5))
mass.dep6_10

# decision matrix plots for best behaviors at each location
constant.pred1_5 <- ggplot(data=subset(df.all1, L %in% c(1,2,3,4,5)), aes(x=Time, y=X, fill=Best.beh)) + geom_tile(color="gray90") + facet_wrap(~ L, ncol = 1) +
  scale_fill_manual(values = c("cornflowerblue", "gold", "limegreen")) + coord_equal() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(strip.background = element_blank(), strip.text = element_blank(), legend.position = "none")  +
  theme(axis.text = element_text(size=5))
constant.pred1_5 

constant.pred6_10 <- ggplot(data=subset(df.all1, L %in% c(6,7,8,9,10)), aes(x=Time, y=X, fill=Best.beh)) + geom_tile(color="gray90") + facet_wrap(~ L, ncol = 1) +
  scale_fill_manual(values = c("cornflowerblue", "gold", "limegreen")) + coord_equal() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(strip.background = element_blank(), strip.text = element_blank(), legend.position = "none") +
  theme(axis.text = element_text(size=5))
constant.pred6_10

library(gridExtra)
grid.arrange(constant.pred1_5, mass.dep1_5, ncol=2)
grid.arrange(constant.pred6_10, mass.dep6_10, ncol=2)

setwd("C:/Users/Megan/Desktop/")
pdf("CONSTANT.1_5.pdf", width=4.5, height=11)
constant.pred1_5
dev.off()

setwd("C:/Users/Megan/Desktop/")
pdf("DEP.1_5.pdf", width=4.5, height=11)
mass.dep1_5
dev.off()

setwd("C:/Users/Megan/Desktop/")
pdf("CONSTANT.6_10.pdf", width=4.5, height=11)
constant.pred6_10
dev.off()

setwd("C:/Users/Megan/Desktop/")
pdf("DEP.6_10.pdf", width=4.5, height=11)
mass.dep6_10
dev.off()


legend.only <- ggplot(data=subset(df.all, L == 1), aes(x=Time, y=X, fill=Best.beh)) + geom_tile(color="gray90") + facet_wrap(~ L, ncol = 1) +
  scale_fill_manual(name= "Best behavior: move __ locations", values = c("cornflowerblue", "gold", "limegreen")) + coord_equal() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  theme(legend.text=element_text(size=20), legend.title=element_text(size=20), legend.position = "bottom")
legend.only  # print this, and only cut out the legend!!!

setwd("C:/Users/Megan/Desktop/")
pdf("LEGEND.pdf", width=8, height=11)
legend.only
dev.off()
