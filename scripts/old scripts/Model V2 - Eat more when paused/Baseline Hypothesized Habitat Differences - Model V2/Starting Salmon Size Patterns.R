
#### Model Version 2: Baseline Parameters Hypothesized

#### Starting Size (Wstart) Patterns
library(ggplot2)


DF.K <- read.csv("H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.K.V2.csv", sep=",")

DF.K$h<-as.factor(DF.K$h)
levels(DF.K$h) <- c("Altered", "Natural")

DF.K$Wstart<-as.numeric(DF.K$Wstart)
DF.K$kn_ka<-as.factor(DF.K$kn_ka)

Wstart_by_Pauses <- ggplot(data=subset(DF.K, Beh == 0 & kn_ka == 1.3), aes(x=Wstart, y=p.tot, fill=h, color=h)) + 
  geom_line(aes(linetype=h), size=1) + geom_point(shape=21, size=3, color="black") +
  theme_classic() + ylim(c(0,1)) +
  scale_fill_manual(values=c("mediumpurple1","green3")) +
  scale_color_manual(values=c("mediumpurple1", "green3")) +
  ylab("Frequency of move 0") + xlab("Starting salmon size (g)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.6, 0.85), legend.background = element_rect(fill="transparent"),
        legend.direction = "horizontal")

Wstart_by_Pauses

setwd("C:/Users/Megan/Desktop/")
pdf("Wstart_by_Pauses.pdf", width=4.5, height=4)
Wstart_by_Pauses
dev.off()


# River growth
ggplot(data=subset(DF.K, Beh == 0 & kn_ka == 1.3), aes(x=Wstart, y=G.riv)) + 
  geom_line(size=1) + geom_point(shape=21, size=3, color="black", fill="gray") +
  theme_classic() +
  ylab("Growth in the river (g)") + xlab("Starting salmon size (g)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11))



# River growth g/day
DF.K$G.day <- DF.K$G.riv / DF.K$dur

Wstart_by_Gday <- ggplot(data=subset(DF.K, Beh == 0 & kn_ka == 1.3), aes(x=Wstart, y=G.day)) + 
  geom_line(size=1) + geom_point(shape=21, size=3, color="black", fill="gray") +
  theme_classic() +
  ylab("Growth in the river (g/day)") + xlab("Starting salmon size (g)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  geom_hline(yintercept=0.25, linetype="dashed") +
  geom_hline(yintercept=0.07, linetype="dashed")

Wstart_by_Gday

setwd("C:/Users/Megan/Desktop/")
pdf("Wstart_by_Gday.pdf", width=4.5, height=4)
Wstart_by_Gday
dev.off()

# Ocean growth
ggplot(data=subset(DF.K, Beh == 0 & kn_ka == 1.3), aes(x=Wstart, y=G.ocean)) + 
  geom_line(size=1) + geom_point(shape=21, size=3, color="black", fill="gray") +
  theme_classic() +
  ylab("Growth in the ocean (g)") + xlab("Starting salmon size (g)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11))


# River Survival
ggplot(data=subset(DF.K, Beh == 0 & kn_ka == 1.3), aes(x=Wstart, y=S.cum.riv)) + 
  geom_line(size=1) + geom_point(shape=21, size=3, color="black", fill="gray") +
  theme_classic() + ylim(c(0,0.13)) +
  geom_hline(yintercept=0, linetype="dashed", size=1) +
  ylab("Cumulative river survival") + xlab("Starting salmon size (g)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11))



############################ change details below.
plot_move0_by_m2

setwd("C:/Users/Megan/Desktop/")
pdf("Fig_move0_by_m2.pdf", width=4.5, height=4)

plot_move0_by_m2

dev.off()








