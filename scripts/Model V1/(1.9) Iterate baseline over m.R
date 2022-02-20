#### Iterate baseline parameters over m (strength of size-selective mortality)

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

#### Iterate Main Function over varying parameters
m <- seq(-1, -0.1, by=0.15)

OUT.m <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(m)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m[i], y, P, # vars in functions
                  qa, qn, ya, yn, yo, dn0, Ba, Bn, Bo, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$m <- rep(m[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.m[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 59 mins

DF.m<-ldply(OUT.m, as.vector)

## Export DF.QA
write.csv(DF.m, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.m.csv")

DF.m <- read.csv("C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.m.csv", sep=",")


# FIGURE X. Wstart by p.tot by habitat.

DF.m$h<-as.factor(DF.m$h)
levels(DF.m$h) <- c("Altered", "Natural")

DF.m$Wstart<-as.factor(DF.m$Wstart)

plot_move0_by_m <- ggplot(data=subset(DF.m, Beh == 0 & h == "Natural"), aes(x=m, y=p.tot, fill=Wstart, color=Wstart)) + 
  geom_line(size=0.5) + geom_point(size=2, shape=21, color="black") +
  theme_classic() + ylim(c(0,1)) +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab("Frequency of move 0") + xlab("Starting salmon size (g)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.8, 0.8), legend.background = element_rect(fill="transparent"))


plot_move0_by_m

setwd("C:/Users/Megan/Desktop/")
pdf("Fig_move0_by_m.pdf", width=4.5, height=4)

plot_move0_by_m

dev.off()


# Plot with Wstarting salmon size on x-axis
DF.m <- read.csv("C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.m.csv", sep=",")

DF.m$h<-as.factor(DF.m$h)
levels(DF.m$h) <- c("Altered", "Natural")

DF.m$Wstart<-as.numeric(DF.m$Wstart)
DF.m$m<-as.factor(DF.m$m)

plot_move0_by_m2 <- ggplot(data=subset(DF.m, Beh == 0), aes(x=Wstart, y=p.tot, fill=m, color=m)) + 
  geom_line(size=0.5, aes(color=m, linetype=h), position=position_dodge(0.4)) + 
  geom_point(size=2, color="black", position=position_dodge(1), shape=21) +
  theme_classic() + ylim(c(0,1)) +
  ylab("Frequency of move 0") + xlab("Starting salmon size (g)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.6, 0.85), legend.background = element_rect(fill="transparent"),
        legend.direction = "horizontal")

plot_move0_by_m2

setwd("C:/Users/Megan/Desktop/")
pdf("Fig_move0_by_m2.pdf", width=4.5, height=4)

plot_move0_by_m2

dev.off()


# Try same plot for move 1
plot_move1_by_m2 <- ggplot(data=subset(DF.m, Beh == 1), aes(x=Wstart, y=p.tot, fill=m, color=m)) + 
  geom_line(size=0.5, aes(color=m, linetype=h), position=position_dodge(0.4)) + 
  geom_point(size=2, color="black", position=position_dodge(1), shape=21) +
  theme_classic() + ylim(c(0,1)) +
  ylab("Frequency of move 1") + xlab("Starting salmon size (g)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.6, 0.85), legend.background = element_rect(fill="transparent"),
        legend.direction = "horizontal")

plot_move1_by_m2

setwd("C:/Users/Megan/Desktop/")
pdf("Fig_move1_by_m2.pdf", width=4.5, height=4)

plot_move1_by_m2

dev.off()



# Try same plot for move 2
plot_move2_by_m2 <- ggplot(data=subset(DF.m, Beh == 2), aes(x=Wstart, y=p.tot, fill=m, color=m)) + 
  geom_line(size=0.5, aes(color=m, linetype=h), position=position_dodge(0.4)) + 
  geom_point(size=2, color="black", position=position_dodge(1), shape=21) +
  theme_classic() + ylim(c(0,1)) +
  ylab("Frequency of move 2") + xlab("Starting salmon size (g)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.6, 0.85), legend.background = element_rect(fill="transparent"),
        legend.direction = "horizontal")

plot_move2_by_m2


setwd("C:/Users/Megan/Desktop/")
pdf("Fig_move2_by_m2.pdf", width=4.5, height=4)

plot_move2_by_m2

dev.off()



#### Plot varied m values.

Beta.W <- function(X, Bw, m){ Bw*X^m }
curve(Beta.W(X, Bw=2, m= -0.37), xname="X", xlim=c(7,20), ylim=c(0,2), ylab="contrib. daily mortality rate", col="blue")
curve(Beta.W(X, Bw=2, m= -0.1), xname="X", add=T)
curve(Beta.W(X, Bw=2, m= -0.25), xname="X", add=T)
curve(Beta.W(X, Bw=2, m= -0.55), xname="X", add=T)
curve(Beta.W(X, Bw=2, m= -1), xname="X", add=T, col="red")
curve(Beta.W(X, Bw=2, m= -0.85), xname="X", add=T)
abline(h=1,lty="dashed")


curve(Beta.W(X, Bw=2, m= -0.37), xname="X", ylab="contrib. daily mortality rate", col="blue")
