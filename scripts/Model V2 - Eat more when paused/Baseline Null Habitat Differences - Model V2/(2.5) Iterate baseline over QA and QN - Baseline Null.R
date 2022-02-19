#### Iterate baseline parameters over qa and qn

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

#### Iterate Main Function over varying parameters
kn <- 1.3

ka <- seq(0.9, 1.3, by=0.1)

OUT.KA <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(ka)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0, Ba, Bn, Bo, ka[i], kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$ka <- rep(ka[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.KA[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 16 mins

DF.KA<-ldply(OUT.KA, as.vector)

## Export DF.QA
#write.csv(DF.KA, "H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.KA.csv")



#### Iterate Main Function over varying parameters
ka <- 1.3

kn <- seq(0.9, 1.3, by=0.1)

OUT.KN <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(kn)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0, Ba, Bn, Bo, ka, kn[i], # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$kn <- rep(kn[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.KN[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 1.83 hours for seeds length = 10!

DF.KN<-ldply(OUT.KN, as.vector)

## Export DF.QN
#write.csv(DF.KN, "H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.KN.V2.csv")

# Join DF.QA and DF.QN 

DF.KA$kn <- rep(1.3, length(DF.KA$Wstart)) # make kn column and set all values to 1
DF.KN$ka <- rep(1.3, length(DF.KN$Wstart)) # make ka column and set all values to 1

DF.K <- rbind(DF.KA, DF.KN)

# Calculate Qn/Qa ratios!
DF.K$kn_ka <- DF.K$kn / DF.K$ka


## Export DF.Q
write.csv(DF.K, "H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.K.V2.Baseline-Null.csv")
DF.K <- read.csv("H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.K.V2.Baseline-Null.csv", sep=",")

# Aggregate  by Wstart (salmon size)
# summarize data for barplot
bar.cp<-aggregate(p.tot ~ h + kn_ka, data=subset(DF.K, Beh == 0), mean)
a<-aggregate(p.tot ~ h + kn_ka, data=subset(DF.K, Beh == 0), sd); colnames(a)[3]<-"sd"
b<-aggregate(p.tot ~ h + kn_ka, data=subset(DF.K, Beh == 0), length); colnames(b)[3]<-"n"
bar.cp<-join(bar.cp, a); bar.cp<-join(bar.cp, b)
bar.cp$se<-bar.cp$sd / sqrt(bar.cp$n)

bar.cp$h<-as.factor(bar.cp$h)
levels(bar.cp$h) <- c("Altered", "Natural")


# Plots!

plot_kn_ka <- ggplot(data=bar.cp, aes(x=log(kn_ka), y=p.tot, fill=h, color=h)) + 
  geom_vline(xintercept = 0, linetype="dashed",  color = "gray24", size=0.5) +
  geom_vline(xintercept = log(1.3/0.9), linetype="dashed",  color = "skyblue", size=0.5) +
  geom_line(size=0.5, aes(color=h)) +
  geom_errorbar(aes(ymax=p.tot + se, ymin=p.tot - se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") + 
  theme_classic() + ylim(c(0,1)) +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab("Frequency of move 0") + xlab(expression(paste(ln(k[n]:k[a])))) +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.8, 0.8), legend.background = element_rect(fill="transparent")) +
  annotate(geom="text", x=-0.2, y=1, label="less food\nin natural", color="forestgreen", fontface="bold") +
  annotate(geom="text", x=0.2, y=1, label="less food\nin altered", color="mediumpurple", fontface="bold") +
  annotate(geom="text", x=log(1.3/0.9), y=0.5, label="baseline", color="skyblue", fontface="bold")


plot_kn_ka

setwd("C:/Users/Megan/Desktop/")
pdf("Fig_Ka_Kn.pdf", width=4.5, height=4)

plot_kn_ka

dev.off()

## Other plots!
ggplot(data=DF.K, aes(x=kn_ka, y=p0.a, color=Wstart)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))
ggplot(data=DF.K, aes(x=kn_ka, y=p0.n, color=Wstart)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))

ggplot(data=subset(DF.K.LONG, Beh == 0 & Wstart == 10), aes(x=kn_ka, y=p, color=h)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))
ggplot(data=subset(DF.K.LONG, Beh == 1 & Wstart == 10), aes(x=kn_ka, y=p, color=h)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))
ggplot(data=subset(DF.K.LONG, Beh == 2 & Wstart == 10), aes(x=kn_ka, y=p, color=h)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))


