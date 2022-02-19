#### Iterate baseline parameters over Ba and Bn

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

#### Iterate Main Function over varying parameters
Bn <- 1

Ba <- seq(0.1,1, by=0.2)

OUT.BA <- list()

start.time <- Sys.time() # time how long the while loop takes



# Start looping MAIN_FUN over different qa values
for(i in 1:length(Ba)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0, Ba[i], Bn, Bo, ka, kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$Ba <- rep(Ba[i], length(OUT$Wstart)) # add column with var value for that iteration.
  
  OUT.BA[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 53 mins

DF.BA<-ldply(OUT.BA, as.vector)

## Export DF.QA
#write.csv(DF.BA, "H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.BA.V2.Baseline-Null.csv")



#### Iterate Main Function over varying parameters
Ba <- 1

Bn <- seq(0.1,0.9, by=0.2)

OUT.BN <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(Bn)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0, Ba, Bn[i], Bo, ka, kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$Bn <- rep(Bn[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.BN[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 1.83 hours for seeds length = 10!

DF.BN<-ldply(OUT.BN, as.vector)

## Export DF.QA
#write.csv(DF.BN, "H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.BN.V2.Baseline-Null.csv")

# Join DF.BA and DF.BN

DF.BA$Bn <- rep(1, length(DF.BA$Wstart)) # make Bn column and set all values to 1
DF.BN$Ba <- rep(1, length(DF.BN$Wstart)) # make Ba column and set all values to 1

DF.B <- rbind(DF.BA, DF.BN)

# Calculate Dn0/d ratios!
DF.B$Bn_Ba <- DF.B$Bn / DF.B$Ba   # have ratios up to 10 -  log transform in plots!

## Export DF.Y
write.csv(DF.B, "H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.B.V2.Baseline-Null.csv")
DF.B <- read.csv("H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.B.V2.Baseline-Null.csv", sep=",")

# Aggregate  by Wstart (salmon size)
# summarize data for barplot
bar.cp<-aggregate(p.tot ~ h + Bn_Ba, data=subset(DF.B, Beh == 0), mean)
a<-aggregate(p.tot ~ h + Bn_Ba, data=subset(DF.B, Beh == 0), sd); colnames(a)[3]<-"sd"
b<-aggregate(p.tot ~ h + Bn_Ba, data=subset(DF.B, Beh == 0), length); colnames(b)[3]<-"n"
bar.cp<-join(bar.cp, a); bar.cp<-join(bar.cp, b)
bar.cp$se<-bar.cp$sd / sqrt(bar.cp$n)

bar.cp$h<-as.factor(bar.cp$h)
levels(bar.cp$h) <- c("Altered", "Natural")


# Plots!
plot_Bn_Ba <- ggplot(data=bar.cp, aes(x=log(Bn_Ba), y=p.tot, fill=h, color=h)) + 
  geom_vline(xintercept = 0, linetype="dashed",  color = "gray24", size=0.5) +
  geom_vline(xintercept = log(0.7/1), linetype="dashed",  color = "skyblue", size=0.5) +
  geom_line(size=0.5, aes(color=h)) +
  geom_errorbar(aes(ymax=p.tot + se, ymin=p.tot - se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") + 
  theme_classic() + ylim(c(0,1)) +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab("Frequency of move 0") + xlab(expression(paste(ln(B[n]:B[a])))) +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.8, 0.8), legend.background = element_rect(fill="transparent")) +
  annotate(geom="text", x=-1.3, y=1, label="greater escape\nability in natural", color="forestgreen", fontface="bold") +
  annotate(geom="text", x=1.3, y=1, label="greater escape\nability in altered", color="mediumpurple", fontface="bold") +
  annotate(geom="text", x=log(0.7/1)-0.5, y=0.8, label="baseline", color="skyblue", fontface="bold")

plot_Bn_Ba

setwd("C:/Users/Megan/Desktop/")
pdf("Fig_Bn_Ba.pdf", width=4.5, height=4)

plot_Bn_Ba

dev.off()
