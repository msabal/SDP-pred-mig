#### Iterate baseline parameters over d and dn0

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

#### Iterate Main Function over varying parameters
d <- 1

dn0 <- seq(0.1,1, by=0.2)

OUT.DN0 <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(dn0)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, # vars in functions
                  qa, qn, ya, yn, yo, dn0[i], Ba, Bn, Bo, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$dn0 <- rep(dn0[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.DN0[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 53 mins

DF.DN0<-ldply(OUT.DN0, as.vector)

## Export DF.QA
write.csv(DF.DN0, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.DN0.csv")



#### Iterate Main Function over varying parameters
dn0 <- 1

d <- seq(0.1,0.9, by=0.2)

OUT.D <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(d)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d[i], v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                  qa, qn, ya, yn, yo, dn0, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$d <- rep(d[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.D[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 1.83 hours for seeds length = 10!

DF.D<-ldply(OUT.D, as.vector)

## Export DF.QA
write.csv(DF.D, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.D.csv")


# Join DF.DN0 and DF.D

DF.DN0$d <- rep(1, length(DF.DN0$Wstart)) # make qn column and set all values to 1
DF.D$dn0 <- rep(1, length(DF.D$Wstart)) # make qa column and set all values to 1

DF.D <- rbind(DF.DN0, DF.D)

# Calculate Dn0/d ratios!
DF.D$dn0_d <- DF.D$dn0 / DF.D$d   # have ratios up to 10!!!! CHECK THIS!!!!!!!!!!!!!!

## Export DF.D
write.csv(DF.D, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.D.csv")
DF.D <- read.csv("C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.D.csv", sep=",")

# Aggregate  by Wstart (salmon size)
# summarize data for barplot
bar.cp<-aggregate(p.tot ~ h + dn0_d, data=subset(DF.D, Beh == 0), mean)
a<-aggregate(p.tot ~ h + dn0_d, data=subset(DF.D, Beh == 0), sd); colnames(a)[3]<-"sd"
b<-aggregate(p.tot ~ h + dn0_d, data=subset(DF.D, Beh == 0), length); colnames(b)[3]<-"n"
bar.cp<-join(bar.cp, a); bar.cp<-join(bar.cp, b)
bar.cp$se<-bar.cp$sd / sqrt(bar.cp$n)

bar.cp$h<-as.factor(bar.cp$h)
levels(bar.cp$h) <- c("Altered", "Natural")

# Plots!
plot_dn0.d <- ggplot(data=bar.cp, aes(x=log(dn0_d), y=p.tot, fill=h, color=h)) + 
  geom_vline(xintercept = 0, linetype="dashed",  color = "gray24", size=0.5) +
  geom_vline(xintercept = log(0.7/1), linetype="dashed",  color = "skyblue", size=0.5) +
  geom_line(size=0.5, aes(color=h)) +
  geom_errorbar(aes(ymax=p.tot + se, ymin=p.tot - se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") + 
  theme_classic() + ylim(c(0,1)) +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab("Frequency of move 0") + xlab(expression(paste(ln(d[n0]:d)))) +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.8, 0.8), legend.background = element_rect(fill="transparent")) +
  annotate(geom="text", x=-1.3, y=1, label="greater flow\nrefugia in natural", color="forestgreen", fontface="bold") +
  annotate(geom="text", x=1.3, y=1, label="greater flow\nrefugia in altered", color="mediumpurple", fontface="bold") +
  annotate(geom="text", x=log(0.7/1)-0.5, y=0.8, label="baseline", color="skyblue", fontface="bold")


plot_dn0.d

setwd("C:/Users/Megan/Desktop/")
pdf("Fig_Dn0_D.pdf", width=4.5, height=4)

plot_dn0.d

dev.off()


