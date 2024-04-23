#### Iterate baseline parameters over qa and qn

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

#### Iterate Main Function over varying parameters
qn <- 1

qa <- seq(0.5, 1, by=0.1)

OUT.QA <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(qa)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, # vars in functions
                  qa[i], qn, ya, yn, yo, dn0, Ba, Bn, Bo, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$qa <- rep(qa[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.QA[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 59 mins

DF.QA<-ldply(OUT.QA, as.vector)

## Export DF.QA
write.csv(DF.QA, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.QA.csv")



#### Iterate Main Function over varying parameters
qa <- 1

qn <- seq(0.5,0.9, by=0.1)

OUT.QN <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(qn)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, # vars in functions
                  qa, qn[i], ya, yn, yo, dn0, Ba, Bn, Bo, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$qn <- rep(qn[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.QN[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 1.83 hours for seeds length = 10!

DF.QN<-ldply(OUT.QN, as.vector)

## Export DF.QN
write.csv(DF.QN, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.QN.csv")

# Join DF.QA and DF.QN 

DF.QA$qn <- rep(1, length(DF.QA$Wstart)) # make qn column and set all values to 1
DF.QN$qa <- rep(1, length(DF.QN$Wstart)) # make qa column and set all values to 1

DF.Q <- rbind(DF.QA, DF.QN)

# Calculate Qn/Qa ratios!
DF.Q$qn_qa <- DF.Q$qn / DF.Q$qa


## Export DF.Q
write.csv(DF.Q, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.Q.csv")
DF.Q <- read.csv("C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.Q.csv", sep=",")

# Aggregate  by Wstart (salmon size)
# summarize data for barplot
bar.cp<-aggregate(p.tot ~ h + qn_qa, data=subset(DF.Q, Beh == 0), mean)
a<-aggregate(p.tot ~ h + qn_qa, data=subset(DF.Q, Beh == 0), sd); colnames(a)[3]<-"sd"
b<-aggregate(p.tot ~ h + qn_qa, data=subset(DF.Q, Beh == 0), length); colnames(b)[3]<-"n"
bar.cp<-join(bar.cp, a); bar.cp<-join(bar.cp, b)
bar.cp$se<-bar.cp$sd / sqrt(bar.cp$n)

bar.cp$h<-as.factor(bar.cp$h)
levels(bar.cp$h) <- c("Altered", "Natural")


# Plots!

plot_qn_qa <- ggplot(data=bar.cp, aes(x=log(qn_qa), y=p.tot, fill=h, color=h)) + 
  geom_vline(xintercept = 0, linetype="dashed",  color = "gray24", size=0.5) +
  geom_vline(xintercept = log(1/0.7), linetype="dashed",  color = "skyblue", size=0.5) +
  geom_line(size=0.5, aes(color=h)) +
  geom_errorbar(aes(ymax=p.tot + se, ymin=p.tot - se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") + 
  theme_classic() + ylim(c(0,1)) +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab("Frequency of move 0") + xlab(expression(paste(ln(q[n]:q[a])))) +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.8, 0.8), legend.background = element_rect(fill="transparent")) +
  annotate(geom="text", x=-0.4, y=1, label="less food\nin natural", color="forestgreen", fontface="bold") +
  annotate(geom="text", x=0.4, y=1, label="less food\nin altered", color="mediumpurple", fontface="bold") +
  annotate(geom="text", x=log(1/0.7)+0.2, y=0.5, label="baseline", color="skyblue", fontface="bold")


plot_qn_qa

setwd("C:/Users/Megan/Desktop/")
pdf("Fig_Qa_Qn.pdf", width=4.5, height=4)

plot_qn_qa

dev.off()

## Other plots!
ggplot(data=DF.Q, aes(x=qn_qa, y=p0.a, color=Wstart)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))
ggplot(data=DF.Q, aes(x=qn_qa, y=p0.n, color=Wstart)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))

ggplot(data=subset(DF.Q.LONG, Beh == 0 & Wstart == 10), aes(x=qn_qa, y=p, color=h)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))
ggplot(data=subset(DF.Q.LONG, Beh == 1 & Wstart == 10), aes(x=qn_qa, y=p, color=h)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))
ggplot(data=subset(DF.Q.LONG, Beh == 2 & Wstart == 10), aes(x=qn_qa, y=p, color=h)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))


