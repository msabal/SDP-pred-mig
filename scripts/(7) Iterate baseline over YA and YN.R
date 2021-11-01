#### Iterate baseline parameters over ya and yn

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

#### Iterate Main Function over varying parameters
yn <- 1

ya <- seq(0.1,1, by=0.2)

OUT.YA <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(ya)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                  qa, qn, ya[i], yn, yo, dn0, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur")
  
  OUT$ya <- rep(ya[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.YA[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 53 mins

DF.YA<-ldply(OUT.YA, as.vector)

## Export DF.QA
write.csv(DF.YA, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.YA.csv")



#### Iterate Main Function over varying parameters
ya <- 1

yn <- seq(0.1,0.9, by=0.2)

OUT.YN <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(yn)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                  qa, qn, ya, yn[i], yo, dn0, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur")
  
  OUT$yn <- rep(yn[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.YN[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 1.83 hours for seeds length = 10!

DF.YN<-ldply(OUT.YN, as.vector)

## Export DF.QA
write.csv(DF.YN, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.YN.csv")


# Join DF.YA and DF.YN

DF.YA$yn <- rep(1, length(DF.YA$Wstart)) # make yn column and set all values to 1
DF.YN$ya <- rep(1, length(DF.YN$Wstart)) # make ya column and set all values to 1

DF.Y <- rbind(DF.YA, DF.YN)

# Calculate Dn0/d ratios!
DF.Y$yn_ya <- DF.Y$yn / DF.Y$ya   # have ratios up to 10 -  log transform in plots!

## Export DF.Y
write.csv(DF.Y, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.Y.csv")


# Aggregate  by Wstart (salmon size)
# summarize data for barplot
bar.cp<-aggregate(p.tot ~ h + yn_ya, data=subset(DF.Y, Beh == 0), mean)
a<-aggregate(p.tot ~ h + yn_ya, data=subset(DF.Y, Beh == 0), sd); colnames(a)[3]<-"sd"
b<-aggregate(p.tot ~ h + yn_ya, data=subset(DF.Y, Beh == 0), length); colnames(b)[3]<-"n"
bar.cp<-join(bar.cp, a); bar.cp<-join(bar.cp, b)
bar.cp$se<-bar.cp$sd / sqrt(bar.cp$n)

bar.cp$h<-as.factor(bar.cp$h)

# Plots!
plot_yn_ya <- ggplot(data=bar.cp, aes(x=log(yn_ya), y=p.tot, fill=h)) + 
  geom_vline(xintercept = 0, linetype="dashed",  color = "gray24", size=0.5) +
  geom_line(size=0.5, aes(color=h)) +
  geom_errorbar(aes(ymax=p.tot + se, ymin=p.tot - se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21) + 
  theme_classic() + ylim(c(0,1)) +
  scale_color_manual(values=c("mediumpurple", "forestgreen"), labels = element_blank()) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen"), labels = c("Altered", "Natural")) +
  ylab("Frequency of move 0") + xlab(expression(paste(d[n0]:d))) +
  theme(axis.text.y = element_text(size=13), axis.text.x = element_text(size=13),
        axis.title.y = element_text(size=13), axis.title.x = element_text(size=13),
        legend.title = element_blank(), legend.text = element_text(size=13)) +
  theme(legend.position = c(0.75, 0.75))

plot_yn_ya

setwd("C:/Users/Megan/Desktop/")
pdf("Fig_yn_ya.pdf", width=4, height=4)

plot_yn_ya

dev.off()

