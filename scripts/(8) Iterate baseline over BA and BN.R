#### Iterate baseline parameters over Ba and Bn

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

#### Iterate Main Function over varying parameters
Bn <- 1

Ba <- seq(0.1,1, by=0.2)

OUT.Ba <- list()

start.time <- Sys.time() # time how long the while loop takes


### STOP HERE! NEED TO SWAP Bh to Bn and Ba THROUGHOUT ALL FUNCTIONS AND CODE!!!!


# Start looping MAIN_FUN over different qa values
for(i in 1:length(Ba)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                  qa, qn, ya, yn, yo, dn0, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur")
  
  OUT$Ba <- rep(Ba[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.Ba[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 53 mins

DF.Ba<-ldply(OUT.Ba, as.vector)

## Export DF.QA
write.csv(DF.Ba, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.Ba.csv")



#### Iterate Main Function over varying parameters
Ba <- 1

Bn <- seq(0.1,0.9, by=0.2)

OUT.Bn <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(Bn)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                  qa, qn, Ba, Bn[i], yo, dn0, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur")
  
  OUT$Bn <- rep(Bn[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.Bn[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 1.83 hours for seeds length = 10!

DF.Bn<-ldply(OUT.Bn, as.vector)

## Export DF.QA
write.csv(DF.Bn, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.Bn.csv")


# Join DF.Ba and DF.Bn

DF.Ba$Bn <- rep(1, length(DF.Ba$Wstart)) # make Bn column and set all values to 1
DF.Bn$Ba <- rep(1, length(DF.Bn$Wstart)) # make Ba column and set all values to 1

DF.Y <- rbind(DF.Ba, DF.Bn)

# Calculate Dn0/d ratios!
DF.Y$Bn_Ba <- DF.Y$Bn / DF.Y$Ba   # have ratios up to 10 -  log transform in plots!

## Export DF.Y
write.csv(DF.Y, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.Y.csv")


# Aggregate  by Wstart (salmon size)
# summarize data for barplot
bar.cp<-aggregate(p.tot ~ h + Bn_Ba, data=subset(DF.Y, Beh == 0), mean)
a<-aggregate(p.tot ~ h + Bn_Ba, data=subset(DF.Y, Beh == 0), sd); colnames(a)[3]<-"sd"
b<-aggregate(p.tot ~ h + Bn_Ba, data=subset(DF.Y, Beh == 0), length); colnames(b)[3]<-"n"
bar.cp<-join(bar.cp, a); bar.cp<-join(bar.cp, b)
bar.cp$se<-bar.cp$sd / sqrt(bar.cp$n)

bar.cp$h<-as.factor(bar.cp$h)

# Plots!
plot_Bn_Ba <- ggplot(data=bar.cp, aes(x=log(Bn_Ba), y=p.tot, fill=h)) + 
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

plot_Bn_Ba

setwd("C:/Users/Megan/Desktop/")
pdf("Fig_Bn_Ba.pdf", width=4, height=4)

plot_Bn_Ba

dev.off()
