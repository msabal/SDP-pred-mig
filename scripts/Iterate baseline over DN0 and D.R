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
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                  qa, qn, ya, yn, yo, dn0[i], # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur")
  
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
                  Wstep.n, Wstep, tmax, seeds, F.vec)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur")
  
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

DF.QA$qn <- rep(1, length(DF.QA$Wstart)) # make qn column and set all values to 1
DF.QN$qa <- rep(1, length(DF.QN$Wstart)) # make qa column and set all values to 1

DF.D <- rbind(DF.DN0, DF.D)

# Calculate Dn0/d ratios!
DF.D$dn0_d <- DF.D$dn0 / DF.Q$d

##UPDATE THIS!
# Melt dataframe to get into long format
df.l.beh <- DF.Q[,c("Wstart", "qn_qa", "p0.n", "p1.n", "p2.n")]
df.l.beh <- melt(df.l.beh, variable.name = "Beh", value.name = "p", id.vars = c("Wstart", "qn_qa"))
levels(df.l.beh$Beh) <- c("0", "1", "2")
df.l.beh$h <- rep("n", length(df.l.beh$Wstart))

# by habitat and movement choice: altered
df.l.beh1 <- DF.Q[,c(1,14,9,10,11)]
df.l.beh1 <- melt(df.l.beh1, variable.name = "Beh", value.name = "p", id.vars = c("Wstart", "qn_qa"))
levels(df.l.beh1$Beh) <- c("0", "1", "2")
df.l.beh1$h <- rep("a", length(df.l.beh1$Wstart))

DF.Q.LONG <- rbind(df.l.beh, df.l.beh1)



# Plots!

ggplot(data=DF.Q, aes(x=qn_qa, y=p0.a, color=Wstart)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))
ggplot(data=DF.Q, aes(x=qn_qa, y=p0.n, color=Wstart)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))

ggplot(data=subset(DF.Q.LONG, Beh == 0 & Wstart == 10), aes(x=qn_qa, y=p, color=h)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))
ggplot(data=subset(DF.Q.LONG, Beh == 1 & Wstart == 10), aes(x=qn_qa, y=p, color=h)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))
ggplot(data=subset(DF.Q.LONG, Beh == 2 & Wstart == 10), aes(x=qn_qa, y=p, color=h)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))
