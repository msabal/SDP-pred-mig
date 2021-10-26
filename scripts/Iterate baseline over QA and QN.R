#### Iterate baseline parameters over qa and qn

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

#### Iterate Main Function over varying parameters
qn <- 1

qa <- seq(0.5,1, by=0.1)

OUT.QA <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(qa)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                  qa[i], qn, ya, yn, yo, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec)
  
  colnames(OUT) <- c("Wstart", "S.cum.riv", "G.riv", "G.ocean", "dur", "p0.n", "p1.n", "p2.n", "p0.a", "p1.a"," p2.a")
  
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
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                  qa, qn[i], ya, yn, yo, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec)
  
  colnames(OUT) <- c("Wstart", "S.cum.riv", "G.riv", "G.ocean", "dur", "p0.n", "p1.n", "p2.n", "p0.a", "p1.a"," p2.a")
  
  OUT$qn <- rep(qn[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.QN[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 1.83 hours for seeds length = 10!

DF.QN<-ldply(OUT.QN, as.vector)

## Export DF.QA
write.csv(DF.QN, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.QN.csv")


# Join DF.QA and DF.QN 

DF.QA$qn <- rep(1, length(DF.QA$Wstart)) # make qn column and set all values to 1
DF.QN$qa <- rep(1, length(DF.QN$Wstart)) # make qa column and set all values to 1

DF.Q <- rbind(DF.QA, DF.QN)

# Calculate Qn/Qa ratios!
DF.Q$qn_qa <- DF.Q$qn / DF.Q$qa


# Plots!

ggplot(data=DF.Q, aes(x=qn_qa, y=p0.a, color=Wstart)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))
ggplot(data=DF.Q, aes(x=qn_qa, y=p0.n, color=Wstart)) + geom_point(size=3) + theme_classic() + ylim(c(0,1))
