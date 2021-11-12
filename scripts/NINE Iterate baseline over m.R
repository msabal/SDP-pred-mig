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
                  Wstep.n, Wstep, tmax, seeds, F.vec)
  
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

