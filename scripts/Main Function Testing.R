
# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)


#### Parameters

# seeds for h.vec
seeds <- 1 # can change

# W: salmon weight (g)
Wmin <- 7
Wmax <- 50
Wstep <- 0.1 
Wstep.n <- ((Wmax-Wmin)/Wstep)

# A: salmon area
Amin <- 1
Amax <- 26

# t: time
tmin <- 1
tmax <- 60
# Behavioral choice
U <- c(0, 1, 2)

# Terminal fitness
Ws    <- 40
r     <- 0.1
Smax  <- 0.3

# Growth
E     <- 0.04
qa    <- 0.7 #can change
qn    <- 1    # can change
a     <- 0.86
Alpha <- 0.00607
d     <- 1
dn0   <- 0.7
v     <- 0.027
f     <- 0.5
g     <- 2
c     <- 40
j     <- 0.07

# Risk
Bu    <- c(0.7, 1, 0.7) # B0, B1, B2 (can concatenate because we will loop over behavior choices?)
Ba    <- 1
Bn    <- 0.7 #can change
Bo    <- 1
Bw    <- 2
M     <- 0.002
m     <- -0.37
ya    <- 1  #can change
yn    <- 1  #can change
yo    <- 1  #can change
P     <- 20


# Check Main Function works.

OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
         E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
         qa, qn, ya, yn, yo, # vars that vary by habitat (h.vec)
         Ws, r, Smax, W, # vars for Terminal fitness function
         Wstep.n, Wstep, tmax, seeds, F.vec)

OUT

colnames(OUT) <- c("Wstart", "S.cum.riv", "G.riv", "G.ocean", "dur", "p0.n", "p1.n", "p2.n", "p0.a", "p1.a"," p2.a")



#### Iterate Main Function over varying parameters

seeds <- seq(1,10, by=1)

OUT.SEEDS <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different h.vec seed values (seeds)
for(i in 1:length(seeds)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                     E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                     qa, qn, ya, yn, yo, # vars that vary by habitat (h.vec)
                     Ws, r, Smax, W, # vars for Terminal fitness function
                     Wstep.n, Wstep, tmax, seeds[i], F.vec)

  colnames(OUT) <- c("Wstart", "S.cum.riv", "G.riv", "G.ocean", "dur", "p0.n", "p1.n", "p2.n", "p0.a", "p1.a"," p2.a")
  
  OUT$seeds <- rep(seeds[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.SEEDS[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 1.83 hours for seeds length = 10!

DF.10SEEDS<-ldply(OUT.SEEDS, as.vector)

## Export DF.SEEDS for Figures 1 and 2!
write.csv(DF.10SEEDS, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.10SEEDS.FIGS1&2.csv")


## Summarize DF.10SEEDS
mean(DF.10SEEDS$dur)
sd(DF.10SEEDS$dur)

mean(DF.10SEEDS$S.cum.riv)
sd(DF.10SEEDS$S.cum.riv)


DF.10SEEDS$mean.G.riv <- DF.10SEEDS$G.riv / DF.10SEEDS$dur
mean(DF.10SEEDS$mean.G.riv)
sd(DF.10SEEDS$mean.G.riv)

DF.10SEEDS$mean.G.ocean <- DF.10SEEDS$G.ocean / (60 - DF.10SEEDS$dur)
mean(DF.10SEEDS$mean.G.ocean)
sd(DF.10SEEDS$mean.G.ocean)

