
##### DYNAMIC PROGRAM: juvenile salmon out-migration

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)




#### Set minimum and maximum values and steps for all state variables

# W: salmon weight (g)
Wmin <- 7
Wmax <- 80 # make this small to start? Eventually want 80 or 100 g...
Wstep <- 0.1 # discrete interval steps maybe try 0.05
Wstep.n <- ((Wmax-Wmin)/Wstep)  # this results in a lot of discrete steps - yikes!
                      # Try linear interpolation? (See Clark & Mangel Ch 2)

# A: salmon area
Amin <- 1
Amax <- 26

# t: time
tmin <- 1
tmax <- 60




#### Simulate river habitats by area
h.vec <- rep(NA, Amax) # create blank vector for habitats for each Area.

h.vec[Amax] <- "o" # make the last area (Amax) the ocean: "o"

h.vec[1:Amax-1] <- sample(0:1, Amax-1, replace=T, prob=c(0.5,0.5))  # randomly sample
        # Amax-1 number of values 0 or 1 with a 50% probability between the two values.

h.vec[h.vec == "1"] <- "a"  # change 1 from sample function to "a"
h.vec[h.vec == "0"] <- "n"  # change 0 from sample function to "n"




#### Make empty objects to store eventual outputs from dynamic programming equations

# F.all <- an array that stores the final expected fitness (probability of surviving to a
#          returning adult) for each salmon weight (columns), time (rows), for each area (matrices)

F.all <- array(NA, dim=c(tmax, Wstep.n, Amax))  #(rows: time, cols: weight, matrices: area)

# Best.beh <- an array with the same structure as F.all. This is the decision matrix. Stored values
#             will be the best decision chosen (move 0, 1, 2 areas).

Best.beh <- array(NA, dim=c(tmax, Wstep.n, Amax))  #(rows: time, cols: weight, matrices: area)

# Surv.day <- an array with the same structure as F.all. This is the survival matrix. Stored values
#             will be the daily survival (equation 4).

Surv.day <- array(NA, dim=c(tmax, Wstep.n, Amax))  #(rows: time, cols: weight, matrices: area)

# F.vec <- an array to hold the expected fitness at time t+1 (2nd column, value used in calculations), and
#         at time t (1st column, resulting value from current calculation, which will be used in the NEXT
#         iteration).

F.vec <- array(NA, dim=c(Wstep.n, 2, Amax))  #(rows: weight, cols: F(x,t), F(x, t+1), matrices: area)




#### Terminal fitness function - how does salmon weight at tmax and Amax relate to probability of
#    surviving to a returning adult? Larger salmon more likely to survive to a returning adult.
#    Sigmoidal function that asymptotes at Smax.

#ONLY applicable for salmon at A 26 at tmax!!!! Otherwise, fitness is 0!
TERM.FUN <- function(W, Ws, r, Smax){ Smax/(1+exp(-r*(W-Ws))) }

# Terminal fitness parameters
Ws <- 40
r <- 0.1
Smax <- 0.3

# plot Terminal Fitness function
curve(TERM.FUN(W, Ws=Ws , r=r, Smax=Smax), xname="W", xlim=c(7,80), ylim=c(0,0.31), ylab="adult marine survival (to age 3)")




#### Fill in last column (tmax) for F.vec based on Terminal fitness function
F.vec[1:Wstep.n, 2, Amax] <- TERM.FUN(W = seq(Wmin+Wstep, Wmax , Wstep),
                                              Ws=Ws, r=r, Smax=Smax)

F.vec[,2,1:Amax-1] <- 0    #if salmon end in any area besides the last (Amax), then their fitness is 0!

View(F.vec[,,Amax]) #check and see terminal values at Amax (should all be values)
View(F.vec[,,1]) #check and see terminal values at Area 1 (should all be 0s)




#### Fitness function - function to calculate fitness at any salmon weight,area,
          # and behavior (movement choice). Use state dynamics to calculate new
          # W(t+1) and new A(t+1). Calculate expected fitness for resulting new
          # states from 2nd column of F.vec. Returns Fit (single value).

# Functions used inside the Fitness function (see Final State Dynamics.R for more details)
GROWTH.FUN <- function(W, E, q, a, Alpha, d, v, U)    { q*E*W^a - d*Alpha*W*exp(v*U) }
OCEAN.Q <-    function(t, f, g, c, j)             { f + g*exp(-(t-c)^2/2*j^2) }
RISK.FUN <-   function(W, Bu, Bh, Bw, M, m, y, P) { (1-M*(Bu + Bh + Bw*W^m))^(y*P) }


# Fitness function (described above)
FITNESS <- function(W, E, q, a, Alpha, d, v, U, t, f, g, c, j, Bu, Bh, Bw, M, m, y, P,
                    Wmax, Amax, h.vec, F.vec) {
  Wnew <- W + GROWTH.FUN(W=W, E=E, a=a, v=v, U=U, Alpha= Alpha,
                         d=d,
                         q= ifelse(h.vec[h.vec] == "o", OCEAN.Q(t, f, g, c, j), q))
  Wnew <- ifelse(A < Amax, )
  Anew <-
  Anew <-
    
  Fit <- ifelse(Wnew > Wmin, RISK.FUN(W, Bu, Bh, Bw, M, m, y, P)*F.vec[Wnew, 2, Anew])
  
  return(Fit)
  
}

sim.mix[t+1,2]<-GROWTH.FUN2(X = sim.mix[t,2], a=0.86, 
                            q= ifelse(sim.mix[t,3] == "o", OCEAN.Q(t=sim.mix[t,1], a=0.07, b=40, c=0.07, d=0.02),
                                      ifelse(sim.mix[t,3] == "a", 0.02, 0.04)), 
                            A= ifelse(sim.mix[t,3] == "n" & sim.mix[t,4] == 0, 0.00407, 0.00607), 
                            v=0.027, U=sim.mix[t,4])


# Fitness function (described above)
FITNESS <- function(B, X, Xmax, L, Lmax, e, e10, c, b, F.vec){
  Xnew <- ifelse(L < 10, X + e - c, X + e10 - c) # calculate new state: energy reserves. If in L 10 than salmon gains MORE energy.
  Xnew <- min(Xnew, Xmax)
  Lnew <- L + b      # calculate new state: location
  Lnew <- min(Lnew, Lmax)
  
  W <- ifelse(Xnew > Xcrit, (1-B)*F.vec[Xnew, 2, Lnew], 0) # calculate fitness: prob of survive predation times future fitness of RESULTING state and location.
  # if X is less than 1, than fitness is 0 no matter what.
  return(W)
}

# check fitness function works.
# FITNESS(B[1], X=7, Xmax, L=10, Lmax, e[1], e10, c[1], b[1], F.vec)
