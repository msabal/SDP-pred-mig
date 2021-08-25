
##### PRELIMINARY RESULTS: Using the programming output.

# load libraries
library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)



# load all output data.
pdat <- read.csv("C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\All.program.output.csv", header = T, sep=",")
pdat$X <- NULL # remove generated X column.

summary(pdat) # check column class types.

pdat$Best.beh <- as.factor(pdat$Best.beh) # make Best.beh a factor.


# unsplit all output back into separate output arrays

#daply(pdat, [, ])

acast(pdat, t ~ A, value.var = Best.beh)




#### FORWARD SIMULATIONS ####
# create subset of salmon starting computer weights (Wc) to simulate tracks for
Ws <- c(7.1, 10, 15, 20) # pick Starting weights (g) to simulate

# convert Ws from W to Wc
Ws.c <- WtoWc(Ws)

tmax <- 60

# create output objects to store for A
output.A <- matrix(NA, tmax, length(Ws))  # rows: time, columns: weight (W). Output to track best behaviors over time for each state.
output.A[1,] <- 1 # salmon start in area 1

A <- 1
t <- 2
Wc <- 


for(Wc in 1:length(Ws)){                    # iterate over chosen starting salmon weights
  for(t in 1:(tmax-1)){                    # iterate over time
    A <- output.A[t,Wc]                     # current area is the value in the current row of time (t)
    Anew <- A + pdat[pdat$t == t & pdat$Wc == Wc & pdat$A == A,4]
      
      Best.beh[t,X,A]            # area in next time step is the current location plus how much they move (Best.beh)
    Anew <- min(Anew, Amax)                # area cannot be greater than Amax 
    output.A[t+1,X] <- Anew                # store new area in the next row (t+1)
    
    output.S[t,X] <- Surv.day[t,X,A]       #  get appro daily survival from Surv.day and save it in output.S
    output.Fit[t,X] <- F.all[t,X,A]        #  get appro expected fitness from F.all and save it in output.Fit
    output.beh[t,X] <- Best.beh[t,X,A]     #  get appro best beh from Best.beh and save it in output.beh
    
  }} # end for loops.




# create output objects to store outcomes from for loop
output.A <- matrix(NA, tmax, length(sim.sam.Wc))  # rows: time, columns: weight (W). Output to track best behaviors over time for each state.
output.A[1,] <- 1 # salmon start in area 1

output.S <- matrix(NA, tmax, length(sim.sam.Wc)) # output to store daily Surv
output.Fit <- matrix(NA, tmax, length(sim.sam.Wc)) # output to store expected Fitness
output.beh <- matrix(NA, tmax, length(sim.sam.Wc)) # output to store best behavior

# for loop tracking new areas for individuals starting at A = 1, and my chosen salmon weights (W in sim.sam).

for(X in 1:length(sim.sam.Wc)){            # iterate over chosen starting salmon weights
  for(t in 1:(tmax-1)){                    # iterate over time
    A <- output.A[t,X]                     # current area is the value in the current row of time (t)
    Anew <- A + Best.beh[t,X,A]            # area in next time step is the current location plus how much they move (Best.beh)
    Anew <- min(Anew, Amax)                # area cannot be greater than Amax 
    output.A[t+1,X] <- Anew                # store new area in the next row (t+1)
    
    output.S[t,X] <- Surv.day[t,X,A]       #  get appro daily survival from Surv.day and save it in output.S
    output.Fit[t,X] <- F.all[t,X,A]        #  get appro expected fitness from F.all and save it in output.Fit
    output.beh[t,X] <- Best.beh[t,X,A]     #  get appro best beh from Best.beh and save it in output.beh
    
  }} # end for loops.

