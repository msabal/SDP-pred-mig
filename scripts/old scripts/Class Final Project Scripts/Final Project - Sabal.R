
#### SABAL FINAL PROJECT ####

### How do migrating salmon change speed (via antipredator behavior) in response to predation risk?


# load libraries
library(abind); library(ggplot2); library(plyr)

#### Make empty objects to store eventual outputs from dynamic programming equations

# F.all <- an array that stores the final (including terminal) fitness (probability of surviving to a
#          returning adult) for each state (columns), time (rows), for each location (matricies)

F.all <- array(NA, dim=c(30,10,10))  #(rows: time, cols: state, matricies: location)


# Best.beh <- an array with the same structure as F.all. This is the decision matrix. Stored values
#             will be the best decision chosen (move 0, 1, 2 locations).

Best.beh <- array(NA, dim=c(30,10,10))  #(rows: time, cols: state, matricies: location)


# F.vec <- an array to hold the total fitness at time t+1 (2nd column, value used in calculations), and
#         at time t (1st column, resulting value from current calculation, which will be used in the NEXT
#         iteration).

F.vec <- array(NA, dim=c(10,2,10))  #(rows: state, cols: F(x,t), F(x, t+1), matricies: location)



#### Terminal fitness function - how do energy reserves at time T and location 10 relate to probability of
#    surving to a returning adult? More final energy reserves, more likely to survive to adult.
#    Terminal fitness can only range between 0 and 1.

#ONLY applicable if location is 10 at time T!!!! Otherwise, fitness is 0!
T.FITNESS <- function(X, m){  1/(1+exp(-X+m)) }
curve(T.FITNESS(X, m=5), xlim=c(0, 10), ylab="probability of surviving to adult",
      xlab="X(T, L10), energy reserves at time T in last location", main="Terminal Fitness Function",
      xname = "X")


### Fill in last column (T) for of F.vec based on Terminal fitness function
F.vec[,2,10] <- T.FITNESS(X = seq(1,10,1), m=5)  #if salmon make it to location 10, then their fitness depends on their energy reserves.

F.vec[,2,1:9] <- 0    #if salmon end in any location besides the last (10), then their fitness is 0!



#### Fitness function - function to calculate fitness at any state, location, and behavior.

FITNESS <- function(B, X, Xmax, L, Lmax, e, c, b, F.vec){
  Xnew <- X + e - c # calculate new state: energy reserves. If in L 10 than salmon gains MORE energy.
  Xnew <- min(Xnew, Xmax)
  Lnew <- L + b      # calculate new state: location
  Lnew <- min(Lnew, Lmax)
  
  W <- ifelse(Xnew > 0, (1-B)*F.vec[Xnew, 2, Lnew], 0) # calculate fitness: prob of survive predation times future fitness of RESULTING state and location.
                                                       # if X is less than 1, than fitness is 0 no matter what.
  return(W)
  }

# check fitness function works.
# FITNESS(B[1], X=1, Xmax, L=5, Lmax, e[1], c[1], b[1], F.vec)


#### Function to get maximum fitness and best behavioral choice for any state and location. Save in Temporary array (Temp.out) to bring to next function/loop.
OVER.BEH <- function(B, X, Xmax, L, Lmax, e, c, b, F.vec){
  W.behs <- matrix(NA, 3, 1)  #matrix to save fitnesses (W) from each (3) behavioral choice.
  
  for(i in 1:length(b)){ W.behs[i,] <- FITNESS(B[i], X, Xmax, L, Lmax, e[i], c[i], b[i], F.vec) }   #where does x come from?
  
  W.best <- max(W.behs)  # store highest fitness in F.vec for next iteration.
  Temp <- data.frame(W = W.behs, beh = seq(0,2,1))  #make temp dataframe with each fitness (W) for each behavior.
  Beh.best <- ifelse(W.best > 0, Temp[Temp$W == max(Temp$W),2], NA)  #get single value of best behavior choice (0,1,2). If W.best is 0 for all behavioral types, than no beh is best, return NA.
  F.vec[X,1,L] <- W.best  # save best fitness in the appropriate F.vec column 1 for that specific state X and L.
  
  Temp.out <- array(NA, dim=c(11,2,10)) #(rows: state, cols: F(x,t), F(x, t+1), matricies: location). Same size as F.vec, but add 1 row to save best W and beh.
  Temp.out[11,,] <- c(W.best, Beh.best)
  Temp.out[1:10,,] <- F.vec  # Temp.out needs ALL the info that will be used in the next function/loop. 
                             # F.vec AND the best W and best beh. Will split up later to use in different ways in the next function OVER.STATES.
  return(Temp.out)
  }


# #Check if for loop inside OVER.BEH works...Yes!
#   W.behs <- matrix(NA, 3, 1)   #vector to save fitnesses (W) from each (3) behavioral choice.
#   for(i in 1:3){ W.behs[i,] <- FITNESS(B[i], X=8, Xmax=10, L=8, Lmax=10, e[i], c[i], b[i], F.vec) }

# # see if OVER.BEH works for a specific X and L state for T-1. Yes! And F.vec column 1 is updated with new value for next iteration!!!
# OVER.BEH(B, X=8, Xmax=10, L=8, Lmax=10, e, c, b, F.vec) # see output of OVER.BEH function.
# OVER.BEH(B, X=2, Xmax=10, L=10, Lmax=10, e, c, b, F.vec) # see output of OVER.BEH function.
# OVER.BEH(B, X=1, Xmax=10, L=1, Lmax=10, e, c, b, F.vec) # Common to have problems when X=1...use this to check.

  

#### Function to iterate over both states X and L. Output is an array where first two columns is updated F.vec, third col is best W, fourth col is best Beh.

OVER.STATES <- function(B, X, Xmax, L, Lmax, e, c, b, F.vec){
Store <- array(NA, dim=c(10,2,10))  # array to store all W.best (column 1) and Beh.best (column 2) values from OVER.BEH for each state X (rows) and L (matricies).

for(X in 1:Xmax){  # iterate over states X
  for(L in 1:Lmax){ # iterate over states L
    Temp.out <- OVER.BEH(B, X, Xmax, L, Lmax, e, c, b, F.vec) # returns Temp.out from OVER.BEH, which as the updated F.vec AND W.best and Beh.best in the last row.
    
    n <- nrow(Temp.out) - 1  # get number of states by subtracting the last row to store W.best and Beh.best (this should be 10)
    F.vec <- Temp.out[1:n,,] # get F.vec back solo by subsetting Temp.out wihout the stored last row with W.best and Beh.best.
   
    best.W.beh <- c(Temp.out[n+1,1,1], Temp.out[n+1,2,1]) # get only stored values W.best and Beh.best (last row of Temp.out)
     Store[X,,L] <- best.W.beh # put best W and best beh in Store.
  }}

  # combine Store values with F.vec in Temp.out2 as the vessel leaving this function to bring important info to next for loop!
  Temp.out2 <- abind(F.vec, Store, along = 2) # add store columns to F.vec. This should be an array with 4 columns: F(X,t), F(X,t+1), W.best, Beh.best.
  return(Temp.out2)
}


# check to see if OVER.STATES works and returns Temp.out2! Yes!!!!
#  OVER.STATES(B, X, Xmax, L, Lmax, e, c, b, F.vec)
  

#### MAIN PROGRAM: iterate all of the above functions over time starting with T and moving backwards until t = 0.

### Parameters 
m <- 5
B <- c(0.1, 0.2, 0.4)
e <- c(2, 2, 2)
# e10 <- 4
c <- c(3, 1, 1)
X <- seq(1, 10, 1)
Xmax <- 10
L <- seq(1, 10, 1)
Lmax <- 10
b <- c(0, 1, 2)  # behavior: move 0, 1, 2 locations
Horizon <- 30  

### Output objects to save data from program
F.vec  # (rows: state, cols: F(x,t), F(x, t+1), matricies: location)
F.all  # (rows: time, cols: state, matricies: location)
Best.beh # (rows: time, cols: state, matricies: location)


# Start iterations over time
Time <- Horizon # time starts with 30 time steps.

# Use a while loop startig at Time = 30, decrement with each loop, until time is 1 and then exit the loop.
while(Time > 1) {
  Time <- Time - 1 # calculate fitness and behavioral choices for the previous time step.
  
  Temp.out2 <- OVER.STATES(B, X, Xmax, L, Lmax, e, c, b, F.vec) # get all fitness values for this time step for each state X and L!
  TempF.vec <- Temp.out2[,1:2,] # get F.vec out of Temp.out2 from OVER.STATES. Only first 2 columns of the array.
  
  for (J in 1:Xmax){    # for each state X and L, update F.vec with the new F.vec (TempF.vec) from the OVER.STATES function (column 1),
    for(K in 1:Lmax){   # and put those values back into F.vec in the second column to be ready for the next time iteration.
      F.vec[J,2,K] <- TempF.vec[J,1,K] }}
  
  Best.beh[Time,,] <- Temp.out2[,4,]  # save best behavior in Best.beh (decision matrix!)
  F.all[Time,,] <- Temp.out2[,3,]     # save best fitness in F.all
}

# check F.all and Best.beh to see if they make sense...they do!!! Woohoo!
F.all
Best.beh


#### OUTPUT
# # add labels for states as a columns in bottom rows
# X <- seq(1,10,1)
# Best.beh[Horizon,,] <- X  # these are decision matricies (behavoiral choices) for each location!
# F.all[Horizon,,] <- X     # these are decision matricies (fitnesses) for each location!



## Collapse arrays and combine into one dataframe for plotting in ggplot.
df.beh <- adply(Best.beh, c(1,2,3))                  # collapse array to dataframe
colnames(df.beh) <- c("Time", "X", "L", "Best.beh")  # add labels to columns

df.W <- adply(F.all, c(1,2,3))
colnames(df.W) <- c("Time", "X", "L", "W")

df.all <- join(df.beh, df.W)   # join data for best behavior and fitness together in one dataframe.
df.all$Best.beh <- as.factor(df.all$Best.beh)

df.all <- df.all[df.all$Time != 30,]  # drop Time = 30 becuase that is the end, and therefore have no behavioral choices.

#### PLOTS

# http://www.roymfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r/

# decision matrix plots for best behaviors at each location
ggplot(data=df.all, aes(x=Time, y=X, fill=Best.beh)) + geom_tile(color="gray90") + facet_wrap(~ L, ncol = 5) +
  scale_fill_manual(values = c("cornflowerblue", "gold", "limegreen")) +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank())


# decision matrix plots for fitness at each location
ggplot(data=df.all, aes(x=Time, y=X, fill=W)) + geom_tile() + facet_wrap(~ L, ncol = 5) +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank())




## ANOTHER THING THAT COULD BE STOCHASTIC IS WHEN PEAK OCEAN ENTRY IS!!!!!! THAT WOULD BE MOST REALISTIC!
