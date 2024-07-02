# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

#### RUN NULL HABITAT MODEL - NO ITERATION

# Parameters - NULL HABITAT DIFFERENCES

#### SET BASELINE PARAMETERS ####

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
qa    <- 1        # no diff
qn    <- 1        # no diff
a     <- 0.86
Alpha <- 0.00607
d     <- 1        # no diff
dn0   <- 1        # no diff
v     <- 0.027
f     <- 0.5
g     <- 2
c     <- 40
j     <- 0.07

# Risk
Bu    <- c(0.7, 1, 0.7) # B0, B1, B2 (can concatenate because we will loop over behavior choices?)
Ba    <- 1        # no diff
Bn    <- 1        # no diff
Bo    <- 1
Bw    <- 2
M     <- 0.002
m     <- -0.37
ya    <- 1        # no diff
yn    <- 1        # no diff
yo    <- 1  
P     <- 20

# Run Model to datatracks

#### Inside of MAIN_FUN only until calculates datatracks

#make objects to store loop outputs.
F.all <- array(NA, dim=c(tmax, Wstep.n, Amax))  #(rows: time, cols: weight, matrices: area)
Best.beh <- array(NA, dim=c(tmax, Wstep.n, Amax))  #(rows: time, cols: weight, matrices: area)
Surv.day <- array(NA, dim=c(tmax, Wstep.n, Amax))  #(rows: time, cols: weight, matrices: area)
G.day <- array(NA, dim=c(tmax, Wstep.n, Amax))  #(rows: time, cols: weight, matrices: area)

# Set up F.vec - does this need to be outside the loops/functions to store properly?
F.vec <- array(NA, dim=c(Wstep.n, 2, Amax))  #(rows: weight, cols: F(x,t), F(x, t+1), matrices: area)
F.vec[1:Wstep.n, 2, Amax] <- TERM.FUN(W = seq(Wmin+Wstep, Wmax , Wstep), Ws=Ws, r=r, Smax=Smax)
F.vec[,2,1:Amax-1] <- 0    #if salmon end in any area besides the last (Amax), then their fitness is 0!

t <- tmax # Start iterations over time, time starts with tmax.

# Use while loop starting at Time = tmax, decrement with each loop, until time is 1 and then exit the loop.
while(t > 1)
{ # start while loop
  
  t <- t - 1 # This takes the Time from the prior loop and decrements it by one for the next loop.
  
  Temp.out2 <- OVER.STATES(Wc, A, t, U, Wmax, Amax,
                           E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P,
                           qa, qn, ya, yn, yo, dn0, Ba, Bn, Bo,
                           seeds, F.vec)  # Get all F.best, Beh.best, and S.day for all W and A for the current Time.
  # Temp.out2 also has the updated F.vec!
  
  TempF.vec <- Temp.out2[,1:2,] # Get F.vec out of Temp.out2 (first two columns).
  
  for (J in 1:Wstep.n){    # for each state W and A, update F.vec with the new F.vec (TempF.vec) from the OVER.STATES function (column 1),
    for(K in 1:Amax){   # and put those values back into F.vec in the second column to be ready for the next time iteration.
      F.vec[J,2,K] <- TempF.vec[J,1,K] }} # end of while loop.
  
  Best.beh[t,,] <- Temp.out2[,4,]  # save best behavior in Best.beh (decision matrix!)
  F.all[t,,] <- Temp.out2[,3,]     # save best fitness in F.all
  Surv.day[t,,] <- Temp.out2[,5,]  # save daily Survival in Surv.day
  G.day[t,,] <- Temp.out2[,6,]     # save daily growth in G.day
  
  
} # end while loop.

#### FORWARD SIMULATIONS ####
# create subset of salmon starting computer weights (Wc) to simulate tracks for
Wstart <- seq(7.1, 20, length.out=10) # pick starting weights (g) to simulate
Wstart <- WtoWc(Wstart) # convert from W in grams to Wc

# create output objects to store outcomes from for loop
output.A <- matrix(NA, tmax, length(Wstart))  # roWstart: time, columns: weight (W). Output to track best behaviors over time for each state.
output.A[1,] <- 1 # salmon start in area 1

output.S <- matrix(NA, tmax, length(Wstart)) # output to store daily Surv
output.Scum <- matrix(NA, tmax, length(Wstart)) # output to store cumulative survival over time.

output.Fit <- matrix(NA, tmax, length(Wstart)) # output to store expected Fitness
output.beh <- matrix(NA, tmax, length(Wstart)) # output to store best behavior

output.Wc <- matrix(NA, tmax, length(Wstart)) # output to store changing W over time.
output.Wc[1,] <- Wstart

# for loop tracking new areas for individuals starting at A = 1, and my chosen salmon weights (W in sim.sam).

for(X in 1:length(Wstart)){            # iterate over chosen starting salmon weights
  for(t in 1:(tmax-1)){                    # iterate over time
    A <- output.A[t,X]                     # current area is the value in the current row of time (t)
    Anew <- A + Best.beh[t,X,A]            # area in next time step is the current location plus how much they move (Best.beh)
    Anew <- min(Anew, Amax)                # area cannot be greater than Amax 
    output.A[t+1,X] <- Anew                # store new area in the next row (t+1)
    
    Wc <-output.Wc[t,X]                    # current Wc (computer weight) is from the appro spot in output.Wc
    W <- WctoW(Wc)                         # convert to W (salmon weight in g)
    W.new <- W + G.day[t,X,A]              # new salmon weight is current W (g) plus growth increment from certain choice stored in G.day
    Wc.new <- WtoWc(W.new)                 # convert new W to new Wc
    output.Wc[t+1,X] <- Wc.new             # store new Wc in output.Wc
    
    output.S[t,X] <- Surv.day[t,X,A]       #  get appro daily survival from Surv.day and save it in output.S
    output.Fit[t,X] <- F.all[t,X,A]        #  get appro expected fitness from F.all and save it in output.Fit
    output.beh[t,X] <- Best.beh[t,X,A]     #  get appro best beh from Best.beh and save it in output.beh
    
  }} # end for loops.

output.W <- WctoW(output.Wc) # convert output.W from Wc to W (grams)

# for loop for cumulative survival
for(X in 1:length(Wstart)){
  for(t in 1:(tmax)){
    output.Scum[t,X] <- prod(output.S[1:t,X])
  }} # end of loop.

## Make Data Tracks.

# Re-make h.vec
h.vec <- rep(NA, Amax) # create blank vector for habitats for each Area.
h.vec[Amax] <- "o" # make the last area (Amax) the ocean: "o"
set.seed(seeds)  # set.seed to keep altered and natural habitat distribution constant for now.
h.vec[1:Amax-1] <- sample(0:1, Amax-1, replace=T, prob=c(0.5,0.5))  # randomly sample
# Amax-1 number of values 0 or 1 with a 50% probability between the two values.
h.vec[h.vec == "1"] <- "a"  # change 1 from sample function to "a"
h.vec[h.vec == "0"] <- "n"  # change 0 from sample function to "n"

data.tracks <- melt(output.A) # melt output.A
colnames(data.tracks)<- c("Time", "Wstart", "A")
data.tracks <- join(data.tracks, data.frame(h = h.vec, A = seq(1,Amax,1)))

data.tracks.S <- melt(output.S) # melt output.S
colnames(data.tracks.S)<- c("Time", "Wstart", "S.day")
data.tracks <- join(data.tracks, data.tracks.S)

data.tracks.Fit <- melt(output.Fit)  # melt output.Fit
colnames(data.tracks.Fit)<- c("Time", "Wstart", "Fit")
data.tracks <- join(data.tracks, data.tracks.Fit)

data.tracks.beh <- melt(output.beh) # melt output.beh
colnames(data.tracks.beh)<- c("Time", "Wstart", "Beh")
data.tracks <- join(data.tracks, data.tracks.beh)

data.tracks.Scum <- melt(output.Scum)  # melt output.beh
colnames(data.tracks.Scum)<- c("Time", "Wstart", "S.cum")
data.tracks <- join(data.tracks, data.tracks.Scum)

data.tracks.W<- melt(output.W)  # melt output.W
colnames(data.tracks.W)<- c("Time", "Wstart", "W")
data.tracks <- join(data.tracks, data.tracks.W)

data.tracks$Wstart <- as.factor(data.tracks$Wstart) # convert Wstart to salmon weigh units and as a factor.
levels(data.tracks$Wstart) <- c(WctoW(Wstart))

## Export DF.SEEDS for Figures 1 and 2!
write.csv(data.tracks, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.NULL.TRACKS.csv")
data.tracks <- read.csv("C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.NULL.TRACKS.csv", sep=",")


#make vector of how to color A x-axis labels by habitat type.
h.col <- ifelse(h.vec == "a", "mediumpurple",
                ifelse(h.vec == "n", "forestgreen", "blue3"))

# FIGURE 1. Baseline simulated salmon tracks.

plot_null_tracks <- ggplot(data=data.tracks, aes(x=Time, y=A, color=as.factor(Wstart))) +
  geom_line(size=1, position=position_dodge(0.4)) + geom_point(position=position_dodge(0.4)) +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.position=c(0.8, 0.45), legend.background=element_blank()) + coord_equal() +
  scale_x_continuous(breaks = seq(1,tmax,2)) +
  scale_y_continuous(breaks = seq(1,Amax,1)) +
  theme(axis.text.y = element_text(color=h.col, face="bold"), axis.text.x = element_text(face = "bold")) +
  ylab("Area") + scale_color_brewer(palette = "Set3", name= "Starting salmon size (g)")

plot_null_tracks

setwd("C:/Users/Megan/Desktop/")
pdf("Null_tracks.pdf", width=8, height=4)

plot_null_tracks

dev.off()


#### Run full main program to get summary stats for one seed.

OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, # vars in functions
                qa, qn, ya, yn, yo, dn0, Ba, Bn, Bo, # vars that vary by habitat (h.vec)
                Ws, r, Smax, W, # vars for Terminal fitness function
                Wstep.n, Wstep, tmax, seeds, F.vec)

colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur")

# Summary stats across salmon sizes
mean(OUT$dur)
mean(OUT$S.cum.riv)
mean(OUT$G.riv) / mean(OUT$dur)
mean(OUT$G.ocean) / (60-mean(OUT$dur))
