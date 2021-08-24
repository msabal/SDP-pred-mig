
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



#### PLOTTING THE DECISION, FITNESS, AND SURVIVAL MATRICIES

# decision matrix plots for best behaviors at each location
#ggplot(data=subset(pdat, A == 1), aes(x=t, y=W, fill=Best.beh)) + geom_tile(color="gray90") + facet_wrap(~ A, ncol = 2) +
#  scale_fill_manual(values = c("cornflowerblue", "gold", "limegreen")) + coord_equal() +
#  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank())


# Dataset is too big (especially 730 values of salmon weights!) to plot.




#### FORWARD SIMULATIONS ####
Wmin <- 7
Wmax <- 80
Wstep <- 0.1
Wstep.n <- ((Wmax-Wmin)/Wstep)
Amin <- 1
Amax <- 26
tmin <- 1
tmax <- 60

# create subset of salmon starting computer weights (Wc) to simulate tracks for
sim.sam <- c(7.1, 10, 15, 20)

# Build functions to convert between W and Wc
WtoWc <- function(W){ round(m.W*W + y.W, digits=Wstep) }
WctoW <- function(Wc){ m.Wc*Wc + y.Wc }

# convert sim.sam
sim.sam <- WtoWc(sim.sam)

# create output objects to store outcomes from for loop
output <- matrix(NA, tmax, length(sim.sam))  # rows: time, columns: weight (W). Output to track best behaviors over time for each state.
output[1,] <- 1 # salmon start in area 1

# for loop tracking new areas for individuals starting at A = 1, and my chosen salmon weights (W in sim.sam).

for(X in 1:length(sim.sam)){                                      # iterate over chosen starting salmon weights
  for(t in 1:(tmax-1)){                                           # iterate over time
    A <- output[t,X]                                              # current area is the value in the current row of time (t)
    Anew <- A + pdat[pdat$t == t & pdat$A == A & pdat$Wc == X, 4] # location in next time step is the current location plus how much they move (Best.beh)
    Anew <- min(Anew, Amax)                                       # location cannot be greater than Lmax 
    output[t+1,X] <- Anew                                        # store new location in the next row (t+1)
  }
}


t=1; A=1; X=1
pdat[pdat$t == t & pdat$A == A & pdat$Wc == X, 4]

rm(t); rm(A); rm(X)


#### Plot forward simulations!

data.tracks <- melt(output)
colnames(data.tracks)<- c("Time", "X", "Location")

indiv.constant <- ggplot(data=data.tracks, aes(x=Time, y=Location, color=as.factor(X))) + geom_line(size=1) + geom_point() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) + coord_equal() +
  scale_x_continuous(breaks = seq(1,Horizon,1)) +
  scale_y_continuous(breaks = seq(1,Lmax,1)) +
  scale_color_brewer(name= "Energy (X)", palette = "PiYG") +
  ggtitle("Constant ocean predation") + theme(plot.title = element_text(hjust = 0.5))
indiv.constant

setwd("C:/Users/Megan/Desktop/")
pdf("TRACKS.CON.pdf", width=7.5, height=11)
indiv.constant
dev.off()

