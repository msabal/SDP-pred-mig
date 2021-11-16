# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)


#### RUN BASELINE PARAMETERS. (no iteration)

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
write.csv(data.tracks, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.BASE.TRACKS.csv")
data.tracks <- read.csv("C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.BASE.TRACKS.csv", sep=",")


#make vector of how to color A x-axis labels by habitat type.
h.col <- ifelse(h.vec == "a", "mediumpurple",
                ifelse(h.vec == "n", "forestgreen", "blue3"))

# FIGURE 1. Baseline simulated salmon tracks.

plot_base_tracks <- ggplot(data=data.tracks, aes(x=Time, y=A, color=as.factor(Wstart))) +
  geom_line(size=1, position=position_dodge(0.4)) + geom_point(position=position_dodge(0.4)) +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.position=c(0.8, 0.45), legend.background=element_blank()) + coord_equal() +
  scale_x_continuous(breaks = seq(1,tmax,2)) +
  scale_y_continuous(breaks = seq(1,Amax,1)) +
  theme(axis.text.y = element_text(color=h.col, face="bold"), axis.text.x = element_text(face = "bold")) +
  ylab("Area") + scale_color_brewer(palette = "Set3", name= "Starting salmon size (g)")

plot_base_tracks

setwd("C:/Users/Megan/Desktop/")
pdf("Baseline_tracks.pdf", width=8, height=4)

plot_base_tracks

dev.off()




# Plot baseline patterns by Wstart (size)

# First, get summary data (do this manually instead of re-running the whole MAIN_FUN)

#Apply TRACK.SUM.FUN on data.tracks
data.tracks.L <- droplevels(data.tracks)
data.tracks.L <- split(data.tracks.L, data.tracks.L$Wstart)

out.L<-lapply(data.tracks.L, function(x) TRACK.SUM.FUN(x$A, x$h, x$Beh, x$Time, x$Fit, x$S.day, x$S.cum, x$W))

out.df<-ldply(out.L, as.vector)
colnames(out.df)[1]<-"Wstart"

# Melt dataframe to get into long format: proportion of moves by habitat PER HABITAT
df.l.beh <- out.df[,c(1, 6, 7, 8)]
df.l.beh <- melt(df.l.beh, variable.name = "Beh", value.name = "p", id.vars = c("Wstart"))
levels(df.l.beh$Beh) <- c("0", "1", "2")
df.l.beh$h <- rep("n", length(df.l.beh$Wstart))

# by habitat and movement choice: altered
df.l.beh1 <- out.df[,c(1, 9, 10, 11)]
df.l.beh1 <- melt(df.l.beh1, variable.name = "Beh", value.name = "p", id.vars = c("Wstart"))
levels(df.l.beh1$Beh) <- c("0", "1", "2")
df.l.beh1$h <- rep("a", length(df.l.beh1$Wstart))

DF.LONG.h <- rbind(df.l.beh, df.l.beh1)

# Melt dataframe to get into long format: proportion of moves by habitat TOTAL
df.l.beh <- out.df[,c(1, 12, 13, 14)]
df.l.beh <- melt(df.l.beh, variable.name = "Beh", value.name = "p.tot", id.vars = c("Wstart"))
levels(df.l.beh$Beh) <- c("0", "1", "2")
df.l.beh$h <- rep("n", length(df.l.beh$Wstart))

# by habitat and movement choice: altered
df.l.beh1 <- out.df[,c(1, 15, 16, 17)]
df.l.beh1 <- melt(df.l.beh1, variable.name = "Beh", value.name = "p.tot", id.vars = c("Wstart"))
levels(df.l.beh1$Beh) <- c("0", "1", "2")
df.l.beh1$h <- rep("a", length(df.l.beh1$Wstart))

DF.LONG.tot <- rbind(df.l.beh, df.l.beh1)

DF.LONG <- join(DF.LONG.h, DF.LONG.tot)
DF.LONG <- join(DF.LONG, out.df[c(1:5,18)])

colnames(DF.LONG) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")

# FIGURE X. Wstart by p.tot by habitat.

DF.LONG$h<-as.factor(DF.LONG$h)
levels(DF.LONG$h) <- c("Altered", "Natural")

DF.LONG$Wstart<-as.numeric(DF.LONG$Wstart)

plot_base_move0_by_Wstart <- ggplot(data=subset(DF.LONG, Beh == 0), aes(x=Wstart, y=p.tot, fill=h, color=h)) + 
  geom_line(size=0.5, aes(color=h)) + geom_point(size=2, shape=21, color="black") +
  theme_classic() + ylim(c(0,1)) +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab("Frequency of move 0") + xlab("Starting salmon size (g)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.8, 0.8), legend.background = element_rect(fill="transparent"))


plot_base_move0_by_Wstart

setwd("C:/Users/Megan/Desktop/")
pdf("Fig_base_move0_by_Wstart.pdf", width=4.5, height=4)

plot_base_move0_by_Wstart

dev.off()


# Plot duration by Wstart
plot_base_dur_by_Wstart <- ggplot(data=DF.LONG, aes(x=Wstart, y=dur)) + 
  geom_line(size=0.5) + geom_point(size=2, shape=21, color="black") +
  theme_classic() + ylim(c(20,30)) +
  ylab("Migration duration") + xlab("Starting salmon size (g)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.8, 0.8), legend.background = element_rect(fill="transparent"))


plot_base_dur_by_Wstart

# Plot G.riv by Wstart
plot_base_Griv_by_Wstart <- ggplot(data=DF.LONG, aes(x=Wstart, y=G.riv)) + 
  geom_line(size=0.5) + geom_point(size=2, shape=21, color="black") +
  theme_classic() + ylim(c(0,5)) +
  ylab("Grams gained in river") + xlab("Starting salmon size (g)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.8, 0.8), legend.background = element_rect(fill="transparent"))


plot_base_Griv_by_Wstart
