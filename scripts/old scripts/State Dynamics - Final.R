

options(scipen=999)


### FINAL EQUATIONS ###

# EQUATION 2: GROWTH

GROWTH.FUN <- function(W, E, q, a, A, d, v, U) {q*E*W^a - d*A*W*exp(v*U) }

# Choosing baseline E value. MacFarlane 2010 - 7 gram salmon in the estuary gains 0.7 g/day.
  # assume the river is max the same (likely lower), 7 gram salmon max gains ~ 0.1 g/day.

curve(GROWTH.FUN(W, E=0.04, q=1, a=0.86, A=0.00607, d=1, v=0.027, U=0),
      xname="W", ylab = "g/day", col="limegreen", xlim=c(7,20), ylim=c(-0.1,0.6))

curve(GROWTH.FUN(W, E=0.04, q=1, a=0.86, A=0.00607, d=1, v=0.027, U=20),
      xname="W", ylab = "g/day", col="mediumpurple", add=T)

curve(GROWTH.FUN(W, E=0.04, q=1, a=0.86, A=0.00607, d=1, v=0.027, U=40),
      xname="W", ylab = "g/day", col="pink", add=T)


# how see what range of q are reasonable without negative growth
curve(GROWTH.FUN(W, E=0.04, q=1, a=0.86, A=0.00607, d=1, v=0.027, U=40),
      xname="W", ylab = "g/day", col="limegreen", xlim=c(7,20), ylim=c(-0.1,0.6))

curve(GROWTH.FUN(W, E=0.04, q=0.7, a=0.86, A=0.00607, d=1, v=0.027, U=40),
      xname="W", ylab = "g/day", col="mediumpurple", add=T)

curve(GROWTH.FUN(W, E=0.04, q=0.5, a=0.86, A=0.00607, d=1, v=0.027, U=40),
      xname="W", ylab = "g/day", col="pink", add=T)


# For methods, calculate what a 7 gram salmon that is not moving (U=0) will grow per day.
  # 0.17 g/day!
GROWTH.FUN(W=7, E=0.04, q=1, a=0.86, A=0.00607, d=1, v=0.027, U=0)




# EQUATION 3: OCEAN GROWTH (q)
OCEAN.Q <- function(t, f, g, c, j){  f + g*exp(-(t-c)^2/2*j^2) }
curve(OCEAN.Q(t, g=2, c=40, j=0.07, f=0.5), xlim=c(0, 60), ylab="q (ocean)",
      xlab="Time (days)", xname = "t")
abline(h=1, col="mediumslateblue", lty="dashed") # max river value


# simulated salmon in the ocean for 30 days
sim.move0o <- data.frame(t=seq(20,49, by=1), W=rep(NA, 30), 
                         h=rep("o", 30), U=rep(0, 30))     # set baseline values.

sim.move0o[1,2] <- 10 # set starting values for mass, 10g

#for loop to simulate salmon mass over 30 days.
for(t in 1:29){
  sim.move0o[t+1,2]<-sim.move0o[t,2] +
                        GROWTH.FUN(W = sim.move0o[t,2], a=0.86, 
                                 q=OCEAN.Q(t=sim.move0o[t,1], g=2, c=40, j=0.07, f=0.5),
                                 E=0.04, A=0.00607, d=1,
                                 v=0.027, U=sim.move0o[t,4])}



### EQUATIONS 4 & 5: RISK
SURV.FUN <- function(W, Bu, Bh, Bw, M, m, y, P){ (1-M*(Bu + Bh + Bw*W^m))^(y*P) }

#Test RISK.FUN
SURV.FUN(W=12, Bu=1, Bh=1, Bw=2, M=0.002, m=-0.37, y=1, P=20) # good.

# solving for Beta-W
Beta.W <- function(X, Bw){ Bw*X^-0.37 }
curve(Beta.W(X, Bw=2), xname="X", xlim=c(7,20), ylim=c(0,1.1), ylab="contrib. daily mortality rate")
abline(h=1,lty="dashed")

Beta.W(X=7, Bw=2)


# simulations
# Simulate survival from sim.mix dataset
head(sim.mix) # make sure this is populated from above.

sim.mix$Surv.day <- NA  # Add column for Survival per day.
sim.mix$Surv.cum <- NA  # Add column for cumulative survival.


#for loop to calculate daily survival probability in each time step/habitat/U/W combination
for(t in 1:60){
  sim.mix[t,5]<-RISK.FUN(W = sim.mix[t,2],
                         Bu = ifelse(sim.mix[t,4] == 20, 1, 0.8),
                         Bh = ifelse(sim.mix[t,3] == "n", 0.8, 1),
                         Bw = 2,
                         M = 0.002,
                         m = -0.37,
                         y = ifelse(sim.mix[t,3] == "n", 1, ifelse(sim.mix[t,3] == "a", 0.8, 1)),
                         P = 20)}

sim.mix$Surv.cum[1] <- sim.mix$Surv.day[1] # set first Surv.cum to the same as the single day.

# for loop to calculate cumulative survival
for(t in 1:59){ sim.mix[t+1,6] <- prod(sim.mix[1:t+1,5])}


#compare plots

#cum surv plots
plot(Surv.cum~t, sim.mix, col=as.factor(U), pch=16)
plot(Surv.cum~t, sim.mix, col=as.factor(h), pch=16)

#surv.day plots
plot(Surv.day~t, sim.mix, col=as.factor(U), pch=16)
plot(Surv.day~t, sim.mix, col=as.factor(h), pch=16)



### EQUATION 7: TERMINAL FITNESS
TERM.FUN <- function(W, Ws, r, Smax){ Smax/(1+exp(-r*(W-Ws))) }

# plot Terminal Fitness function
curve(TERM.FUN(W, Ws=40 , r=0.1, Smax=0.3), xname="W", xlim=c(7,80), ylim=c(0,0.31), ylab="adult marine survival (to age 3)")

# check relative values
TERM.FUN(W=10, Ws=40 , r=0.1, Smax=0.3) # 10 g salmon 0.014 (1.4% survival)
TERM.FUN(W=20, Ws=40 , r=0.1, Smax=0.3) # 20 g salmon 0.035 (3.5% survival)
TERM.FUN(W=40, Ws=40 , r=0.1, Smax=0.3) # 40 g salmon 0.15 (15% survival)
# 10 g to 20 g is a little more than 2x higher (2x her fits Duffy) 
# 10 g to 40 g is almost exactly 10x higher (10x her fits Duffy!)
