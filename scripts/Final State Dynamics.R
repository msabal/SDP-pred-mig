

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










