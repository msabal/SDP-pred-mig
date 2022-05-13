

# Model V2

# State Dynamics

## (2.0) State Dynamics ## Choosing parameters ##

options(scipen=999)


# Terminal Fitness Function
Ws    <- 40
r     <- 0.1
Smax  <- 0.3

TERM.FUN <- function(W, Ws, r, Smax){ Smax/(1+exp(-r*(W-Ws))) }

curve(TERM.FUN(W, Ws=Ws , r=r, Smax=Smax), xname="W", xlim=c(7,80), ylim=c(0,0.31), ylab="adult marine survival (to age 3)")

#river growth by speed
z     <- -0.01
ka    <- 0.9 # can vary btw 0.9 and 1.3
kn    <- 1.2 # can vary btw 0.9 and 1.3

RIVER.Q <- function(U, z, kh) { z*U+kh }
curve(RIVER.Q(U, z=z, kh=ka), xname="U", xlim=c(0,40), ylim=c(0,1.5))
curve(RIVER.Q(U, z=z, kh=kn), xname="U", add=T, col="mediumpurple")
abline(h=0.5, col="gray24", lty="dashed")

RIVER.Q(U=0, z=-0.01, kh=ka)
RIVER.Q(U=20, z=-0.01, kh=ka)
RIVER.Q(U=40, z=-0.01, kh=ka)


# ocean growth
f     <- 0.75
g     <- 1.2
c     <- 40
j     <- 0.05

OCEAN.Q <-    function(t, f, g, c, j) { f + g*exp(-(t-c)^2/2*j^2) }
curve(OCEAN.Q(t,f=f, g=g, c=c, j=j), xlim=c(0, 60), ylab="q (ocean)",
      xlab="Time (days)", xname = "t", ylim=c(0,2))
abline(h=1.1, col="gray24", lty="dashed") #river value max (paused in natural)
abline(h=0.5, col="gray24", lty="dashed") #river value min (fast in altered)


# Total Growth
E     <- 0.03
a     <- 0.86
Alpha <- 0.00607
d     <- 1
dn0   <- 0.7
v     <- 0.027

GROWTH.FUN <- function(W, E, q, a, Alpha, d, v, U)    { q*E*W^a - d*Alpha*W*exp(v*U) }

curve(GROWTH.FUN(W, E=E, q=1, a, Alpha, d, v, U=1), xname="W", xlim=c(7,70), ylim=c(-0.2,1))
curve(GROWTH.FUN(W, E=E, q=1.5, a, Alpha, d, v, U=1), xname="W", add=T, col="skyblue")


## Simulate Growth to Check for realistic growth rates.
sim.mix <- data.frame(t=seq(1,60, by=1), X=rep(NA, 60), 
                      h=rep(NA, 60), U=rep(NA, 60))     # set baseline values.

sim.mix[1,2] <- 7.1 # set starting values for mass

sim.mix[31:60, 3] <- rep("o", 30)   # final 30 days in ocean
sim.mix[1:30, 3] <- sample(0:1, 30, replace=T, prob=c(0.5,0.5))  # first 30 days random between altered and natural
sim.mix$h[sim.mix$h == "1"] <- "a"   # change 1 from sample function to "a"
sim.mix$h[sim.mix$h == "0"] <- "n"   # change 0 from sample function to "n"

sim.mix[31:60, 4] <- rep(0, 30)   # last 30 days move 0 in ocean
sim.mix[1:30, 4] <- sample(0:2, 30, replace=T)  # first 30 days random sample between 0,1,2.
sim.mix$U[sim.mix$U == "1"] <- 20  # change 1 from sample function to "20" km/day
sim.mix$U[sim.mix$U == "2"] <- 40  # change 2 from sample function to "40" km/day


#for loop to simulate salmon mass over 60 days in a mix of habitats.
for(t in 1:59){
  sim.mix[t+1,2]<- sim.mix[t,2] + GROWTH.FUN(W = sim.mix[t,2], E=E, a=a, Alpha=Alpha, v=v,
                                             d = ifelse(sim.mix[t,3] == "n" & sim.mix[t,4] == 0, dn0, d),
                                             q= ifelse(sim.mix[t,3] == "o", OCEAN.Q(t=sim.mix[t,1], f=f, g=g, c=c, j=j),
                                                       ifelse(sim.mix[t,3] == "a", RIVER.Q(U=sim.mix[t,4], z=z, kh=ka), 
                                                              RIVER.Q(U=sim.mix[t,4], z=z, kh=kn))),
                                             U=sim.mix[t,4]) } # end loop.

sim.mix # look at trajectories.
(sim.mix[30,2] - sim.mix[1,2]) / 30   # daily river growth X g/day
(sim.mix[60,2] - sim.mix[31,2]) / 30  # daily ocean growth X g/day
(log(sim.mix[30,2]) - log(sim.mix[1,2])) / 30 * 100 # river specific growth rate (lnWf - lnWi)/time *100
(log(sim.mix[60,2]) - log(sim.mix[31,2])) / 30 * 100 # ocean specific growth rate (lnWf - lnWi)/time *100



# Survival
SURV.FUN <-   function(W, Bu, Bh, Bw, M, m, y, P)     { (1-M*(Bu + Bh + Bw*W^m))^(y*P) }









