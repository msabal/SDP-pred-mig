

## (2.0) State Dynamics ## Choosing parameters ##

options(scipen=999)


# Terminal Fitness Function
Ws    <- 40
r     <- 0.1
Smax  <- 0.3

TERM.FUN <- function(W, Ws, r, Smax){ Smax/(1+exp(-r*(W-Ws))) }

curve(TERM.FUN(W, Ws=Ws , r=r, Smax=Smax), xname="W", xlim=c(7,80), ylim=c(0,0.31), ylab="adult marine survival (to age 3)")

# Growth
E     <- 0.04
a     <- 0.86
Alpha <- 0.00607
d     <- 1
dn0   <- 0.7
v     <- 0.027

#river growth by speed
z     <- -0.015
ka    <- 1 # can vary btw 1 and 2
kn    <- 2 # can vary btw 1 and 2

# ocean growth
f     <- 0.5
g     <- 2
c     <- 40
j     <- 0.07

q  <- 
  
OCEAN.Q(t=t, f=f, g=g, c=c, j=j)
RIVER.Q(U=U, z=z, kh=kn)
RIVER.Q(U=U, z=z, kh=ka)



GROWTH.FUN <- function(W, E, q, a, Alpha, d, v, U)    { q*E*W^a - d*Alpha*W*exp(v*U) }

curve(GROWTH.FUN(W, E=0.02, q=1, a, Alpha, d, v, U=1), xname="W", xlim=c(7,70), ylim=c(-0.2,1.5))

OCEAN.Q <-    function(t, f, g, c, j)                 { f + g*exp(-(t-c)^2/2*j^2) }
RIVER.Q <-    function(U, z, kh)                      { z*U + kh }





# Survival
SURV.FUN <-   function(W, Bu, Bh, Bw, M, m, y, P)     { (1-M*(Bu + Bh + Bw*W^m))^(y*P) }



