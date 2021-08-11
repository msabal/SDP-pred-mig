
### FINAL GROWTH EQUATION ###


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

