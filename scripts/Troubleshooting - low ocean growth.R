
# Troubleshooting SDP low ocean growth compared to direct simulation.

GROWTH.FUN <- function(W, E, q, a, Alpha, d, v, U)    { q*E*W^a - d*Alpha*W*exp(v*U) }

OCEAN.Q <-  function(t, f, g, c, j) { f + g*exp(-(t-c)^2/2*j^2) } # original (2) Functions - All.R

# Choose parameters...that simulate a 10 g salmon in the ocean
                      # for 30 days from t = 20 to t = 49 that grows
                      # a total of 30 g per day (average 1 g/day).

# Salmon simulations - DIRECT
sim.move0o <- data.frame(t=seq(20,49, by=1), W=rep(NA, 30), 
                         h=rep("o", 30), U=rep(0, 30))     # set baseline values.

sim.move0o[1,2] <- 10 # set starting values for mass, 10g

#for loop to simulate salmon mass over 30 days.
for(t in 1:29){
  sim.move0o[t+1,2]<-sim.move0o[t,2] +
    GROWTH.FUN(W = sim.move0o[t,2], a=a, 
               q=OCEAN.Q(t=sim.move0o[t,1], g=g, c=c, j=j, f=f),
               E=E, Alpha=Alpha, d=d,
               v=v, U=sim.move0o[t,4])}

sim.move0o # look at output dataset.

sim.move0o$G.day <- NA

for(t in 1:29){
  sim.move0o[t,5] <- sim.move0o[t+1,2] - sim.move0o[t,2]
}

g_30 <- sim.move0o[30,2] - sim.move0o[1,2]  # calc total grams gained in 30 days
g_30

g_30/30  # calc average grams gained (g/day)



# Inside Main Function - Test Fitness: Do i get the same growth rates?

FITNESS(Wc=WtoWc(10), A=Amax, t=21, U=0, Wmax, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                     E, q, a, Alpha, d, v, f, g, c, j, Bu=Bu[1], Bw, M, m, y, P, # vars in functions
                     qa, qn, ya, yn, yo, dn0, Ba, Bn, Bo, # vars that vary by habitat (h.vec)
                     seeds=1, F.vec) # vectors describing habitats of areas and stored Fitness values
   
### YES. The fitness calculations match exactly with the direct simulations. No issues there.



 ### G.DAY FROM BASELIEN MAIN FUNCTION OUTPUT GIVES ALL CORRECT G.DAY INCREMENTS. THAT MEANS THE LOOK-UP TABLES ARE GOOD.
      # THE PROBLEM MUST BE IN THE FORWARD SIMULATIONS. TRY THIS NEXT.



#### FORWARD SIMULATIONS ####
# create subset of salmon starting computer weights (Wc) to simulate tracks for
Wstart <- 10 # pick starting weights (g) to simulate
Wstart <- WtoWc(Wstart) # convert from W in grams to Wc

output.Wc <- matrix(NA, 30, length(Wstart)) # output to store changing W over time.
output.Wc[1,] <- Wstart

# for loop tracking new areas for individuals starting at A = 1, and my chosen salmon weights (W in sim.sam).

  for(t in 1:29){                    # iterate over time
    
    Wc <-output.Wc[t]                    # current Wc (computer weight) is from the appro spot in output.Wc
    W <- WctoW(Wc)                         # convert to W (salmon weight in g)
    W.new <- W + G.day[t+19, output.Wc[t], A=26]     # new salmon weight is current W (g) plus growth increment from certain choice stored in G.day
    Wc.new <- WtoWc(W.new)                 # convert new W to new Wc
    output.Wc[t+1] <- Wc.new             # store new Wc in output.Wc
    
  } # end for loops.

output.W <- WctoW(output.Wc) # convert output.W from Wc to W (grams)

comp.df <- data.frame(output.W = output.W, output.Wc = output.Wc, sim.W = sim.move0o[,2], sim.Gday = sim.move0o[,5])

# compare sdp output growth rate: 1.006667 g/day!!!
(comp.df$output.W[30] - comp.df$output.W[1]) / 30

# compare direct sim growth rate: 1.011244 g/day/!!!!
(comp.df$sim.W[30] - comp.df$sim.W[1]) / 30
