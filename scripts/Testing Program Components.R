
# Test Fitness Function

# Pick state combo to test function on:
W <- 15
A <- 26
t <- 60
U <- 1

rm(W,A,t,U) # clear practice values.

# Test Growth function
W + GROWTH.FUN(W=W, E=E, a=a, v=v, Alpha= Alpha,
               U=ifelse(U == 1, 20, ifelse(U == 2, 40, 0)),
               d=ifelse(U == 0 & h.vec[A] == "n", dn0, d),
               q= ifelse(h.vec[A] == "o", OCEAN.Q(t=t, f=f, g=g, c=c, j=j),
                         ifelse(h.vec[A] == "n", qn, qa)))


# Test Risk function
RISK.FUN(W=W, Bu=Bu[1], Bw=Bw, M=M, m=m, P=P,
         Bh=ifelse(h.vec[A] == "n", Bn, ifelse(h.vec[A] == "a", Ba, Bo)),
         y=ifelse(h.vec[A] == "n", yn,
                  ifelse(h.vec[A] == "a", ya, yo))) *F.vec[20, 2, 26] # F.vec[Wnew, 2, Anew]

# Test entire Fitness function
FITNESS(W, A, t, U, Wmax, Amax,
                    E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, 
                    qa, qn, ya, yn, yo, 
                    h.vec, F.vec) 

# omg it worrrrks!!!! need to test with values A<-26 and t<-60 to get an actual number.


# try new Fitness function to also get Surv.day values
FITNESS <- function(W, A, t, U, Wmax, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                    E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                    qa, qn, ya, yn, yo, # vars that vary by habitat (h.vec)
                    h.vec, F.vec) # vectors describing habitats of areas and stored Fitness values
  
{ # start of function
  
  #define or change input parameters that depend on U, habitat, or Area.
  U  <- ifelse(U == 1, 20, ifelse(U == 2, 40, 0))
  d  <- ifelse(U == 0 & h.vec[A] == "n", dn0, d)
  q  <- ifelse(h.vec[A] == "o", OCEAN.Q(t=t, f=f, g=g, c=c, j=j), ifelse(h.vec[A] == "n", qn, qa))
  Bh <- ifelse(h.vec[A] == "n", Bn, ifelse(h.vec[A] == "a", Ba, Bo))
  y  <- ifelse(h.vec[A] == "n", yn, ifelse(h.vec[A] == "a", ya, yo))
  
  Wnew <- W + GROWTH.FUN(W=W, E=E, a=a, v=v, Alpha= Alpha, U=U, d=d, q=q)
  
  Wnew <- min(Wnew, Wmax) # if Wnew is greater than max value, keep at max value (this will be unlikely to happen since our Wmax is BIG.)
  Anew <- A + ifelse(U == 20, 1, ifelse(U == 40, 2, 0))           # salmon's new location is its current A plus movement choice U.
  Anew <- min(Anew, Amax) # if salmon are in the ocean (Amax) keep them in the ocean.
  
  Fit <- matrix(NA, 2, 1)
  Fit[1,] <- ifelse(Wnew > Wmin, Surv.day, 0)
  Fit[2,] <- RISK.FUN(W=W, Bu=Bu[1], Bw=Bw, M=M, m=m,
                      P=P, Bh=Bh, y=y)*F.vec[Wnew, 2, Anew]
  
  return(Fit)
  
}



# main programming loops

# Use while loop starting at Time = tmax, decrement with each loop, until time is 1 and then exit the loop.
while(t > 1)
{ # start while loop
  
  t <- t - 1 # This takes the Time from the prior loop and decrements it by one for the next loop.
  
  Temp.out2 <- OVER.STATES(Wc, A, t, U, Wmax, Amax,
                           E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P,
                           qa, qn, ya, yn, yo,
                           h.vec, F.vec)  # Get all F.best, Beh.best, and S.day for all W and A for the current Time.
  # Temp.out2 also has the updated F.vec!
  
  Temp.out2 <- Test.States
  
  
  
  TempF.vec <- Temp.out2[,1:2,] # Get F.vec out of Temp.out2 (first two columns).
  View(TempF.vec[,,Amax])
  
  
  for (J in 1:Wstep.n){    # for each state W and A, update F.vec with the new F.vec (TempF.vec) from the OVER.STATES function (column 1),
    for(K in 1:Amax){   # and put those values back into F.vec in the second column to be ready for the next time iteration.
      F.vec[J,2,K] <- TempF.vec[J,1,K] }}
  
  Best.beh[t,,] <- Temp.out2[,4,]  # save best behavior in Best.beh (decision matrix!)
  F.all[t,,] <- Temp.out2[,3,]     # save best fitness in F.all
  Surv.day[t,,] <- Temp.out2[,5,]  # save daily Survival in Surv.day
  
} # end while loop.




