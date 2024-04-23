
# Test Fitness Function

# Pick state combo to test function on:
W <- 7.1
A <- 26
t <- 40
U <- 0

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




### OVER.BEH troubleshooting :(
Wc=1


OVER.BEH <- function(Wc, A, t, U, Wmax, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                     E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                     qa, qn, ya, yn, yo, # vars that vary by habitat (h.vec)
                     h.vec, F.vec)
  
{ # start function
  F.beh.surv <- matrix(NA, 3, 3)  #matrix to save fitness (Fit) from each (3) behavioral choice.
  
  # run a for loop over all behavioral choices to get FITNESS (Fit) from each one.
  for(i in 1:length(U)){ F.beh.surv[i,] <- FITNESS(Wc, A, t, U[i], Wmax, Amax,
                                                   E, q, a, Alpha, d, v, f, g, c, j, Bu[i], Bh, Bw, M, m, y, P, 
                                                   qa, qn, ya, yn, yo, 
                                                   h.vec, F.vec) }
  
  F.best <- max(F.beh.surv[,1])  # Get maximum expected fitness from all three behavioral choices.
  S.day <- ifelse(F.best > 0, F.beh.surv[F.beh.surv[,1] == F.best, 2], NA) # Get the daily survival from the max expected fitness.
  G.day <- ifelse(F.best > 0, F.beh.surv[F.beh.surv[,1] == F.best, 3], NA) # Get the daily growth from the max expected fitness.
  
  Temp <- data.frame(Fit = F.beh.surv[,1], beh = seq(0,2,1), Surv = F.beh.surv[,2], Growth = F.beh.surv[,3])  # make temp dataframe with each fitness (Fit) for each behavior (0,1,2), and daily survival (Surv).
  Beh.best <- ifelse(F.best > 0, Temp[Temp$Fit == max(Temp$Fit),2], NA)  #get single value of best behavior choice (0,1,2). If F.best is 0 for all behavioral types, than no beh is best, return NA.
  
  F.vec[Wc,1,A] <- F.best  # save best fitness in the appropriate F.vec column 1 for that specific state W and A.
  
  Temp.out <- array(NA, dim=c(Wstep.n+2, 2, Amax)) #(rows: salmon weight, cols: F(x,t), F(x, t+1), matrices: area). Same size as F.vec, but add 2 rows to save F.best, Best.beh, and Surv.day.
  Temp.out[Wstep.n+1,,] <- c(F.best, Beh.best)
  Temp.out[Wstep.n+2,,] <- c(S.day, G.day)
  Temp.out[1:Wstep.n,,] <- F.vec  # Temp.out needs ALL the info that will be used in the next function/loop. 
  # F.vec AND F.best, S.day, and Beh.best. Will split up later to use in different ways in the next function OVER.STATES.
  return(Temp.out)
  
} # end function.






