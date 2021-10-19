
# practice with Main Function


MAIN_FUN <- function(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                     E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                     qa, qn, ya, yn, yo, # vars that vary by habitat (h.vec)
                     Ws, r, Smax, W, # vars for Terminal fitness function
                     Wstep.n, Wstep, tmax, seeds, F.vec)
{ # start function
  
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
                             E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P,
                             qa, qn, ya, yn, yo,
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
  
  return(Best.beh)
  
} # end function.
