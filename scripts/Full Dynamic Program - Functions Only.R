

 #### Make functions to convert between W in grams and W in computer discrete indexing

# Make a data frame to covert between real W (grams) and computer discrete index (Wc)
Wconvdf <- data.frame(W = seq(Wmin+Wstep, Wmax, 0.1), Wc = seq(1,Wstep.n,1))

plot(W ~ Wc, Wconvdf)
summary(lm(W ~ Wc, Wconvdf)) # y-int: 7, slope: 0.1
m.Wc <- 0.1
y.Wc <- 7

plot(Wc ~ W, Wconvdf)
summary(lm(Wc ~ W, Wconvdf)) # y-int: -70, slope: 10
m.W <- 10
y.W <- -70

# Build functions to convert between W and Wc
WtoWc <- function(W){ round(m.W*W + y.W, digits=Wstep) }
WctoW <- function(Wc){ m.Wc*Wc + y.Wc }

# Test
WtoWc(10); WtoWc(10.1); WtoWc(10.11) # Good. 30, 31, 31.
WctoW(30); WctoW(31) # Good.



#### Terminal fitness function - how does salmon weight at tmax and Amax relate to probability of
#    surviving to a returning adult? Larger salmon more likely to survive to a returning adult.
#    Sigmoidal function that asymptotes at Smax.

#ONLY applicable for salmon at A 26 at tmax!!!! Otherwise, fitness is 0!
TERM.FUN <- function(W, Ws, r, Smax){ Smax/(1+exp(-r*(W-Ws))) }

# plot Terminal Fitness function
curve(TERM.FUN(W, Ws=Ws , r=r, Smax=Smax), xname="W", xlim=c(7,80), ylim=c(0,0.31), ylab="adult marine survival (to age 3)")



# Functions used inside the Fitness function (see Final State Dynamics.R for more details)
GROWTH.FUN <- function(W, E, q, a, Alpha, d, v, U)    { q*E*W^a - d*Alpha*W*exp(v*U) }
OCEAN.Q <-    function(t, f, g, c, j)             { f + g*exp(-(t-c)^2/2*j^2) }
SURV.FUN <-   function(W, Bu, Bh, Bw, M, m, y, P) { (1-M*(Bu + Bh + Bw*W^m))^(y*P) }


# Fitness function (described above)
FITNESS <- function(Wc, A, t, U, Wmax, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                    E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                    qa, qn, ya, yn, yo, # vars that vary by habitat (h.vec)
                    seeds, F.vec) # vectors describing habitats of areas and stored Fitness values
  
{ # start of function
  
  #define h.vec based on variable seeds
  h.vec <- rep(NA, Amax) # create blank vector for habitats for each Area.
  h.vec[Amax] <- "o" # make the last area (Amax) the ocean: "o"
  set.seed(seeds)  # set.seed to keep altered and natural habitat distribution constant for now.
  h.vec[1:Amax-1] <- sample(0:1, Amax-1, replace=T, prob=c(0.5,0.5))  # randomly sample
  # Amax-1 number of values 0 or 1 with a 50% probability between the two values.
  h.vec[h.vec == "1"] <- "a"  # change 1 from sample function to "a"
  h.vec[h.vec == "0"] <- "n"  # change 0 from sample function to "n"
  
  #define or change input parameters that depend on U, habitat, or Area.
  U  <- ifelse(U == 1, 20, ifelse(U == 2, 40, 0))
  d  <- ifelse(U == 0 & h.vec[A] == "n", dn0, d)
  q  <- ifelse(h.vec[A] == "o", OCEAN.Q(t=t, f=f, g=g, c=c, j=j), ifelse(h.vec[A] == "n", qn, qa))
  Bh <- ifelse(h.vec[A] == "n", Bn, ifelse(h.vec[A] == "a", Ba, Bo))
  y  <- ifelse(h.vec[A] == "n", yn, ifelse(h.vec[A] == "a", ya, yo))
  W  <- WctoW(Wc) # convert Wc (discrete 1 to 730 to W in grams)
  
  Wnew <- W + GROWTH.FUN(W=W, E=E, a=a, v=v, Alpha= Alpha, U=U, d=d, q=q)
  
  Wnew <- min(Wnew, Wmax) # if Wnew is greater than max value, keep at max value (this will be unlikely to happen since our Wmax is BIG.)
  Wcnew <- WtoWc(Wnew)
  
  Anew <- A + ifelse(U == 20, 1, ifelse(U == 40, 2, 0))           # salmon's new location is its current A plus movement choice U.
  Anew <- min(Anew, Amax) # if salmon are in the ocean (Amax) keep them in the ocean.
  
  Fit <- matrix(NA, 1, 3)
  Fit[,1] <- ifelse(Wnew > Wmin, SURV.FUN(W=W, Bu=Bu, Bw=Bw, M=M, m=m,
                                          P=P, Bh=Bh, y=y)*F.vec[Wcnew, 2, Anew], 0)
  Fit[,2] <- SURV.FUN(W=W, Bu=Bu, Bw=Bw, M=M, m=m,
                      P=P, Bh=Bh, y=y)
  Fit[,3] <- GROWTH.FUN(W=W, E=E, a=a, v=v, Alpha= Alpha, U=U, d=d, q=q)
  
  return(Fit)
  
} # end function.

#Test Fitness function 
FITNESS(Wc=1, A=Amax, t=tmax, U=0, Wmax, Amax,
        E, q, a, Alpha, d, v, f, g, c, j, Bu=Bu[1], Bh, Bw, M, m, y, P, 
        qa, qn, ya, yn, yo, 
        seeds=1, F.vec) # # (Expected Fitness) and # (daily survival) and # growth rate (for that time step)!!!!!



#### OVER.BEH function to apply over BEHAVIORAL CHOICES (move 0, 1, 2).
# Function used to apply FITNESS using for loop over all behavioral choices.
# Returns the maximum fitness (F.best), best choice (Beh.best), and daily survival (Surv.day).
# F.best saved in appro first col of F.vec.
# Returns Temp.out: copy of updated F.vec with extra rows at bottom with F.best and Beh.best.

OVER.BEH <- function(Wc, A, t, U, Wmax, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                     E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                     qa, qn, ya, yn, yo, # vars that vary by habitat (h.vec)
                     seeds, F.vec)
  
{ # start function
  F.beh.surv <- matrix(NA, 3, 3)  #matrix to save fitness (Fit) from each (3) behavioral choice.
  
  # run a for loop over all behavioral choices to get FITNESS (Fit) from each one.
  for(i in 1:length(U)){ F.beh.surv[i,] <- FITNESS(Wc, A, t, U[i], Wmax, Amax,
                                                   E, q, a, Alpha, d, v, f, g, c, j, Bu[i], Bh, Bw, M, m, y, P, 
                                                   qa, qn, ya, yn, yo, 
                                                   seeds, F.vec) }
  
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

# Check if loop inside OVER.BEH works
F.beh.surv <- matrix(NA, 3, 3)
for(i in 1:length(U)){ F.beh.surv[i,] <- FITNESS(Wc=1, A=Amax, t=tmax, U[i], Wmax, Amax,
                                                 E, q, a, Alpha, d, v, f, g, c, j, Bu[i], Bh, Bw, M, m, y, P, 
                                                 qa, qn, ya, yn, yo, 
                                                 seeds=1, F.vec) }
F.beh.surv # Works!!!

rm(F.beh.surv) #remove after done checking.


# Check if OVER.BEH works for a specific W and A state for tmax-1. 
Test.beh <- OVER.BEH(Wc=1, A=Amax, t=tmax, U, Wmax, Amax,
                     E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P,
                     qa, qn, ya, yn, yo, 
                     seeds=1, F.vec)
View(Test.beh[,,Amax])   # Works!!! Look for a value in column one at W<-3 and
# in rows 731 and 731 where we stored Fit, Beh.best, S.day, and G.day.
rm(Test.beh)


#### OVER.STATES function to apply over BEHAVIORAL CHOICES (move 0, 1, 2) AND AREA (1 to 26).
# Function used to iterate OVER.BEH over both states W and A using nested for loops.
# Run OVER.BEH and get Temp.out. Split Temp.out to F.vec and best.F.beh (only F.best and Beh.best).
# Put Beh.best in appro Store spot. Combine F.vec and Store in an array,
# Temp.out2 with 4 cols (F(x,t), F(x,t+1), F.best, Beh.best, S.day).

OVER.STATES <- function(Wc, A, t, U, Wmax, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                        E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                        qa, qn, ya, yn, yo, # vars that vary by habitat (h.vec)
                        seeds, F.vec)
{ # start function
  
  Store <- array(NA, dim=c(Wstep.n, 4, Amax)) # Array to store all F.best (col 1), Beh.best (col 2), and S.day (col 3), G.day (col 4)
  # from OVER.BEH for each state W (rows) and A (matrices).
  
  for(Wc in 1:Wstep.n){
    for(A in 1:Amax){
      Temp.out <- OVER.BEH(Wc, A, t, U, Wmax, Amax,
                           E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P,
                           qa, qn, ya, yn, yo,
                           seeds, F.vec) # this returns Temp.out from OVER.BEH, which looks like...
      # an array: (rows: salmon weight (length: Wstep.n+2),
      # 2 cols: F(x,t), F(x, t+1), matrices: area (length: Amax)).
      # Same size as F.vec, but add 2 rows to Wstep.n to save
      # F.best [in Wstep.n+1 row, 1st col], Best.beh [in Wstep.n+1 row, 2nd col],
      # Surv.day [Wstep.n=2 row, 1st col], and G.day [Wstep.n=2 row, 2nd col].
      
      F.vec <- Temp.out[1:Wstep.n,,] # get F.vec by itself out of Temp.out (without last two rows).
      best.F.beh.S <- c(Temp.out[Wstep.n+1,1,1], Temp.out[Wstep.n+1,2,1], Temp.out[Wstep.n+2,1,1], Temp.out[Wstep.n+2,2,1]) # get F.best, Beh.best, S.day, G.day.
      
      Store[Wc,,A] <- best.F.beh.S # put F.best, Beh.best, S.day, G.day in Store for appro loop state combo.
      
    }} #end nested for loops.
  
  # Combine Store values with F.vec in Temp.out2 as the vessel leaving this function to bring important info to next for loop!
  Temp.out2 <- abind(F.vec, Store, along = 2) # add Store columns to F.vec. This should be an array (rows: Wstep.n, matrices: Amax)
  # with 5 columns: F(X,t), F(X,t+1), F.best, Beh.best. S.day.
  # along = 2 tells to bind the columns (dimension 2) together!
  return(Temp.out2)
  
} # end function.


# check to see if OVER.STATES works and returns Temp.out2
Test.States <- OVER.STATES(Wc, A, t=tmax-1, U, Wmax, Amax,
                           E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P,
                           qa, qn, ya, yn, yo, 
                           seeds=1, F.vec)

View(Test.States[,,Amax-1]) # works!!
# View(Test.States[,,Amax]) Col 2 (F(W,t+1)) has values from terminal fitness, optimal choice has to be move 0
# View(Test.States[,,Amax-1]) Col 2 (F(W,t+1)) has 0s from terminal fitness, optimal choice can be move 1 or 2
# View(Test.States[,,Amax-2]) Col 2 (F(W,t+1)) has 0s from terminal fitness, optimal choice has to be move 2
# View(Test.States[,,Amax-3]) Col 2 (F(W,t+1)) has 0s from terminal fitness, F.best all 0s because can't make it
# to A26 in by tmax. Therefore, Beh.best and S.day all NAs.
rm(Test.States)



#### MAIN FUNCTION of dynamic programming models.


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
        F.vec[J,2,K] <- TempF.vec[J,1,K] }}
    
    Best.beh[t,,] <- Temp.out2[,4,]  # save best behavior in Best.beh (decision matrix!)
    F.all[t,,] <- Temp.out2[,3,]     # save best fitness in F.all
    Surv.day[t,,] <- Temp.out2[,5,]  # save daily Survival in Surv.day
    G.day[t,,] <- Temp.out2[,6,]     # save daily growth in G.day
    
    
    # for each seed iteration of decision matrices... 
    
    # or...
    Temp.out3 <- abind(Best.beh, F.all, Surv.day, G.day, along = 2)  # not sure if this will work or what it will look like.
      
    return(Temp.out3)
    
    
  } # end while loop.
  } # end function.






