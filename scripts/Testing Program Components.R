
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