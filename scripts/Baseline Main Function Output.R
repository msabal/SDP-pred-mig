
#### Run baseline parameters. (no iteration)

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

# Run MAIN_FUN with baseline parameters
OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                qa, qn, ya, yn, yo, dn0, # vars that vary by habitat (h.vec)
                Ws, r, Smax, W, # vars for Terminal fitness function
                Wstep.n, Wstep, tmax, seeds, F.vec)

colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur")

OUT

