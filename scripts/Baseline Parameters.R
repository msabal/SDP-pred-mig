
#### SET BASELINE PARAMETERS ####

#### Parameters

# seeds for h.vec
seeds <- 1 # can change

# W: salmon weight (g)
Wmin <- 7
Wmax <- 50
Wstep <- 0.1 # maybe switch to 0.05 g - I think salmon aren't growing enough because of rounding...
Wstep.n <- ((Wmax-Wmin)/Wstep)

# A: salmon area
Amin <- 1
Amax <- 26

# t: time
tmin <- 1
tmax <- 60
# Behavioral choice
U <- c(0, 1, 2)

# Terminal fitness
Ws    <- 40
r     <- 0.1
Smax  <- 0.3

# Growth
E     <- 0.04
qa    <- 0.7 #can change
qn    <- 1    # can change
a     <- 0.86
Alpha <- 0.00607
d     <- 1
dn0   <- 0.7
v     <- 0.027
f     <- 0.5
g     <- 2
c     <- 40
j     <- 0.07

# Risk
Bu    <- c(0.7, 1, 0.7) # B0, B1, B2 (can concatenate because we will loop over behavior choices?)
Ba    <- 1
Bn    <- 0.7 #can change
Bo    <- 1
Bw    <- 2
M     <- 0.002
m     <- -0.37
ya    <- 1  #can change
yn    <- 1  #can change
yo    <- 1  #can change
P     <- 20