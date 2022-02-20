
#### SET BASELINE PARAMETERS ####

#### Parameters

# seeds for h.vec
seeds <- 1 # can change
N <- 0.5 # percent of natural habitat in river.

# W: salmon weight (g)
Wmin <- 7
Wmax <- 60   # 
Wstep <- 0.1 # checked 0.05 steps and no big difference. This is good.
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
E     <- 0.03
a     <- 0.86
Alpha <- 0.00607
d     <- 1
dn0   <- 0.7
v     <- 0.027

#river growth by speed
z     <- -0.01 # -0.01 when more food when moving slower, 0 to test without this effect.
ka    <- 1 # can vary btw 0.9 and 1.3
kn    <- 1.3 # can vary btw 0.9 and 1.3

# ocean growth
f     <- 0.75
g     <- 1.2
c     <- 40
j     <- 0.05

# Risk
Bu    <- c(0.7, 1, 0.7) # B0, B1, B2 (can concatenate because we will loop over behavior choices?)
Ba    <- 1
Bn    <- 1 #can change
Bo    <- 1
Bw    <- 2
M     <- 0.002
m     <- -0.37
ya    <- 1  #can change
yn    <- 1  #can change
yo    <- 1  #can change
P     <- 20


# Parameter names list that each main function requires
params.list <-c("Wc", "A", "t", "U", "Wmax", "Amax",
                "E", "q", "a", "Alpha", "d", "v", "f", "g", "c", "j", "Bu", "Bw", "M", "m", "y", "P", "z", 
                "ya", "yn", "yo", "dn0", "Ba", "Bn", "Bo", "ka", "kn",
                "seeds", "F.vec", "N")