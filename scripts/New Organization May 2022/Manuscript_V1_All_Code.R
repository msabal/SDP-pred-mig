# Title: Risks and rewards of habitats shape optimal migration behavior 

# All analyses for manuscript draft


# Outline ----
# 1. Load libraries and set settings
# 2. Set habitat hypotheses parameters
# 3. Load all functions
# 4. Scenario 1: habitat-hypotheses - do movement choices vary between shoreline habitats?
# 5. Scenario 2: null habitat-hypotheses - which mechanism most affects the frequency of pauses?
# 6. Scenario 3: greater predator abundances in natural - can it ever be optimal to pause in natural despite more predators?
# 7. Scenario 4: habitat restoration - how do movement choices, river growth, survival to ocean, and fitness vary over % of natural habitats?


#..........................................................................................................................................
# 1. Load libraries and set settings ----
library(abind); library(ggplot2); library(plyr); library(reshape2);library(colorRamps); library(ggpubr)
library(tidyverse); library(purrr)

#remove scientific notation
options(scipen=999)


# load needed functions from other R script
source(file = "scripts/New Organization May 2022/Manuscript_All_Functions.R")


#..........................................................................................................................................
# Scenario 1: Which mechanism most affects the frequency of pauses? ----

# Set scenario parameters
param_dat <- read.csv("raw-data//Parameter_Iterations_Tracking.csv")

# Parameters: Assign null parameter values

# Choose which "scenario" parameter values you need here: baseline.
scenario_data <- param_dat %>% filter(scenario == "baseline")

# Convert the filtered row to a list
scenario_list <- as.list(scenario_data)

# Function to convert comma-separated strings to numeric vectors
convert_to_numeric_vector <- function(x) {
  if (is.character(x) && grepl(",", x)) {
    as.numeric(unlist(strsplit(x, ",")))
  } else {
    x
  }
}

# Apply the conversion function to each element in the list
scenario_list <- lapply(scenario_list, convert_to_numeric_vector)

# Assign R objects with the names of the columns in the filtered dataframe
purrr::walk2(names(scenario_list), scenario_list, ~assign(.x, .y, envir = .GlobalEnv))

# Assign a few extras:
Wstep.n         <- ((Wmax-Wmin)/Wstep) # number of increments
Wstart_setup    <- seq(7, 10, length.out = 6) # starting sizes to simulate
Wstart_setup[1] <- 7.1 # needs to be start at 7.1


## FIGURE OUT HOW TO RUN ALL THESE TOGETHER???!!!!


## Predator abundance ##

# Double check
ya # should be 1
yn # we will reduce to simulate fewer predators in natural habitats


# Set iterations
df_iters <- data.frame(yn = c(1, 0.9, 0.7, 0.5, 0.3, 0.1))


# Start simulation
OUT.SUM <- list() # make object to save function output

# Start looping MAIN_FUN over different qa values
for(i in 1:length(df_iters[,1])) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn=df_iters[i,1], yo, dn0, Ba, Bn, Bo, ka, kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, Wstart_setup, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$iter_val <- rep(df_iters[i,(length(colnames(df_iters)))], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.SUM[[i]] <- OUT
  
} # end loop.

DF.SUM<-ldply(OUT.SUM, as.vector)

DF.SUM <- DF.SUM %>% as_tibble() %>% mutate(iter_var = "yn")


## Simulate over Bn

# Double check
Ba # should be 1
Bn # we will reduce to simulate lower mortality (increased survival aka escape ability) in natural habitats


# Set iterations
df_iters <- data.frame(Bn = c(1, 0.9, 0.7, 0.5, 0.3, 0.1))


# Start simulation
OUT.SUM <- list() # make object to save function output

# Start looping MAIN_FUN over different qa values
for(i in 1:length(df_iters[,1])) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0, Ba, Bn=df_iters[i,1], Bo, ka, kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, Wstart_setup, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$iter_val <- rep(df_iters[i,(length(colnames(df_iters)))], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.SUM[[i]] <- OUT
  
} # end loop.

DF.SUM2<-ldply(OUT.SUM, as.vector)

DF.SUM2 <- DF.SUM2 %>% as_tibble() %>% mutate(iter_var = "Bn")

DF.SUM <- DF.SUM %>% bind_rows(DF.SUM2)



## Simulate over dn0

# Double check
d # should be 1
dn0 # we will reduce to simulate lower energy costs (more savings) in natural habitats


# Set iterations
df_iters <- data.frame(dn0 = c(1, 0.9, 0.7, 0.5, 0.3, 0.1))


# Start simulation
OUT.SUM <- list() # make object to save function output

# Start looping MAIN_FUN over different qa values
for(i in 1:length(df_iters[,1])) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0=df_iters[i,1], Ba, Bn, Bo, ka, kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, Wstart_setup, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$iter_val <- rep(df_iters[i,(length(colnames(df_iters)))], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.SUM[[i]] <- OUT
  
} # end loop.

DF.SUM2<-ldply(OUT.SUM, as.vector)

DF.SUM2 <- DF.SUM2 %>% as_tibble() %>% mutate(iter_var = "dn0")

DF.SUM <- DF.SUM %>% bind_rows(DF.SUM2)


## Simulate over ka

# Double check
kn # should be 1
ka # we will reduce food in altered to simulate more food in natural habitats


# Set iterations
df_iters <- data.frame(ka = c(1.3, 1.2, 1.1, 1.0, 0.9))


# Start simulation
OUT.SUM <- list() # make object to save function output

# Start looping MAIN_FUN over different qa values
for(i in 1:length(df_iters[,1])) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0, Ba, Bn, Bo, ka=df_iters[i,1], kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, Wstart_setup, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$iter_val <- rep(df_iters[i,(length(colnames(df_iters)))], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.SUM[[i]] <- OUT
  
} # end loop.

DF.SUM2<-ldply(OUT.SUM, as.vector)

DF.SUM2 <- DF.SUM2 %>% as_tibble() %>% mutate(iter_var = "ka")

DF.SUM <- DF.SUM %>% bind_rows(DF.SUM2)


# Save all data from scenario 2

write.csv(DF.SUM, "C://Users//sabalm//Desktop//scenario2.csv") # UPDATE SAVE LOCATION!
#DF.SUM <- read.csv("C://Users//sabalm//Desktop//scenario2.csv")

DF.SUM <- read.csv("P:/REDD/Personal/Sabal/GIT Repositories/SDP-pred-mig/results/Manuscript V1/scenario2.csv")


## Figure 3 ----

fig3_dat <- DF.SUM %>%
  mutate(rel_nat_index = ifelse(iter_var == "ka", abs(log(1.3/iter_val)),
                                abs(log(1/iter_val)))) %>% 
  group_by(Beh, h, iter_var, rel_nat_index) %>% 
  summarise(mean.p.tot = mean(p.tot))

fig3_dat$iter_var <- factor(fig3_dat$iter_var, levels = c("yn", "Bn", "ka", "dn0"))
levels(fig3_dat$iter_var) <- c("(a) Predator abundance", "(b) Salmon escape ability", 
                               "(c) Foraging gain", "(d) Energy refugia")

fig3_dat$h <- factor(fig3_dat$h)
levels(fig3_dat$h) <- c("Altered", "Natural")


fig3 <- ggplot(filter(fig3_dat, Beh == 0), aes(x=rel_nat_index, y=mean.p.tot, color=h)) +
  geom_line(size=1, alpha=0.7) + geom_point(size=2, alpha=1) + theme_classic() +
  facet_wrap(~iter_var, scales = "free_x") +
  scale_color_manual(values = c("mediumpurple", "forestgreen")) +
  ylab("Proportion of pauses\n(move 0)") + 
  xlab("Relative benefit in \nnatural habitats index") +
  theme(strip.text.x = element_text(size=11),
        legend.title = element_blank()) +
  ylim(c(0,1)); fig3


# theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                    strip.background = element_blank()) +


# Save Figure 3
pdf.options(reset = TRUE, onefile = FALSE)
setwd("C:/Users/sabalm/Desktop/")
pdf("Figure3.pdf", width=6, height=4)

fig3

dev.off()



#..........................................................................................................................................
# 6. Scenario 3: greater predator abundances in natural - can it ever be optimal to pause in natural despite more predators? ----

# Set baseline null habitat-hypotheses parameters
# Parameters: Habitat-hypothesized Differences

# seeds for h.vec
seeds <- param_dat['null','seeds']
N <- param_dat['null','N'] 

# W: salmon weight (g)
Wmin <- param_dat['null','Wmin']
Wmax <- param_dat['null','Wmax'] 
Wstep <- param_dat['null','Wstep']
Wstep.n <- ((Wmax-Wmin)/Wstep)
Wstart_setup <- seq(7, 10, length.out = 6); Wstart_setup[1] <- 7.1 # needs to be start at 7.1

# A: salmon area
Amin <- param_dat['null','Amin']
Amax <- param_dat['null','Amax']

# t: time
tmin <- param_dat['null','tmin']
tmax <- param_dat['null','tmax']
# Behavioral choice
U <- c(0, 1, 2)

# Terminal fitness
Ws    <- param_dat['null','Ws']
r     <- param_dat['null','r']
Smax  <- param_dat['null','Smax']

# Growth
E     <- param_dat['null','E']
a     <- param_dat['null','a']
Alpha <- param_dat['null','Alpha']
d     <- param_dat['null','d']
dn0   <- param_dat['null','dn0']
v     <- param_dat['null','v']

#river growth by speed
z     <- param_dat['null','z']
ka    <- param_dat['null','ka']
kn    <- param_dat['null','kn']

# ocean growth
f     <- param_dat['null','f']
g     <- param_dat['null','g']
c     <- param_dat['null','c']
j     <- param_dat['null','j']

# Risk
Bu    <- c(0.7, 1, 0.7) # B0, B1, B2 (can concatenate because we will loop over behavior choices?)
Ba    <- param_dat['null','Ba']
Bn    <- param_dat['null','Bn']
Bo    <- param_dat['null','Bo']
Bw    <- param_dat['null','Bw']
M     <- param_dat['null','M']
m     <- param_dat['null','m']
ya    <- param_dat['null','ya']
yn    <- param_dat['null','yn']
yo    <- param_dat['null','yo']
P     <- param_dat['null','P']



# Set iterations

ya <- 0.7 # set lower to simulate more predators in natural habitats.
yn        # should be 1

df <- data.frame(Bn = c(1, 0.9, 0.7, 0.5, 0.3, 0.1),
                 ka = c(1.3, 1.2, 1.125, 1.05, 0.975, 0.9),
                 dn0 = c(1, 0.9, 0.7, 0.5, 0.3, 0.1))

df_iters <- expand.grid(df) %>% as_tibble() %>% slice(-1) # expand grid to all combinations but drop top row of all baseline values.
df_iters$iter_index <- seq(1, length(df_iters$Bn), by=1)

df_iters <- as.data.frame(df_iters) # needs to be a dataframe for indexing to work properly without using pull()



# Start simulation
OUT.SUM <- list() # make object to save function output

# Start looping MAIN_FUN over different qa values
for(i in 1:length(df_iters[,1])) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0=df_iters[i,3], Ba, Bn=df_iters[i,1], Bo, ka=df_iters[i,2], kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, Wstart_setup, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$iter_index <- rep(df_iters[i,4], length(OUT$Wstart)) # add column with iter value
  
  OUT.SUM[[i]] <- OUT
  
} # end loop.

DF.SUM<-ldply(OUT.SUM, as.vector)

DF.SUM <- DF.SUM %>% as_tibble() %>% left_join(df_iters)


# Make category column of how many mechanisms benefiting natural.
DF.SUM <- DF.SUM %>% mutate(more_nat_preds_cat = ifelse(ka == 1.3 & Bn == 1 |
                                        ka == 1.3 & dn0 == 1 |
                                        Bn == 1 & dn0 == 1, 1, 
                                        ifelse(ka != 1.3 & Bn != 1 & dn0 != 1,
                                          3, 2)))

nat_preds_cat_N <- DF.SUM %>% dplyr::select(iter_index, Bn, ka, dn0, more_nat_preds_cat) %>% distinct() %>% 
  group_by(more_nat_preds_cat) %>% count()  # this returns the number of iterations in categories: 1, 2, or 3 mechanisms varied at a time.


# Save all data from scenario 3

write.csv(DF.SUM, "C://Users//sabalm//Desktop//scenario3.csv") # UPDATE SAVE LOCATION!
#DF.SUM <- read.csv("C://Users//sabalm//Desktop//scenario3.csv")

DF.SUM <- read.csv("P:/REDD/Personal/Sabal/GIT Repositories/SDP-pred-mig/results/Manuscript V1/scenario3.csv")


## Figure 4 ----

# Contour plots

cont_dat <- DF.SUM %>% filter(h != "o" & Beh == "0") %>%              # get rid of ocean time steps and focus only on pauses.
  group_by(h, iter_index) %>% summarise(mean = mean(p.tot)) %>%       # get average proportion of pauses by habitat for each iteration.
  pivot_wider(id_cols = iter_index, names_from = h, values_from = mean) %>%
  left_join(df_iters) %>% 
  mutate(more_n = n - a) %>% 
  mutate(more_n_cat = ifelse(more_n > 0, "more_n", "more_a")) %>% # 1 indicates more natural, 2 indicates more altered
  left_join(DF.SUM %>% dplyr::select(iter_index, more_nat_preds_cat) %>% distinct())#


cont_dat$dn0 <- as.factor(cont_dat$dn0)
cont_dat$Bn <- as.factor(cont_dat$Bn)
cont_dat$ka <- as.factor(cont_dat$ka)


eg1 <- cont_dat %>% filter(iter_index %in% c(1:35)) # example data where Bn and ka vary but dn0 is at baseline value


# ggplot(data=eg1, aes(x=Bn, y=ka, fill=as.factor(more_n_cat))) + geom_tile(width=1, height = 1, color="black") + 
#   theme_classic() + coord_equal() + 
#   scale_fill_manual(values=c("forestgreen", "mediumpurple"), labels = c("more pauses in natural", "more pauses in altered")) +
#   theme(legend.position = "bottom", legend.title = element_blank()) 
# 

# ggplot(data=eg1, aes(x=Bn, y=ka, fill=as.factor(more_n_cat))) + geom_tile(width=1, height = 1, color="black") + 
#   theme_classic() + coord_equal() + 
#   scale_fill_manual(values=c("gray88", "gray24"), labels = c("more pauses in natural", "more pauses in altered")) +
#   theme(legend.position = "bottom", legend.title = element_blank()) 


fig4a <- ggplot(data=eg1, aes(x=Bn, y=ka, fill=(more_n))) + geom_tile(width=1, height = 1, color="black") + 
  theme_classic() +
  scale_fill_gradient2(high = "#208B20", low = "#936EDB", name = "Difference in mean proportion\nof pauses (natural - altered)") +
  theme(legend.position = "bottom") + 
  ylab("Foraging gain in altered habitats (k)") + xlab("Salmon vulnerability to predation (B)") +
  #ylab(expression(k[a])) + xlab(expression(B[n])) +
  ggtitle(label = "(a)") + theme(plot.title = element_text(size=20)); fig4a
  
#coord_equal() 

 
tally_dat <- cont_dat %>% group_by(more_nat_preds_cat, more_n_cat) %>% count() %>% 
  rename(n_by_h = n) %>% 
  left_join(nat_preds_cat_N) %>% mutate(p_by_h = n_by_h / n)
  
cont_dat %>% group_by(more_n_cat) %>% count() %>% mutate(p = n / 215)
# 51.6 % of all simulations more pauses in altered
# 48.4 % of all simulations more pauses in natural!!!!

fig_4b <- ggplot(data=tally_dat, aes(x=more_nat_preds_cat, y=p_by_h, fill=more_n_cat)) + 
  geom_bar(stat = "identity", color="black") + theme_classic() +
  scale_y_continuous(expand = c(0,0)) + theme(legend.position = "bottom") +
  scale_fill_manual(values = c("mediumpurple", "forestgreen"), labels = c("Altered", "Natural"), name = "More pauses in:") +
  ylab("Proportion of simulations") + xlab("Number of mechanisms benefiting natural\nhabitats, despite 30% more predators") +
  ggtitle(label = "(b)") + theme(plot.title = element_text(size=20)); fig_4b


ggarrange(fig4a, fig_4b, align = "hv")


# Save Figure 4
pdf.options(reset = TRUE, onefile = FALSE)
setwd("C:/Users/sabalm/Desktop/")
pdf("Figure4.pdf", width=7, height=4.5)

ggarrange(fig4a, fig_4b, align = "hv")

dev.off()


## Figure S3

fig_s3 <- ggplot(data=cont_dat, aes(x=Bn, y=ka, fill=(more_n))) + geom_tile(width=1, height = 1, color="black") + 
  theme_classic() + facet_wrap(~dn0) +
  scale_fill_gradient2(high = "#208B20", low = "#936EDB", name = "Difference in mean proportion\nof pauses (natural - altered)") +
  theme(legend.position = "bottom") + 
  ylab(expression(k[a])) + xlab(expression(B[n])); fig_s3

# Save Figure S3
pdf.options(reset = TRUE, onefile = FALSE)
setwd("C:/Users/sabalm/Desktop/")
pdf("Figure_S3.pdf", width=7, height=6)

fig_s3

dev.off()



#..........................................................................................................................................
# 7. Scenario 4: habitat restoration - how do movement choices, river growth, survival to ocean, and fitness vary over % of natural habitats? ----

# Set habitat hypotheses parameters

# Load parameters
param_dat <- read.csv("P://REDD//Personal//Sabal//GIT Repositories//SDP-pred-mig//raw-data//Parameter_Iterations_Tracking.csv")
#param_dat <- read.csv("Parameter_Iterations_Tracking.csv")
row.names(param_dat) <- param_dat$baseline

# Parameters: Habitat-hypothesized Differences

# seeds for h.vec
seeds <- param_dat['habitat_hypoth','seeds']
N <- param_dat['habitat_hypoth','N'] 

# W: salmon weight (g)
Wmin <- param_dat['habitat_hypoth','Wmin']
Wmax <- param_dat['habitat_hypoth','Wmax'] 
Wstep <- param_dat['habitat_hypoth','Wstep']
Wstep.n <- ((Wmax-Wmin)/Wstep)
Wstart_setup <- seq(7, 10, length.out = 6); Wstart_setup[1] <- 7.1 # needs to be start at 7.1

# A: salmon area
Amin <- param_dat['habitat_hypoth','Amin']
Amax <- param_dat['habitat_hypoth','Amax']

# t: time
tmin <- param_dat['habitat_hypoth','tmin']
tmax <- param_dat['habitat_hypoth','tmax']
# Behavioral choice
U <- c(0, 1, 2)

# Terminal fitness
Ws    <- param_dat['habitat_hypoth','Ws']
r     <- param_dat['habitat_hypoth','r']
Smax  <- param_dat['habitat_hypoth','Smax']

# Growth
E     <- param_dat['habitat_hypoth','E']
a     <- param_dat['habitat_hypoth','a']
Alpha <- param_dat['habitat_hypoth','Alpha']
d     <- param_dat['habitat_hypoth','d']
dn0   <- param_dat['habitat_hypoth','dn0']
v     <- param_dat['habitat_hypoth','v']

#river growth by speed
z     <- param_dat['habitat_hypoth','z']
ka    <- param_dat['habitat_hypoth','ka']
kn    <- param_dat['habitat_hypoth','kn']

# ocean growth
f     <- param_dat['habitat_hypoth','f']
g     <- param_dat['habitat_hypoth','g']
c     <- param_dat['habitat_hypoth','c']
j     <- param_dat['habitat_hypoth','j']

# Risk
Bu    <- c(0.7, 1, 0.7) # B0, B1, B2 (can concatenate because we will loop over behavior choices?)
Ba    <- param_dat['habitat_hypoth','Ba']
Bn    <- param_dat['habitat_hypoth','Bn']
Bo    <- param_dat['habitat_hypoth','Bo']
Bw    <- param_dat['habitat_hypoth','Bw']
M     <- param_dat['habitat_hypoth','M']
m     <- param_dat['habitat_hypoth','m']
ya    <- param_dat['habitat_hypoth','ya']
yn    <- param_dat['habitat_hypoth','yn']
yo    <- param_dat['habitat_hypoth','yo']
P     <- param_dat['habitat_hypoth','P']



# 
## Amount of natural habitat (N) ##


# Set iterations
df_iters <- data.frame(N = c(0, 0.25, 0.5, 0.75, 1))


# Start simulation
OUT.SUM <- list() # make object to save function output

# Start looping MAIN_FUN over different qa values
for(i in 1:length(df_iters[,1])) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0, Ba, Bn, Bo, ka, kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, Wstart_setup, tmax, seeds, F.vec, N=df_iters[i,1])
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$iter_val <- rep(df_iters[i,(length(colnames(df_iters)))], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.SUM[[i]] <- OUT
  
} # end loop.

DF.SUM<-ldply(OUT.SUM, as.vector)

DF.SUM <- DF.SUM %>% as_tibble() %>% mutate(iter_var = "N")


# Save all data from scenario 4

write.csv(DF.SUM, "C://Users//sabalm//Desktop//scenario4.csv") # UPDATE SAVE LOCATION!
DF4 <- read.csv("C://Users//sabalm//Desktop//scenario4.csv")

DF4 <- read.csv("P://REDD//Personal//Sabal//GIT Repositories//SDP-pred-mig//results//Manuscript V1//scenario4.csv")



## Run again but get the tracks

# Set iterations
df_iters <- data.frame(N = c(0, 0.25, 0.5, 0.75, 1))


# Start simulation
OUT.SUM <- list() # make object to save function output

# Start looping MAIN_FUN over different qa values
for(i in 1:length(df_iters[,1])) {
  
  OUT <- MAIN_FUN_TRACKS(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0, Ba, Bn, Bo, ka, kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, Wstart_setup, tmax, seeds, F.vec, N=df_iters[i,1])
  
  OUT$iter_val <- rep(df_iters[i,(length(colnames(df_iters)))], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.SUM[[i]] <- OUT
  
} # end loop.

DF.SUM<-ldply(OUT.SUM, as.vector)

DF.SUM <- DF.SUM %>% as_tibble() %>% mutate(iter_var = "N")


# Save all data from scenario 4

write.csv(DF.SUM, "C://Users//sabalm//Desktop//scenario4_tracks.csv") # UPDATE SAVE LOCATION!
#DF4_tracks <- read.csv("C://Users//sabalm//Desktop//scenario4_tracks.csv")
#DF4_tracks <- read.csv("G://My Drive//Professional//GIT Repositories//SDP-pred-mig//results//Manuscript V1//scenario4_tracks.csv")
DF4_tracks <- read.csv("P://REDD//Personal//Sabal//GIT Repositories//SDP-pred-mig//results//Manuscript V1//scenario4_tracks.csv")


## Figure 5 ----

# a - proportion of moves
# b - river growth rate
# c - river survival rate
# d - duration
# e - size at ocean entry
# f - cumulative survial to the ocean
# g - fitness
# h - size at t=60
# i - cumulative survival to t = 60

# SI figure
# a - ocean growth rate
# b - ocean survival rate

#..................................
# a - proportion of moves ----

moves_dat <- DF4_tracks %>% as_tibble() %>% group_by(iter_val, Beh) %>% filter(h != "o") %>% 
  summarise(count_moves = n()) %>%   # number of movements in teh river between 0, 1, 2.
  ungroup() %>% group_by(iter_val) %>% 
  mutate(tot_moves = sum(count_moves)) %>% 
  mutate(p_moves = count_moves/tot_moves,
         iter_val = iter_val*100)

fig_pmoves <- ggplot(data=moves_dat, aes(x=as.factor(iter_val), y=p_moves, fill=as.factor(Beh))) + 
  geom_bar(stat="identity", color="black") + 
  theme_classic() +
  scale_fill_brewer(palette = "Blues", labels = c("0", "1", "2")) + 
  scale_y_continuous(expand = c(0,0), limits=c(0,1), breaks = seq(0,1,by=0.2)) +
  ylab("Proportion of moves") + xlab("Percent of natural habitat") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  ggtitle(label = "(a)") + theme(plot.title = element_text(size=14, face = "bold")); fig_pmoves


# Summarize metrics over starting salmon weights.

sub_dat <- DF4 %>% group_by(iter_val) %>% summarise(mean_dur = mean(dur),
                                                    mean_G_riv = mean(G.riv),
                                                    mean_S_cum = mean(S.cum.riv),
                                                    mean_Fit = mean(Fit),
                                                    mean_G_riv_d = mean(G.riv/dur),
                                                    mean_G_ocean_d = mean(G.ocean/(60-dur))) %>% 
  mutate(iter_val = iter_val*100)


sub_dat <- DF4_tracks %>% as_tibble() %>% group_by(iter_val, Wstart) %>% 
  filter(h == "o") %>% slice(1) %>% 
  mutate(SOE = W) %>% 
  mutate(G.d.river = ((SOE - Wstart) / Time)) %>% 
  ungroup() %>% group_by(iter_val) %>% 
  summarise(size_oe = mean(SOE)) %>% 
  select(size_oe) %>% 
  bind_cols(sub_dat) %>% relocate(iter_val, .before = size_oe)


sub_dat <- DF4_tracks %>% as_tibble() %>% group_by(Wstart, iter_val) %>%
  filter(Time == 59) %>% select(Wstart, iter_val, S.cum) %>% 
  group_by(iter_val) %>% summarize(S.cum.T60 = mean(S.cum)) %>% 
  select(S.cum.T60) %>% 
  bind_cols(sub_dat) %>% relocate(iter_val, .before = S.cum.T60)


sub_dat <- DF4_tracks %>% as_tibble() %>% group_by(Wstart, iter_val) %>%
  filter(Time == 60) %>% select(Wstart, iter_val, W) %>% 
  group_by(iter_val) %>% summarize(W.t60 = mean(W)) %>% 
  select(W.t60) %>% 
  bind_cols(sub_dat) %>% relocate(iter_val, .before = W.t60)

sub_dat <- DF4_tracks %>% mutate(riv_cat = ifelse(h != "o", "river", "ocean")) %>% 
  group_by(iter_val, riv_cat, Wstart) %>% 
  summarize(mean_S_d = mean(S.day, na.rm=T)) %>% 
  summarize(mean_S_d = mean(mean_S_d, na.rm=T)) %>% 
  pivot_wider(names_from = riv_cat, values_from = mean_S_d) %>% 
  rename(mean_S_riv_d = river, mean_S_ocean_d = ocean) %>% 
  mutate(iter_val = iter_val*100) %>% 
  ungroup() %>% select(-iter_val) %>% 
  bind_cols(sub_dat) %>% relocate(iter_val, .before = mean_S_ocean_d)


sub_dat # look at all summary values by percent natural habitat.


# b - river growth rate ----
fig_rg <- ggplot(data=sub_dat, aes(x=iter_val, y=mean_G_riv_d)) +
  geom_line(size=1, alpha=0.7, color="gray24") + geom_point(size=2, alpha=1, color="gray24") + 
  theme_classic() +
  ylab("River growth rate (g/d)") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(b)") + theme(plot.title = element_text(size=14, face = "bold")); fig_rg


# c - river survival rate ----
fig_rs <- ggplot(data=sub_dat, aes(x=iter_val, y=mean_S_riv_d)) +
  geom_line(size=1, alpha=0.7, color="gray24") + geom_point(size=2, alpha=1, color="gray24") + 
  theme_classic() +
  ylab("River survival rate") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(c)") + theme(plot.title = element_text(size=14, face = "bold")); fig_rs


# d - duration ----
fig_dur <- ggplot(data=sub_dat, aes(x=iter_val, y=mean_dur)) +
  geom_line(size=1, alpha=0.7, color="gray24") + geom_point(size=2, alpha=1, color="gray24") + 
  theme_classic() +
  ylab("Duration (d)") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(d)") + theme(plot.title = element_text(size=14, face = "bold")); fig_dur


# e - size at ocean entry ----
fig_Woe <- ggplot(data=sub_dat, aes(x=iter_val, y=size_oe)) +
  geom_line(size=1, alpha=0.7, color="gray24") + geom_point(size=2, alpha=1, color="gray24") + 
  theme_classic() +
  ylab("Size at ocean entry (g)") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(e)") + theme(plot.title = element_text(size=14, face = "bold")); fig_Woe


# f - cumulative survival to the ocean ----
fig_cumS_o <- ggplot(data=sub_dat, aes(x=iter_val, y=mean_S_cum)) +
  geom_line(size=1, alpha=0.7, color="gray24") + geom_point(size=2, alpha=1, color="gray24") + 
  theme_classic() +
  ylab("Cumulative survival to ocean") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(f)") + theme(plot.title = element_text(size=14, face = "bold")); fig_cumS_o


# g - fitness ----
fig_fit <- ggplot(data=sub_dat, aes(x=iter_val, y=mean_Fit)) +
  geom_line(size=2, alpha=0.7, color="gray24") + geom_point(size=4, alpha=1, color="gray24") + 
  theme_classic() +
  ylab("Fitness") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(g)") + theme(plot.title = element_text(size=14, face = "bold")); fig_fit


# h - size at t=60 ----
fig_W60 <- ggplot(data=sub_dat, aes(x=iter_val, y=W.t60)) +
  geom_line(size=1, alpha=0.7, color="gray24") + geom_point(size=2, alpha=1, color="gray24") + 
  theme_classic() +
  ylab("Size at T=60 (g)") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(h)") + theme(plot.title = element_text(size=14, face = "bold")); fig_W60


# i - cumulative survival to t = 60 ----
fig_cumS_60 <- ggplot(data=sub_dat, aes(x=iter_val, y=S.cum.T60)) +
  geom_line(size=1, alpha=0.7, color="gray24") + geom_point(size=2, alpha=1, color="gray24") + 
  theme_classic() +
  ylab("Cumulative survival to T=60") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(i)") + theme(plot.title = element_text(size=14, face = "bold")); fig_cumS_60


# Save Figure 5
theme_transparent <- theme(
  panel.background = element_rect(fill = "transparent",colour = NA),
  panel.grid.minor = element_blank(), 
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill = "transparent", color = NA))


# With transparent backgrounds!
ggsave(
  plot = ggarrange(fig_pmoves + theme_transparent, 
                   fig_rg + theme_transparent, 
                   fig_rs + theme_transparent,
                   fig_dur + theme_transparent, 
                   fig_Woe + theme_transparent, 
                   fig_cumS_o + theme_transparent,
                   fig_fit + theme_transparent, 
                   fig_W60 + theme_transparent, 
                   fig_cumS_60 + theme_transparent,
                   ncol = 3, nrow = 3),
  filename = "Figure5.png",
  bg = "transparent",
  width = 8, height = 7, units = "in",
  dpi = 500
)




# SI figure 4 ----
# a - ocean growth rate
fig_g.d.ocean <- ggplot(data=sub_dat, aes(x=iter_val, y=mean_G_ocean_d)) +
  geom_line(size=1, alpha=0.7, color="gray24") + geom_point(size=2, alpha=1, color="gray24") + 
  theme_classic() +
  ylab("Ocean growth rate (g/d)") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(a)") + theme(plot.title = element_text(size=14, face = "bold")); fig_g.d.ocean 

# b - ocean survival rate
fig_s.d.ocean <- ggplot(data=sub_dat, aes(x=iter_val, y=mean_S_ocean_d)) +
  geom_line(size=1, alpha=0.7, color="gray24") + geom_point(size=2, alpha=1, color="gray24") + 
  theme_classic() +
  ylab("Ocean survival rate") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(b)") + theme(plot.title = element_text(size=14, face = "bold")); fig_s.d.ocean

# Save SI Figure 4
pdf("Figure_S4.pdf", width=6, height=2.5)
ggarrange(fig_g.d.ocean, fig_s.d.ocean, ncol=2)
dev.off()






# Potential SI figure plots ...............................................................................................................

# plot Terminal Fitness function


curve(TERM.FUN(W, Ws=Ws , r=r, Smax=Smax), xname="W", xlim=c(7,80), ylim=c(0,0.31), ylab="adult marine survival (to age 3)")
