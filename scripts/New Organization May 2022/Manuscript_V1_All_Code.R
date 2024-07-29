# Title: Risks and rewards of habitats shape optimal migration behavior 

# All analyses for manuscript draft


# Outline ----
## Load libraries, functions, and set settings
## Scenario 1: Which ecological factor is most influential affecting the frequency of pauses?
## Scenario 2: What if there were 30% more predators along natural shorelines?
## Scenario 3: How does natural habitat quantity affect behavior and fitness?


## TO TRY AND CHECK

# Scenario 1 with limits on Bn and yn of 0.7; then check summary output to see
# if any have dur == 59. - DONE. yes pretty good, yn = 0.7 still causes 59 duration in river.

# Scenario 3 try with 30% decreases in other parameters & update how
# Fitness is portrayed/explained!

# Okay the higher pauses in altered for low levels of
# are all in A(1) which is altered! 
# Try Scenario 1 on 10 different seed values (then plot grouped by whether they
# A(1) is a vs. n)...
# Did this. Yes, pattern is repeated depending on if A1 is a vs. n
# THE REASON WHY is that when they start in A1, there are OTHER energy benefits
# to pausing! Remember, that when move 0 can eat more and spend the least energy.
# SO I'm pretty sure that's why then only move to N at higher relative foraging gain
# and energy refugia levels...? Still a bit confusing.

#..........................................................................................................................................
# Load libraries and set settings ----
library(abind); library(ggplot2); library(plyr); library(reshape2);library(colorRamps); library(ggpubr)
library(tidyverse); library(purrr)

#remove scientific notation
options(scipen=999)

# Set the pillar width option to Inf to display all columns
options(pillar.width = Inf)

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

# Seeds
seeds <- c(1,2,3,4,6,7)
A1_h <- c("a", "a", "a", "n", "n", "n")


# Make data frame of parameters to iterate over
df <- data.frame(yn = seq(0.5, 1, length.out = 6),  # originally seq(0.1, 1, length.out = 6) but causing unrealistic outmigration behavior.
                 Bn = seq(0.5, 1, length.out = 6),
                 ka = seq(0.9, 1.3, length.out = 6),
                 dn0 = seq(0.5, 1, length.out = 6),
                 seeds = seeds)

df_iters <- expand.grid(df) %>% as_tibble() %>%# expand grid to all combinations.
  filter(!(yn < 1 & Bn < 1 | yn < 1 & ka < 1.3 | yn < 1 & dn0 < 1 |
           Bn < 1 & ka < 1.3 | Bn < 1 & dn0 < 1 | ka < 1.3 & dn0 < 1)) %>%  # only keep rows where one factor is varied at a time
  #filter(!(yn == 1 & Bn == 1 & ka == 1.3 & dn0 == 1)) %>% # drop one row where all are at baseline values
  # make unique iteration id (doesn't matter the order of the numbers)
  group_by(yn, Bn, ka, dn0, seeds) %>%
  mutate(iter_id = cur_group_id()) %>% ungroup()
  
# 6 values per factor (1 is baseline), so 5 combinations per factor with decreasing values (bc throw out when all are at baseline.)
# 5 * 4 = 20 * 6 seeds = 120
length(df_iters$yn) # check if 20
df_iters <- as.data.frame(df_iters) # needs to be a dataframe for indexing to work properly without using pull()

# Start simulation
OUT.SUM <- list() # make object to save function output
OUT.TRACKS <- list()

# Start looping MAIN_FUN over different parameter values
for(i in 1:length(df_iters[,1])) {
  
  OUT <- MAIN_FUN(Wc=Wc, A=A, t=t, U=U, Wmax=Wmax, Amax=Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E=E, a=a, Alpha=Alpha, d=d, v=v, f=f, g=g, c=c, j=j, Bu=Bu, Bw=Bw, M=M, m=m, P=P, z=z, # vars in functions
                  ya=ya, yn=df_iters[i,1], yo=yo, dn0=df_iters[i,4], Ba=Ba, Bn=df_iters[i,2], Bo=Bo, ka=df_iters[i,3], kn=kn, # vars that vary by habitat (h.vec)
                  seeds=df_iters[i,5], F.vec=F.vec, N=N,
                  Wstep.n=Wstep.n, Wstep=Wstep, Wstart_setup=Wstart_setup, tmax=tmax,
                  Ws=Ws, r=r, Smax=Smax)
  
  colnames(OUT[[1]]) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT[[1]] <- bind_cols(OUT[[1]], slice(df_iters[i,], rep(1, 6*6)))
  OUT[[2]] <- bind_cols(OUT[[2]], slice(df_iters[i,], rep(1, 6*60))) # 6 Wstarts * 60 time steps
  
  OUT.SUM[[i]] <- OUT[[1]]     # save the summary outputs: 36 rows per parameter combination: by Wstart, h, and Beh.
  OUT.TRACKS[[i]] <- OUT[[2]]  # save the individual tracks: 60 rows (time steps) per param combo
  
} # end loop.


# Collapse outputs in list format to long dataframes
DF.SUM.1 <- bind_rows(OUT.SUM)
DF.TRACKS.1 <- bind_rows(OUT.TRACKS)

# Save output files
write.csv(DF.SUM.1, "results//Manuscript V4/scenario1_summary-0.5_wseeds.csv") 
write.csv(DF.TRACKS.1, "results//Manuscript V4/scenario1_tracks-0.5_wseeds.csv") 

# Read in saved output files
DF.SUM.1 <- read.csv("results//Manuscript V4/scenario1_summary-0.5_wseeds.csv")
DF.TRACKS.1 <- read.csv("results//Manuscript V4/scenario1_tracks-0.5_wseeds.csv")

# Check for realistic outmigration behavior (duration != 59)
# okay for some this scenario, but note in results!
ggplot(data=DF.SUM.1, aes(x=dur)) + geom_histogram()
DF.SUM.1 %>% filter(dur == 59) %>% select(dur, yn:dn0) %>% distinct()
# remaining problem parameter: yn = 0.79 (maybe try only varying 1-0.8?)


## Figure: Scenario 1 ----

fig1_dat <- DF.SUM.1 %>% as_tibble() %>% 
  mutate(nat_benefit = ifelse(yn == 1 & Bn == 1 & ka == 1.3, (1-dn0)/1*100, 
                              ifelse(yn ==1 & Bn == 1 & dn0 == 1,  (1.3-ka)/1.3*100, 
                                     ifelse(yn == 1 & dn0 == 1 & ka == 1.3, (1-Bn)/1*100, (1-yn)/1*100  )))) 

base_dat <- fig1_dat %>% filter(nat_benefit == 0) %>% mutate(iter_var = "dn0") %>% 
  bind_rows(fig1_dat %>% filter(nat_benefit == 0) %>% mutate(iter_var = "ka")) %>% 
  bind_rows(fig1_dat %>% filter(nat_benefit == 0) %>% mutate(iter_var = "Bn")) %>% 
  bind_rows(fig1_dat %>% filter(nat_benefit == 0) %>% mutate(iter_var = "yn"))


# next make a category for which variable is changing
fig1_dat <- fig1_dat %>% filter(nat_benefit != 0) %>% 
  mutate(iter_var = ifelse(yn == 1 & Bn == 1 & ka == 1.3, "dn0",     
                                                ifelse(yn ==1 & Bn == 1 & dn0 == 1, "ka",
                                                    ifelse(yn == 1 & dn0 == 1 & ka == 1.3, "Bn",
                                                        "yn")))) %>% 
  bind_rows(base_dat) %>% 
  group_by(Beh, h, iter_var, nat_benefit, seeds) %>% 
  summarise(mean.p.tot = mean(p.tot)) %>%   # calculate average total proportion of behaviors by groups
  mutate(seeds_cat = ifelse(seeds <=3, "A1a", "A1n"))
  
# re-order and re-name factor levels
fig1_dat$iter_var <- factor(fig1_dat$iter_var, levels = c("yn", "Bn", "ka", "dn0"))
levels(fig1_dat$iter_var) <- c("(a) Predator abundance", "(b) Salmon vulnerability", 
                               "(c) Foraging gain", "(d) Energy refugia")

fig1_dat$h <- factor(fig1_dat$h)
levels(fig1_dat$h) <- c("Altered", "Natural")

# Make the figure with A1 h=n (main text)
fig_sc1_A1N <- ggplot(filter(fig1_dat, Beh == 0 & seeds == 6), aes(x=nat_benefit, y=mean.p.tot, color=h)) + #linetype = as.factor(seeds)
  geom_line(linewidth=1, alpha=0.7) + geom_point(size=2, alpha=1) + theme_classic() +
  facet_wrap(~iter_var, scales = "free_x") +
  scale_color_manual(values = c("mediumpurple", "forestgreen")) +
  ylab("Proportion of pauses\n(move 0)") + 
  xlab("Percent relative benefit in \nnatural habitats") +
  theme(strip.text.x = element_text(size=11),
        legend.title = element_blank()) +
  ylim(c(0,1)); fig_sc1_A1N

# Save Figure for scenario 1
pdf.options(reset = TRUE, onefile = FALSE)
pdf("results//Manuscript V4/figures/Figure_sc1_A1N.pdf", width=6, height=4)
fig_sc1_A1N
dev.off()


# Make the figure with A1 h=a (appendix)
fig_sc1_A1A <- ggplot(filter(fig1_dat, Beh == 0 & seeds == 1), aes(x=nat_benefit, y=mean.p.tot, color=h)) + #linetype = as.factor(seeds)
  geom_line(linewidth=1, alpha=0.7) + geom_point(size=2, alpha=1) + theme_classic() +
  facet_wrap(~iter_var, scales = "free_x") +
  scale_color_manual(values = c("mediumpurple", "forestgreen")) +
  ylab("Proportion of pauses\n(move 0)") + 
  xlab("Percent relative benefit in \nnatural habitats") +
  theme(strip.text.x = element_text(size=11),
        legend.title = element_blank()) +
  ylim(c(0,1)); fig_sc1_A1A

# Save Figure for scenario 1
pdf.options(reset = TRUE, onefile = FALSE)
pdf("results//Manuscript V4/figures/Figure_sc1_A1A.pdf", width=6, height=4)
fig_sc1_A1A
dev.off()


#..........................................................................................................................................
# Scenario 2: What if 30% more predators? ----

# Set scenario parameters
param_dat <- read.csv("raw-data//Parameter_Iterations_Tracking.csv")

# Choose which "scenario" parameter values you need here: scenario 2
scenario_data <- param_dat %>% filter(scenario == "2")

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


# Make data frame of parameters to iterate over
df <- data.frame(yn = 0.7,
                 Bn = seq(0.1, 1, length.out = 6),
                 ka = seq(0.9, 1.3, length.out = 6),
                 dn0 = seq(0.1, 1, length.out = 6))

df_iters <- expand.grid(df) %>% as_tibble() %>% distinct() %>%  # expand grid to all combinations.
  filter(!(yn == 0.7 & Bn == 1 & ka == 1.3 & dn0 == 1)) %>%          # drop one row where Bn, ka, and dn0 are all at baseline values
  # make unique iteration id (doesn't matter the order of the numbers)
  group_by(yn, Bn, ka, dn0) %>%
  mutate(iter_id = cur_group_id()) %>% ungroup()
  
  # 215 total parameter combinations
length(df_iters$yn) # check if 215
df_iters <- as.data.frame(df_iters) # needs to be a dataframe for indexing to work properly without using pull()

# Start simulation
OUT.SUM <- list() # make object to save function output
OUT.TRACKS <- list()

# Start looping MAIN_FUN over different parameter values
for(i in 1:length(df_iters[,1])) {
  
  OUT <- MAIN_FUN(Wc=Wc, A=A, t=t, U=U, Wmax=Wmax, Amax=Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E=E, a=a, Alpha=Alpha, d=d, v=v, f=f, g=g, c=c, j=j, Bu=Bu, Bw=Bw, M=M, m=m, P=P, z=z, # vars in functions
                  ya=ya, yn=df_iters[i,1], yo=yo, dn0=df_iters[i,4], Ba=Ba, Bn=df_iters[i,2], Bo=Bo, ka=df_iters[i,3], kn=kn, # vars that vary by habitat (h.vec)
                  seeds=seeds, F.vec=F.vec, N=N,
                  Wstep.n=Wstep.n, Wstep=Wstep, Wstart_setup=Wstart_setup, tmax=tmax,
                  Ws=Ws, r=r, Smax=Smax)
  
  colnames(OUT[[1]]) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT[[1]] <- bind_cols(OUT[[1]], slice(df_iters[i,], rep(1, 6*6)))
  OUT[[2]] <- bind_cols(OUT[[2]], slice(df_iters[i,], rep(1, 6*60))) # 6 Wstarts * 60 time steps
  
  OUT.SUM[[i]] <- OUT[[1]]     # save the summary outputs
  OUT.TRACKS[[i]] <- OUT[[2]]  # save the individual tracks
  
} # end loop.


# Collapse outputs in list format to long dataframes
DF.SUM.2 <- bind_rows(OUT.SUM)
DF.TRACKS.2 <- bind_rows(OUT.TRACKS)

# Save output files
write.csv(DF.SUM.2, "results//Manuscript V4/scenario2_summary.csv")
write.csv(DF.TRACKS.2, "results//Manuscript V4/scenario2_tracks.csv")

# Read in saved output files
DF.SUM.2 <- read.csv("results//Manuscript V4/scenario2_summary.csv")
DF.TRACKS.2 <- read.csv("results//Manuscript V4/scenario2_tracks.csv")

## Figure: Scenario 2 ----

# Make category column of how many mechanisms benefit natural.
fig_sc2_df <- DF.SUM.2 %>%  mutate(more_nat_preds_cat = ifelse(ka == 1.3 & Bn == 1 |
                                        ka == 1.3 & dn0 == 1 |
                                        Bn == 1 & dn0 == 1, 1, 
                                        ifelse(ka != 1.3 & Bn != 1 & dn0 != 1,
                                          3, 2)))

nat_preds_cat_N <- fig_sc2_df %>% dplyr::select(Bn, ka, dn0, more_nat_preds_cat) %>% distinct() %>% 
  group_by(more_nat_preds_cat) %>% count()  # this returns the number of iterations in categories: 1, 2, or 3 mechanisms varied at a time.


# Contour plots for every parameter combination

cont_dat <- fig_sc2_df %>% filter(h != "o" & Beh == "0") %>%    # get rid of ocean time steps and focus only on pauses.
  group_by(h, iter_id) %>% summarise(mean = mean(p.tot)) %>%    # get average proportion of pauses by habitat for each iteration.
  pivot_wider(id_cols = iter_id, names_from = h, values_from = mean) %>%
  left_join(df_iters) %>% # maybe don't need?
  mutate(more_n = n - a) %>% 
  mutate(more_n_cat = ifelse(more_n > 0, "more_n", "more_a")) %>% # 1 indicates more natural, 2 indicates more altered
  left_join(fig_sc2_df %>% dplyr::select(iter_id, more_nat_preds_cat) %>% distinct()) #


cont_dat$dn0 <- as.factor(cont_dat$dn0)
cont_dat$Bn <- as.factor(cont_dat$Bn)
cont_dat$ka <- as.factor(cont_dat$ka)


eg1 <- cont_dat %>% filter(iter_id %in% c(1:35)) # example data where Bn and ka vary but dn0 is at baseline value


# This figure shows one example panel of dn0 values while Bn and ka vary.
fig_sc2_ex <- ggplot(data=eg1, aes(x=Bn, y=ka, fill=(more_n))) + geom_tile(width=1, height = 1, color="black") + 
  theme_classic() +
  scale_fill_gradient2(high = "#208B20", low = "#936EDB", name = "Difference in mean proportion\nof pauses (natural - altered)") +
  theme(legend.position = "bottom") + 
  ylab(expression("Foraging gain in altered habitats (k "[a]*")")) + 
  xlab(expression("Salmon escape ability ("* beta[n] *")")) +
  ggtitle(label = "(a)") + theme(plot.title = element_text(size=20)); fig_sc2_ex

# This figure shows the proportion of simulations where pauses are greater in
# altered vs. natural by the number of mechanisms (1, 2, or 3) that have natural benefits.
 
tally_dat <- cont_dat %>% group_by(more_nat_preds_cat, more_n_cat) %>% count() %>% 
  rename(n_by_h = n) %>% 
  left_join(nat_preds_cat_N) %>% mutate(p_by_h = n_by_h / n)
  
# Get summary data for results for text:...
cont_dat %>% group_by(more_n_cat) %>% count() %>% mutate(p = n / 215)
# 51.6 % of all simulations more pauses in altered
# 48.4 % of all simulations more pauses in natural!!!!

fig_sc2_bar <- ggplot(data=tally_dat, aes(x=more_nat_preds_cat, y=p_by_h, fill=more_n_cat)) + 
  geom_bar(stat = "identity", color="black") + theme_classic() +
  scale_y_continuous(expand = c(0,0)) + theme(legend.position = "bottom") +
  scale_fill_manual(values = c("mediumpurple", "forestgreen"), labels = c("Altered", "Natural"), name = "More pauses in:") +
  ylab("Proportion of simulations") + xlab("Number of mechanisms benefiting natural\nhabitats, despite 30% more predators") +
  ggtitle(label = "(b)") + theme(plot.title = element_text(size=20)); fig_sc2_bar

# Arrange these first two plots together
ggarrange(fig_sc2_ex, fig_sc2_bar, align = "hv")

# Save Figure for scenario 2
pdf.options(reset = TRUE, onefile = FALSE)
pdf("results//Manuscript V4/figures/Figure_sc2.pdf", width=7, height=4.5)
ggarrange(fig_sc2_ex, fig_sc2_bar, align = "hv")
dev.off()


# Make Supplemental Figure with contour plot for all possible parameter combinations
fig_sc2_supp <- ggplot(data=cont_dat, aes(x=Bn, y=ka, fill=(more_n))) + geom_tile(width=1, height = 1, color="black") + 
  theme_classic() + facet_wrap(~dn0) +
  scale_fill_gradient2(high = "#208B20", low = "#936EDB", name = "Difference in mean proportion\nof pauses (natural - altered)") +
  theme(legend.position = "bottom") + 
  ylab(expression("Foraging gain in altered habitats (k "[a]*")")) + 
  xlab(expression("Salmon escape ability ("* beta[n] *")")); fig_sc2_supp

# Save Figure
pdf.options(reset = TRUE, onefile = FALSE)
pdf("results//Manuscript V4/figures/Figure_sc2_supp.pdf", width=7, height=6)
fig_sc2_supp
dev.off()



#..........................................................................................................................................
# Scenario 3: How does natural habitat quantity affect behavior and fitness? ----

# Set scenario parameters
param_dat <- read.csv("raw-data//Parameter_Iterations_Tracking.csv")

# Choose which "scenario" parameter values you need here: scenario 2
scenario_data <- param_dat %>% filter(scenario == "3")

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


# Make data frame of parameters to iterate over
df <- data.frame(N = c(0, 0.25, 0.5, 0.75, 1),
                 yn = c(1, 0.8, NA, NA, NA),
                 ya = c(1, 0.8, NA, NA, NA),
                 seeds = c(1, 4, NA, NA, NA))

df_iters <- expand.grid(df) %>% as_tibble() %>% distinct() %>%       # expand grid to all combinations.
  filter(yn == 1 & ya == 1 |
           yn == 0.8 & ya == 1 |
           yn == 1 & ya == 0.8) %>% 
  filter(!(is.na(seeds))) %>% 
  group_by(N, ya, yn) %>%
  mutate(iter_id = cur_group_id()) %>% ungroup()

# 5 total parameter combinations
length(df_iters$N)  # 5 habitat levels * 3 predator abundance levels
df_iters <- as.data.frame(df_iters) # needs to be a dataframe for indexing to work properly without using pull()

# Start simulation
OUT.SUM <- list() # make object to save function output
OUT.TRACKS <- list()

# Start looping MAIN_FUN over different parameter values
for(i in 1:length(df_iters[,1])) {
  
  OUT <- MAIN_FUN(Wc=Wc, A=A, t=t, U=U, Wmax=Wmax, Amax=Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E=E, a=a, Alpha=Alpha, d=d, v=v, f=f, g=g, c=c, j=j, Bu=Bu, Bw=Bw, M=M, m=m, P=P, z=z, # vars in functions
                  ya=df_iters[i,3], yn=df_iters[i,2], yo=yo, dn0=dn0, Ba=Ba, Bn=Bn, Bo=Bo, ka=ka, kn=kn, # vars that vary by habitat (h.vec)
                  seeds=df_iters[i,4], F.vec=F.vec, N=df_iters[i,1],
                  Wstep.n=Wstep.n, Wstep=Wstep, Wstart_setup=Wstart_setup, tmax=tmax,
                  Ws=Ws, r=r, Smax=Smax)
  
  colnames(OUT[[1]]) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT[[1]] <- bind_cols(OUT[[1]], slice(df_iters[i,], rep(1, 6*6)))
  OUT[[2]] <- bind_cols(OUT[[2]], slice(df_iters[i,], rep(1, 6*60))) # 6 Wstarts * 60 time steps
  
  OUT.SUM[[i]] <- OUT[[1]]     # save the summary outputs
  OUT.TRACKS[[i]] <- OUT[[2]]  # save the individual tracks
  
} # end loop.


# Collapse outputs in list format to long dataframes
DF.SUM.3 <- bind_rows(OUT.SUM)
DF.TRACKS.3 <- bind_rows(OUT.TRACKS)

# Save output files
write.csv(DF.SUM.3, "results//Manuscript V4/scenario3_summary_30percent_2seeds.csv")
write.csv(DF.TRACKS.3, "results//Manuscript V4/scenario3_tracks_30percent_2seeds.csv")

# Read in saved output files
DF.SUM.3 <- read.csv("results//Manuscript V4/scenario3_summary_30percent_2seeds.csv")
DF.TRACKS.3 <- read.csv("results//Manuscript V4/scenario3_tracks_30percent_2seeds.csv")

## Figures: Scenario 3 ----

# Gather many summary metrics related to growth and survival, which are 
# potentially useful for disentangling what the model is doing.

# proportion of moves
# duration
# river growth rate
# river total growth
# river survival rate
# river cumulative survival
# ocean growth rate
# ocean total growth
# ocean survival rate
# cumulative survival to T=60
# size at ocean entry
# fitness at T=60 (equivalent to size at T=60)


p_moves_dat <- DF.TRACKS.3 %>% as_tibble() %>% 
  group_by(iter_id, Beh, h) %>% filter(h != "o") %>% 
  summarise(count_moves = n()) %>%   # number of movements in the river between 0, 1, 2.
  ungroup() %>% group_by(iter_id) %>% 
  mutate(tot_moves = sum(count_moves)) %>% 
  mutate(p_moves = count_moves/tot_moves) %>%   # Behavior metric: proportion of moves
  left_join(df_iters)

p_moves_dat

# maybe use this instead because mirrors earlier figure?
p_moves_dat2 <- DF.SUM.3 %>% as_tibble() %>% 
  group_by(Beh, h, iter_id) %>% 
  summarise(mean.p.tot = mean(p.tot)) %>% 
  left_join(df_iters) %>% 
  mutate(yn_ya_cat = str_c(yn, ya, sep = "-"))

# Summarize a lot of data

sum_dat <- DF.SUM.3 %>% group_by(iter_id) %>% summarise(mean_dur = mean(dur),
                                                    mean_G_riv = mean(G.riv),
                                                    mean_S_cum = mean(S.cum.riv),
                                                    mean_Fit = mean(Fit),
                                                    mean_G_riv_d = mean(G.riv/dur),
                                                    mean_G_ocean_d = mean(G.ocean/(60-dur))) %>% 
  left_join(df_iters)

sum_dat <- DF.TRACKS.3 %>% as_tibble() %>% group_by(iter_id, Wstart) %>% 
  filter(h == "o") %>% slice(1) %>% 
  mutate(SOE = W) %>% 
  mutate(G.d.river = ((SOE - Wstart) / Time)) %>% 
  ungroup() %>% group_by(iter_id) %>% 
  summarise(size_oe = mean(SOE)) %>% 
  select(size_oe) %>% 
  bind_cols(sum_dat) %>% relocate(iter_id, .before = size_oe)


sum_dat <- DF.TRACKS.3 %>% as_tibble() %>% group_by(Wstart, iter_id) %>%
  filter(Time == 59) %>% select(Wstart, iter_id, S.cum) %>% 
  group_by(iter_id) %>% summarize(S.cum.T60 = mean(S.cum)) %>% 
  select(S.cum.T60) %>% 
  bind_cols(sum_dat) %>% relocate(iter_id, .before = S.cum.T60)


sum_dat <- DF.TRACKS.3 %>% as_tibble() %>% group_by(Wstart, iter_id) %>%
  filter(Time == 60) %>% select(Wstart, iter_id, W) %>% 
  group_by(iter_id) %>% summarize(W.t60 = mean(W)) %>% 
  select(W.t60) %>% 
  bind_cols(sum_dat) %>% relocate(iter_id, .before = W.t60)

sum_dat <- DF.TRACKS.3 %>% mutate(riv_cat = ifelse(h != "o", "river", "ocean")) %>% 
  group_by(iter_id, riv_cat, Wstart) %>% 
  summarize(mean_S_d = mean(S.day, na.rm=T)) %>% 
  summarize(mean_S_d = mean(mean_S_d, na.rm=T)) %>% 
  pivot_wider(names_from = riv_cat, values_from = mean_S_d) %>% 
  rename(mean_S_riv_d = river, mean_S_ocean_d = ocean) %>% 
  mutate(iter_id = iter_id*100) %>% 
  ungroup() %>% select(-iter_id) %>% 
  bind_cols(sum_dat) %>% relocate(iter_id, .before = mean_S_ocean_d)


sum_dat

sum_dat <- sum_dat %>% mutate(S.cum_times_Fit = S.cum.T60*mean_Fit)

ggplot(data=sum_dat, aes(x = N, y = S.cum_times_Fit)) +
  geom_line() + facet_wrap(~yn+ya, scales = "free")




# Make another style of pmoves figure
ggplot(filter(p_moves_dat2, Beh == 0), aes(x=N, y=mean.p.tot, color=h)) +
  geom_line(size=1, alpha=0.7) + geom_point(size=2, alpha=1) + theme_classic() +
  facet_wrap(~yn_ya_cat, scales = "free_x") +
  scale_color_manual(values = c("mediumpurple", "forestgreen")) +
  ylab("Proportion of pauses\n(move 0)") + 
  xlab("Proportion of natural habitat") +
  theme(strip.text.x = element_text(size=11),
        legend.title = element_blank()) +
  ylim(c(0,1))

# Troubleshooting notes: why higher fitness with 0% natural habitat? this should be
# wrong because if optimal to go fast they should just do the SAME thing and get
# the SAME fitness...why different? Something must be off in the model?

# Pattern is the same across all Wstart values
# Pattern is the same across different summary stats of Fit

# Okay, for some reason once N > 0, then the salmon are pausing basically the whole time
# and are going to the ocean the very last time step (is this because we )

# Ah-ha! Fit is simply the fitness as defined exactly by size, but
# The model is driven by both size at T=60 AND cumulative survival to the end
# of the simulation. So If I plot...

ggplot(data=sum_dat, aes(x = N, y = S.cum_times_Fit)) + theme_classic() +
  geom_line() + facet_wrap(~yn+ya, scales = "free")

#... then they all go UP!!! Except for when there are more predators in natural (which makes sense).

# SO, they must be not going to the ocean because I made the river too "safe" relative
# to the ocean.


### Figure: behavior and fitness ----

fig_sc3_beh <- ggplot(data=sc3_moves_dat, aes(x=as.factor(N*100), y=p_moves, fill=as.factor(Beh))) + 
  geom_bar(stat="identity", color="black") + 
  facet_wrap(~yn + ya) +
  theme_classic() +
  scale_fill_brewer(palette = "Blues", labels = c("0", "1", "2")) + 
  scale_y_continuous(expand = c(0,0), limits=c(0,1), breaks = seq(0,1,by=0.2)) +
  ylab("Proportion of moves") + xlab("Percent of natural habitat") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  ggtitle(label = "(a)") + theme(plot.title = element_text(size=14, face = "bold")); fig_sc3_beh

group_by(Beh, h, iter_var, nat_benefit) %>% 
  summarise(mean.p.tot = mean(p.tot))




sc3_sum_dat <- DF.SUM.3 %>%
  mutate(iter_cat = str_c(yn, ya, sep = "-")) %>% 
  group_by(iter_cat, N) %>% 
  summarise(mean_Fit = mean(Fit))


fig_sc3_fit <- ggplot(data=sc3_sum_dat, aes(x=N, y=mean_Fit, color = iter_cat)) +
  geom_line(size=1, alpha=0.7) + geom_point(size=2, alpha=1) + 
  theme_classic() +
  ylab("Fitness") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(g)") + theme(plot.title = element_text(size=14, face = "bold")); fig_sc3_fit

  
ggplot(data=DF.SUM.3, aes(x= N, y = Fit, color = Wstart)) +
  geom_line() + facet_wrap(~yn+ya)


### Figure: growth and survival ----

sc3_g_dat <- DF.SUM.3 %>%
  mutate(iter_cat = str_c(yn, ya, sep = "-")) %>% 
  group_by(iter_cat, N) %>% 
  summarise(mean_G_riv = mean(G.riv),
            mean_G_riv_d = mean(G.riv/dur),
            mean_G_ocean_d = mean(G.ocean/(60-dur)))



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
