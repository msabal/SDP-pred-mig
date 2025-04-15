# Title: Habitat restoration shapes risk-reward tradeoffs, movement behavior,
# and fitness in migrating juvenile salmon

# Authors:
# Megan C. Sabal (code creator)
# Suzanne H. Alonzo
# Steven T. Lindley
# Joseph E. Merz
# Paul G. Carvalho
# Eric P. Palkovacs


# All analyses for manuscript draft


# Outline ----
## Load libraries, functions, and set settings
## Scenario 1: Which ecological factor is most influential affecting the frequency of pauses?
## Scenario 2: What if there were 15% more predators along natural shorelines?
## Scenario 3: How does natural habitat quantity affect behavior and fitness?


#..........................................................................................................................................
# Load libraries and set settings ----
library(abind); library(ggplot2); library(plyr); library(reshape2);library(colorRamps); library(ggpubr)
library(tidyverse); library(purrr)

#remove scientific notation
options(scipen=999)

# Set the pillar width option to Inf to display all columns
options(pillar.width = Inf)

# load needed functions from other R script
source(file = "scripts/Sabal et al. Manuscript Submission/Manuscript_All_Functions.R")


#..........................................................................................................................................
# Scenario 1: Which mechanism most affects the frequency of pauses? ----

# Set scenario parameters
param_dat <- read.csv("scripts/Sabal et al. Manuscript Submission/Parameters_Scenarios.csv")

# Parameters: Assign null parameter values

# Choose which "scenario" parameter values you need here: baseline.
scenario_data <- param_dat %>% filter(scenario == 1)

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
pdf("results//Manuscript V5/figures/Figure_S5.pdf", width=6, height=4)
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
pdf("results//Manuscript V4/figures/Figure_2.pdf", width=6, height=4)
fig_sc1_A1A
dev.off()


#..........................................................................................................................................
# Scenario 2: What if 30% more predators? ----

# Set scenario parameters
param_dat <- read.csv("scripts/Sabal et al. Manuscript Submission/Parameters_Scenarios.csv")

# Choose which "scenario" parameter values you need here: scenario 2
scenario_data <- param_dat %>% filter(scenario == 2)

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
seeds <- c(1, 4)
A1_h <- c("a", "n")


# Make data frame of parameters to iterate over
df <- data.frame(ya = (1-(1*0.15)),    # reduce by 0.15 (15%)
                 Bn = seq(0.5, 1, length.out = 6),
                 ka = seq(0.9, 1.3, length.out = 6),
                 dn0 = seq(0.5, 1, length.out = 6),
                 seeds = seeds)

df_iters <- expand.grid(df) %>% as_tibble() %>% distinct() %>%  # expand grid to all combinations.
  #filter(!(yn == 0.7 & Bn == 1 & ka == 1.3 & dn0 == 1)) %>%          # drop one row where Bn, ka, and dn0 are all at baseline values
  # make unique iteration id (doesn't matter the order of the numbers)
  group_by(ya, Bn, ka, dn0, seeds) %>%
  mutate(iter_id = cur_group_id()) %>% ungroup()
  
  # 215 total parameter combinations
length(df_iters$ya) # check if 215
df_iters <- as.data.frame(df_iters) # needs to be a dataframe for indexing to work properly without using pull()

# Start simulation
OUT.SUM <- list() # make object to save function output
OUT.TRACKS <- list()

# Start looping MAIN_FUN over different parameter values
for(i in 1:length(df_iters[,1])) {
  
  OUT <- MAIN_FUN(Wc=Wc, A=A, t=t, U=U, Wmax=Wmax, Amax=Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E=E, a=a, Alpha=Alpha, d=d, v=v, f=f, g=g, c=c, j=j, Bu=Bu, Bw=Bw, M=M, m=m, P=P, z=z, # vars in functions
                  ya=df_iters[i,1], yn=yn, yo=yo, dn0=df_iters[i,4], Ba=Ba, Bn=df_iters[i,2], Bo=Bo, ka=df_iters[i,3], kn=kn, # vars that vary by habitat (h.vec)
                  seeds=df_iters[i,5], F.vec=F.vec, N=N,
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
write.csv(DF.SUM.2, "results//Manuscript V4/scenario2_summary_15per_2seeds.csv")
write.csv(DF.TRACKS.2, "results//Manuscript V4/scenario2_tracks_15per_2seeds.csv")

# Read in saved output files
DF.SUM.2 <- read.csv("results//Manuscript V4/scenario2_summary_15per_2seeds.csv")
DF.TRACKS.2 <- read.csv("results//Manuscript V4/scenario2_tracks_15per_2seeds.csv")

## Figure: Scenario 2 ----

# Make category column of how many mechanisms benefit natural.
fig_sc2_df <- DF.SUM.2 %>%  mutate(more_nat_preds_cat = ifelse(ka == 1.3 & Bn == 1 |
                                        ka == 1.3 & dn0 == 1 |
                                        Bn == 1 & dn0 == 1, 1, 
                                        ifelse(ka != 1.3 & Bn != 1 & dn0 != 1,
                                          3, 2))) %>% 
  filter(!(ka == 1.3 & dn0 == 1 & Bn == 1))  # drop scenarios where all other mechanisms are at baseline values.

nat_preds_cat_N <- fig_sc2_df %>% dplyr::select(Bn, ka, dn0, seeds, more_nat_preds_cat) %>% distinct() %>% 
  group_by(more_nat_preds_cat) %>% count()  # this returns the number of iterations in categories: 1, 2, or 3 mechanisms varied at a time.


# Contour plots for every parameter combination
#sub_dat <- fig_sc2_df %>% filter(seeds == 4)  # seeds == 1 is altered A1, seeds == 4 is natural A1
sub_dat <- fig_sc2_df  # plot all parameters combos including both seeds values together.

cont_dat <- sub_dat %>% filter(h != "o" & Beh == "0") %>%    # get rid of ocean time steps and focus only on pauses.
  group_by(h, iter_id) %>% summarise(mean = mean(p.tot)) %>%    # get average proportion of pauses by habitat for each iteration.
  pivot_wider(id_cols = iter_id, names_from = h, values_from = mean) %>%
  left_join(df_iters) %>% # maybe don't need?
  mutate(more_n = n - a) %>% 
  mutate(more_n_cat = ifelse(more_n > 0, "more_n", "more_a")) %>% # 1 indicates more natural, 2 indicates more altered
  left_join(sub_dat %>% dplyr::select(iter_id, more_nat_preds_cat) %>% distinct()) #


cont_dat$dn0 <- as.factor(cont_dat$dn0)
cont_dat$Bn <- as.factor(cont_dat$Bn)
cont_dat$ka <- as.factor(cont_dat$ka)

#Single example for Figure 3 from full Figure S6
eg1 <- cont_dat %>% 
  filter(dn0 == 0.7 & seeds == 4) # example data: across all Bn and ka and one dn0 and seeds value.


# This figure shows one example panel of dn0 values while Bn and ka vary.
fig_sc2_ex <- ggplot(data=eg1, aes(x=Bn, y=ka, fill=(more_n))) + geom_tile(width=1, height = 1, color="black") + 
  theme_classic() +
  scale_fill_gradient2(high = "#208B20", low = "#936EDB", name = "Difference in mean proportion\nof pauses (natural - altered)") +
  theme(legend.position = "bottom") + 
  ylab(expression("Foraging gain in altered habitats (k "[a]*")")) + 
  xlab(expression("Salmon vulnerability ("* beta[n] *")")) +
  ggtitle(label = "(a)") + theme(plot.title = element_text(size=20)); fig_sc2_ex

# This figure shows the proportion of simulations where pauses are greater in
# altered vs. natural by the number of mechanisms (1, 2, or 3) that have natural benefits.
 
tally_dat <- cont_dat %>% group_by(more_nat_preds_cat, more_n_cat) %>% count() %>% 
  rename(n_by_h = n) %>% 
  left_join(nat_preds_cat_N) %>% mutate(p_by_h = n_by_h / n)
  
# Get summary data for results for text:...
cont_dat %>% group_by(more_n_cat) %>% count() %>% mutate(p = n / 430)
# 39.8 % of all simulations more pauses in altered
# 60.2 % of all simulations more pauses in natural!!!!

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
pdf("results//Manuscript V5/figures/Figure_3.pdf", width=7, height=4.5)
ggarrange(fig_sc2_ex, fig_sc2_bar, align = "hv")
dev.off()


# Make Supplemental Figure with contour plot for all possible parameter combinations
fig_sc2_supp <- ggplot(data=cont_dat, aes(x=Bn, y=ka, fill=(more_n))) + geom_tile(width=1, height = 1, color="black") + 
  theme_classic() + facet_wrap(~dn0) +
  scale_fill_gradient2(high = "#208B20", low = "#936EDB", name = "Difference in mean proportion\nof pauses (natural - altered)") +
  theme(legend.position = "bottom") + 
  ylab(expression("Foraging gain in altered habitats (k "[a]*")")) + 
  xlab(expression("Salmon vulnerability ("* beta[n] *")")); fig_sc2_supp

# Save Figure
pdf.options(reset = TRUE, onefile = FALSE)
pdf("results//Manuscript V5/figures/Figure_S6.pdf", width=7, height=6)
fig_sc2_supp
dev.off()



#..........................................................................................................................................
# Scenario 3: How does natural habitat quantity affect behavior and fitness? ----

# Set scenario parameters
param_dat <- read.csv("scripts/Sabal et al. Manuscript Submission/Parameters_Scenarios.csv")

# Choose which "scenario" parameter values you need here: scenario 2
scenario_data <- param_dat %>% filter(scenario == 3)

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
                 yn = c(1, 0.85, NA, NA, NA),
                 ya = c(1, 0.85, NA, NA, NA),
                 seeds = 4,
                 dn0 = 0.7,
                 ka = 0.9,
                 Bn = 0.7)

df_iters <- expand.grid(df) %>% as_tibble() %>% distinct() %>%       # expand grid to all combinations.
  filter(yn == 1 & ya == 1 |                # get only the three predator abundance scenarios i care about
           yn == 0.85 & ya == 1 |
           yn == 1 & ya == 0.85) %>%
  group_by(N, ya, yn) %>%
  mutate(iter_id = cur_group_id()) %>% ungroup() %>% 
  mutate(iter_name = ifelse(yn == 1 & ya == 1, "equal_preds",
                            ifelse(yn == 0.85, "more_preds_in_altered",
                                   "more_preds_in_natural")))

# 15 total parameter combinations
length(df_iters$N)  # 5 habitat levels * 3 predator abundance levels
df_iters <- as.data.frame(df_iters) # needs to be a dataframe for indexing to work properly without using pull()

# Start simulation
OUT.SUM <- list() # make object to save function output
OUT.TRACKS <- list()

# Start looping MAIN_FUN over different parameter values
for(i in 1:length(df_iters[,1])) {
  
  OUT <- MAIN_FUN(Wc=Wc, A=A, t=t, U=U, Wmax=Wmax, Amax=Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E=E, a=a, Alpha=Alpha, d=d, v=v, f=f, g=g, c=c, j=j, Bu=Bu, Bw=Bw, M=M, m=m, P=P, z=z, # vars in functions
                  ya=df_iters[i,3], yn=df_iters[i,2], yo=yo, dn0=df_iters[i,5], Ba=Ba, Bn=df_iters[i,7], Bo=Bo, ka=df_iters[i,6], kn=kn, # vars that vary by habitat (h.vec)
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
write.csv(DF.SUM.3, "results//Manuscript V4/scenario3_summary_30percent_1seed.csv")
write.csv(DF.TRACKS.3, "results//Manuscript V4/scenario3_tracks_30percent_1seed.csv")

# Read in saved output files
DF.SUM.3 <- read.csv("results//Manuscript V4/scenario3_summary_30percent_1seed.csv")
DF.TRACKS.3 <- read.csv("results//Manuscript V4/scenario3_tracks_30percent_1seed.csv")

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


# proportion of moves (not habitat-specific)

p_moves_dat <- DF.TRACKS.3 %>% as_tibble() %>% 
  group_by(iter_id, Beh) %>% filter(h != "o") %>% 
  tally(name = "count_moves") %>%    # number of movements in the river between 0, 1, 2.
  ungroup() %>% group_by(iter_id) %>% 
  mutate(tot_moves = sum(count_moves)) %>% 
  mutate(p_moves = count_moves/tot_moves) %>%   # Behavior metric: proportion of moves
  left_join(df_iters)

p_moves_dat

# Get...
sum_dat <- DF.SUM.3 %>% group_by(iter_id) %>% 
  summarise(mean_dur = mean(dur),                         # duration
            mean_G_riv = mean(G.riv),                     # total growth in river
            mean_S_cum = mean(S.cum.riv),                 # cum surv to ocean entry
            mean_Fit = mean(Fit),                         # Fitness from T60
            mean_G_riv_d = mean(G.riv/dur),               # growth rate in river
            mean_G_ocean_d = mean(G.ocean/(60-dur))) %>%  # growth rate in ocean
  left_join(df_iters)

# Get more...

sum_dat <- DF.TRACKS.3 %>% as_tibble() %>% group_by(iter_id, Wstart) %>% 
  filter(h == "o") %>% slice(1) %>% 
  mutate(SOE = W) %>% 
  #mutate(G.d.river = ((SOE - Wstart) / Time)) %>% 
  ungroup() %>% group_by(iter_id) %>% 
  summarise(size_oe = mean(SOE)) %>%                  # mean size (g) at ocean entry
  left_join(sum_dat)

# Get more...
sum_dat <- DF.TRACKS.3 %>% as_tibble() %>% group_by(Wstart, iter_id) %>%
  filter(Time == 59) %>% select(Wstart, W, iter_id, S.cum) %>% 
  group_by(iter_id) %>% summarize(S.cum.T60 = mean(S.cum),    # cum survival to end of simulation (t=59)
                                  W.T60 = mean(W)) %>%        # size at end of simulation (t=59)
  left_join(sum_dat)


sum_dat <- DF.TRACKS.3 %>% mutate(riv_cat = ifelse(h != "o", "river", "ocean")) %>% 
  group_by(iter_id, riv_cat) %>%
  summarize(mean_S_d = mean(S.day, na.rm = T)) %>%              # FYI NaN values for iter_ids (5, 8, 11, 14) are all more preds in altered and its because they say in the river until the last possible time step bc natural river is just too good. probably don't show these in the paper because then not realistic outmigration behavior.
  pivot_wider(names_from = riv_cat, values_from = mean_S_d) %>% 
  rename(mean_S_riv_d = river,          # mean survival rate in river
         mean_S_ocean_d = ocean) %>%    # mean survival rate in ocean
  left_join(sum_dat)
  
# And finally...

sum_dat <- sum_dat %>% 
  mutate(mean_G_ocean = (W.T60 - size_oe),        # mean ocean growth (g)
         Fit_cumsurv = mean_Fit * S.cum.T60) %>%  # "true" fitness: fit at t60 * cumsurv to t59
  left_join(sum_dat)



# For SI: when equal_preds between altered and natural

p_moves_dat2 <- p_moves_dat %>% filter(iter_name == "equal_preds")
sc3_dat_l <- sum_dat %>% filter(iter_name == "equal_preds")  # in long format


# Figure #: behavior and fitness ----

fig_sc3_beh <- ggplot(data=p_moves_dat2, 
                      aes(x=as.factor(N*100), y=p_moves, fill=as.factor(Beh))) + 
  geom_bar(stat="identity", color="black") + 
  theme_classic() +
  scale_fill_brewer(palette = "Blues", labels = c("0", "1", "2")) + 
  scale_y_continuous(expand = c(0,0), limits=c(0,1), breaks = seq(0,1,by=0.2)) +
  ylab("Proportion of moves") + xlab("Percent of natural habitat") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  ggtitle(label = "(a)") + theme(plot.title = element_text(size=14, face = "bold")); fig_sc3_beh

# (b) Migration duration
fig_sc3_dur<- ggplot(data=sc3_dat_l, aes(x=(N*100), y=mean_dur)) +
  geom_line(size=1, alpha=0.7) + geom_point(size=2, alpha=1) + 
  theme_classic() +
  ylab("Migration duration (days)") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(b)") + theme(plot.title = element_text(size=14, face = "bold")); fig_sc3_dur

# (c) Fitness
fig_sc3_fit <- ggplot(data=sc3_dat_l, aes(x=(N*100), y=Fit_cumsurv)) +
  geom_line(size=1, alpha=0.7) + geom_point(size=2, alpha=1) + 
  theme_classic() +
  ylab("Fitness") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(c)") + theme(plot.title = element_text(size=14, face = "bold")); fig_sc3_fit

  
# Together
ggarrange(fig_sc3_beh, fig_sc3_dur, fig_sc3_fit, ncol = 1)

# Save Figure for scenario 3
pdf.options(reset = TRUE, onefile = FALSE)
pdf("results//Manuscript V5/figures/Figure_S7.pdf", width=4, height=8)
ggarrange(fig_sc3_beh, fig_sc3_dur, fig_sc3_fit, ncol = 1, align = "hv")
dev.off()




### Figure #: growth and survival ----

# growth rate
sc3_dat_w1 <- sc3_dat_l %>% select(iter_id, N, 
                                  mean_G_riv_d, mean_G_ocean_d) %>% 
  pivot_longer(cols = c(mean_G_riv_d, mean_G_ocean_d),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "mean_G_riv_d", "river", "ocean"),
         metric = "growth_rate")

# growth
sc3_dat_w2 <- sc3_dat_l %>% select(iter_id, N, 
                                   mean_G_riv, mean_G_ocean) %>% 
  pivot_longer(cols = c(mean_G_riv, mean_G_ocean),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "mean_G_riv", "river", "ocean"),
         metric = "growth_g")

# size
sc3_dat_w3 <- sc3_dat_l %>% select(iter_id, N, 
                                   size_oe, W.T60) %>% 
  pivot_longer(cols = c(size_oe, W.T60),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "size_oe", "river", "ocean"),
         metric = "size_g")

# cumulative survival
sc3_dat_w4 <- sc3_dat_l %>% select(iter_id, N, 
                                   mean_S_cum, S.cum.T60) %>% 
  pivot_longer(cols = c(mean_S_cum, S.cum.T60),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "mean_S_cum", "river", "ocean"),
         metric = "cum_surv")

# survival rate
sc3_dat_w5 <- sc3_dat_l %>% select(iter_id, N, 
                                   mean_S_riv_d, mean_S_ocean_d) %>% 
  pivot_longer(cols = c(mean_S_riv_d, mean_S_ocean_d),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "mean_S_riv_d", "river", "ocean"),
         metric = "surv_rate")

# Put all long data together: raw metric values!
sc3_dat_w <- sc3_dat_w1 %>% bind_rows(sc3_dat_w2) %>% bind_rows(sc3_dat_w3) %>% 
  bind_rows(sc3_dat_w4) %>% bind_rows(sc3_dat_w5)


# Now make a dataset with the value all relative to 0% natural habitat.

base_values <- sc3_dat_w %>% filter(N == 0) %>% ungroup() %>% 
  mutate(base_value = value) %>% select(hab_cat, base_value, metric) %>% ungroup()

df_percent <- sc3_dat_w %>% ungroup() %>%
  left_join(base_values) %>% 
  #filter(N != 0) %>%  # Keep only the rows with N > 0
  mutate(per_change = ((value - base_value)/base_value) * 100) %>%
  select(N, hab_cat, per_change, metric) %>%
  ungroup()


# Figure #: Growth, size, and survival ----

# Reorder the factor levels
df_percent$metric <- factor(df_percent$metric, 
                            levels = c("growth_g", "growth_rate",
                                       "size_g", "cum_surv", "surv_rate"))

# with percent change relative to baseline 0% natural
fig_sc3_gs <- ggplot(data = df_percent, aes(x = (N*100), y = per_change)) +
  geom_point(aes(color = hab_cat)) + geom_line(aes(color = hab_cat)) +
  facet_wrap(~metric, ncol = 1, scales = "free",
             labeller = labeller(
               metric = c(
                 "cum_surv" = "(d)    Cumulative survival",
                 "growth_g" = " (a)    Total growth (g)",
                 "growth_rate" = "(b)    Growth rate (g/d)",
                 "size_g" = "(c)    Size (g)",
                 "surv_rate" = "(e)    Survival rate (S/d)"
               )
             )) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("royalblue", "forestgreen")) +
  theme_bw() +
  theme(strip.text = element_text(size = 10, hjust = 0),  # Adjust the text size
        strip.background = element_rect(fill = "white", color = NA),
        legend.title = element_blank()) +  # Adjust margin around text
  xlab("Percent natural habitat") +
  ylab("Percent change relative\nto 0% natural habitat"); fig_sc3_gs 


# Save Figure for scenario 3
pdf.options(reset = TRUE, onefile = FALSE)
pdf("results//Manuscript V5/figures/Figure_S8.pdf", width=4, height=9)
fig_sc3_gs
dev.off()


# percent difference in fitness from baseline (0% natural) to 100% natural.
temp <- sc3_dat_l %>% select(Fit_cumsurv, N)
((temp$Fit_cumsurv[5] - temp$Fit_cumsurv[1]) / temp$Fit_cumsurv[1] ) * 100



# For main text! When more predators in natural ----

p_moves_dat2 <- p_moves_dat %>% filter(iter_name == "more_preds_in_natural")
sc3_dat_l <- sum_dat %>% filter(iter_name == "more_preds_in_natural")  # in long format


# Figure #: behavior and fitness ----

fig_sc3_beh <- ggplot(data=p_moves_dat2, 
                      aes(x=as.factor(N*100), y=p_moves, fill=as.factor(Beh))) + 
  geom_bar(stat="identity", color="black") + 
  theme_classic() +
  scale_fill_brewer(palette = "Blues", labels = c("0", "1", "2")) + 
  scale_y_continuous(expand = c(0,0), limits=c(0,1), breaks = seq(0,1,by=0.2)) +
  ylab("Proportion of moves") + xlab("Percent of natural habitat") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  ggtitle(label = "(a)") + theme(plot.title = element_text(size=14, face = "bold")); fig_sc3_beh

# (b) Migration duration
fig_sc3_dur<- ggplot(data=sc3_dat_l, aes(x=(N*100), y=mean_dur)) +
  geom_line(size=1, alpha=0.7) + geom_point(size=2, alpha=1) + 
  theme_classic() +
  ylab("Migration duration (days)") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(b)") + theme(plot.title = element_text(size=14, face = "bold")); fig_sc3_dur


# (c) Fitness
fig_sc3_fit <- ggplot(data=sc3_dat_l, aes(x=(N*100), y=Fit_cumsurv)) +
  geom_line(size=1, alpha=0.7) + geom_point(size=2, alpha=1) + 
  theme_classic() +
  ylab("Fitness") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(c)") + theme(plot.title = element_text(size=14, face = "bold")); fig_sc3_fit


# Together
ggarrange(fig_sc3_beh, fig_sc3_dur, fig_sc3_fit, ncol = 1)

# Save Figure for scenario 2
pdf.options(reset = TRUE, onefile = FALSE)
pdf("results//Manuscript V5/figures/Figure_4.pdf", width=4, height=8)
ggarrange(fig_sc3_beh, fig_sc3_dur, fig_sc3_fit, ncol = 1, align = "hv")
dev.off()


### Figure #: growth and survival ----

# growth rate
sc3_dat_w1 <- sc3_dat_l %>% select(iter_id, N, 
                                   mean_G_riv_d, mean_G_ocean_d) %>% 
  pivot_longer(cols = c(mean_G_riv_d, mean_G_ocean_d),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "mean_G_riv_d", "river", "ocean"),
         metric = "growth_rate")

# growth
sc3_dat_w2 <- sc3_dat_l %>% select(iter_id, N, 
                                   mean_G_riv, mean_G_ocean) %>% 
  pivot_longer(cols = c(mean_G_riv, mean_G_ocean),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "mean_G_riv", "river", "ocean"),
         metric = "growth_g")

# size
sc3_dat_w3 <- sc3_dat_l %>% select(iter_id, N, 
                                   size_oe, W.T60) %>% 
  pivot_longer(cols = c(size_oe, W.T60),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "size_oe", "river", "ocean"),
         metric = "size_g")

# cumulative survival
sc3_dat_w4 <- sc3_dat_l %>% select(iter_id, N, 
                                   mean_S_cum, S.cum.T60) %>% 
  pivot_longer(cols = c(mean_S_cum, S.cum.T60),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "mean_S_cum", "river", "ocean"),
         metric = "cum_surv")

# survival rate
sc3_dat_w5 <- sc3_dat_l %>% select(iter_id, N, 
                                   mean_S_riv_d, mean_S_ocean_d) %>% 
  pivot_longer(cols = c(mean_S_riv_d, mean_S_ocean_d),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "mean_S_riv_d", "river", "ocean"),
         metric = "surv_rate")

# Put all long data together: raw metric values!
sc3_dat_w <- sc3_dat_w1 %>% bind_rows(sc3_dat_w2) %>% bind_rows(sc3_dat_w3) %>% 
  bind_rows(sc3_dat_w4) %>% bind_rows(sc3_dat_w5)


# Now make a dataset with the value all relative to 0% natural habitat.

base_values <- sc3_dat_w %>% filter(N == 0) %>% ungroup() %>% 
  mutate(base_value = value) %>% select(hab_cat, base_value, metric) %>% ungroup()

df_percent <- sc3_dat_w %>% ungroup() %>%
  left_join(base_values) %>% 
  #filter(N != 0) %>%  # Keep only the rows with N > 0
  mutate(per_change = ((value - base_value)/base_value) * 100) %>%
  select(N, hab_cat, per_change, metric) %>%
  ungroup()


# Figure #: Growth, size, and survival ----

# Reorder the factor levels
df_percent$metric <- factor(df_percent$metric, 
                            levels = c("growth_g", "growth_rate",
                                       "size_g", "cum_surv", "surv_rate"))

# with percent change relative to baseline 0% natural
fig_sc3_gs <- ggplot(data = df_percent, aes(x = (N*100), y = per_change)) +
  geom_point(aes(color = hab_cat)) + geom_line(aes(color = hab_cat)) +
  facet_wrap(~metric, ncol = 1, scales = "free",
             labeller = labeller(
               metric = c(
                 "cum_surv" = "(d)    Cumulative survival",
                 "growth_g" = "(a)    Total growth (g)",
                 "growth_rate" = "(b)    Growth rate (g/d)",
                 "size_g" = "(c)    Size (g)",
                 "surv_rate" = "(e)    Survival rate (S/d)"
               )
             )) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("royalblue", "forestgreen")) +
  theme_bw() +
  theme(strip.text = element_text(size = 10, hjust = 0),  # Adjust the text size
        strip.background = element_rect(fill = "white", color = NA),
        legend.title = element_blank()) +  # Adjust margin around text
  xlab("Percent natural habitat") +
  ylab("Percent change relative\nto 0% natural habitat"); fig_sc3_gs

# Save Figure for scenario 3
pdf.options(reset = TRUE, onefile = FALSE)
pdf("results//Manuscript V5/figures/Figure_5.pdf", width=4, height=9)
fig_sc3_gs
dev.off()


# Scenario 3 summary stats ----
# When more predators in natural

sc3_dat_w %>% left_join(select(df_iters, iter_id, iter_name)) %>% 
  filter(iter_name == "more_preds_in_natural" &
         metric == "size_g" & hab_cat == "river")

sc3_dat_l %>% select(mean_dur, N)

# percent difference in fitness from baseline (0% natural) to 100% natural.
temp <- sc3_dat_l %>% select(Fit_cumsurv, N)
((temp$Fit_cumsurv[5] - temp$Fit_cumsurv[1]) / temp$Fit_cumsurv[1] ) * 100


# Not in paper: When more predators in altered ----

p_moves_dat2 <- p_moves_dat %>% filter(iter_name == "more_preds_in_altered")
sc3_dat_l <- sum_dat %>% filter(iter_name == "more_preds_in_altered")  # in long format


# Figure #: behavior and fitness ----

fig_sc3_beh <- ggplot(data=p_moves_dat2, 
                      aes(x=as.factor(N*100), y=p_moves, fill=as.factor(Beh))) + 
  geom_bar(stat="identity", color="black") + 
  theme_classic() +
  scale_fill_brewer(palette = "Blues", labels = c("0", "1", "2")) + 
  scale_y_continuous(expand = c(0,0), limits=c(0,1), breaks = seq(0,1,by=0.2)) +
  ylab("Proportion of moves") + xlab("Percent of natural habitat") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  ggtitle(label = "(a)") + theme(plot.title = element_text(size=14, face = "bold")); fig_sc3_beh

# (b) Migration duration
fig_sc3_dur<- ggplot(data=sc3_dat_l, aes(x=(N*100), y=mean_dur)) +
  geom_line(size=1, alpha=0.7) + geom_point(size=2, alpha=1) + 
  theme_classic() +
  ylab("Migration duration (days)") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(b)") + theme(plot.title = element_text(size=14, face = "bold")); fig_sc3_dur


# (c) Fitness
fig_sc3_fit <- ggplot(data=sc3_dat_l, aes(x=(N*100), y=Fit_cumsurv)) +
  geom_line(size=1, alpha=0.7) + geom_point(size=2, alpha=1) + 
  theme_classic() +
  ylab("Fitness") +
  xlab("Percent of natural habitat") +
  ggtitle(label="(c)") + theme(plot.title = element_text(size=14, face = "bold")); fig_sc3_fit


# Together
ggarrange(fig_sc3_beh, fig_sc3_dur, fig_sc3_fit, ncol = 1)

# # Save Figure for scenario 2
# pdf.options(reset = TRUE, onefile = FALSE)
# pdf("results//Manuscript V5/figures/Figure_sc3_more_preds_alt.pdf", width=4, height=8)
# ggarrange(fig_sc3_beh, fig_sc3_dur, fig_sc3_fit, ncol = 1, align = "hv")
# dev.off()

### Figure #: growth and survival ----

# growth rate
sc3_dat_w1 <- sc3_dat_l %>% select(iter_id, N, 
                                   mean_G_riv_d, mean_G_ocean_d) %>% 
  pivot_longer(cols = c(mean_G_riv_d, mean_G_ocean_d),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "mean_G_riv_d", "river", "ocean"),
         metric = "growth_rate")

# growth
sc3_dat_w2 <- sc3_dat_l %>% select(iter_id, N, 
                                   mean_G_riv, mean_G_ocean) %>% 
  pivot_longer(cols = c(mean_G_riv, mean_G_ocean),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "mean_G_riv", "river", "ocean"),
         metric = "growth_g")

# size
sc3_dat_w3 <- sc3_dat_l %>% select(iter_id, N, 
                                   size_oe, W.T60) %>% 
  pivot_longer(cols = c(size_oe, W.T60),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "size_oe", "river", "ocean"),
         metric = "size_g")

# cumulative survival
sc3_dat_w4 <- sc3_dat_l %>% select(iter_id, N, 
                                   mean_S_cum, S.cum.T60) %>% 
  pivot_longer(cols = c(mean_S_cum, S.cum.T60),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "mean_S_cum", "river", "ocean"),
         metric = "cum_surv")

# survival rate
sc3_dat_w5 <- sc3_dat_l %>% select(iter_id, N, 
                                   mean_S_riv_d, mean_S_ocean_d) %>% 
  pivot_longer(cols = c(mean_S_riv_d, mean_S_ocean_d),
               names_to = "hab_cat") %>% 
  mutate(hab_cat = ifelse(hab_cat == "mean_S_riv_d", "river", "ocean"),
         metric = "surv_rate")

# Put all long data together: raw metric values!
sc3_dat_w <- sc3_dat_w1 %>% bind_rows(sc3_dat_w2) %>% bind_rows(sc3_dat_w3) %>% 
  bind_rows(sc3_dat_w4) %>% bind_rows(sc3_dat_w5)


# Now make a dataset with the value all relative to 0% natural habitat.

base_values <- sc3_dat_w %>% filter(N == 0) %>% ungroup() %>% 
  mutate(base_value = value) %>% select(hab_cat, base_value, metric) %>% ungroup()

df_percent <- sc3_dat_w %>% ungroup() %>%
  left_join(base_values) %>% 
  #filter(N != 0) %>%  # Keep only the rows with N > 0
  mutate(per_change = ((value - base_value)/base_value) * 100) %>%
  select(N, hab_cat, per_change, metric) %>%
  ungroup()


# Figure #: Growth, size, and survival ----

# Reorder the factor levels
df_percent$metric <- factor(df_percent$metric, 
                            levels = c("growth_g", "growth_rate",
                                       "size_g", "cum_surv", "surv_rate"))

# with percent change relative to baseline 0% natural
fig_sc3_gs <- ggplot(data = df_percent, aes(x = (N*100), y = per_change)) +
  geom_point(aes(color = hab_cat)) + geom_line(aes(color = hab_cat)) +
  facet_wrap(~metric, ncol = 1, scales = "free",
             labeller = labeller(
               metric = c(
                 "cum_surv" = "(d)    Cumulative survival",
                 "growth_g" = "(a)    Total growth (g)",
                 "growth_rate" = "(b)    Growth rate (g/d)",
                 "size_g" = "(c)    Size (g)",
                 "surv_rate" = "(e)    Survival rate (S/d)"
               )
             )) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("royalblue", "forestgreen")) +
  theme_bw() +
  theme(strip.text = element_text(size = 10, hjust = 0),  # Adjust the text size
        strip.background = element_rect(fill = "white", color = NA),
        legend.title = element_blank()) +  # Adjust margin around text
  xlab("Percent natural habitat") +
  ylab("Percent change relative\nto 0% natural habitat"); fig_sc3_gs

# # Save Figure for scenario 3
# pdf.options(reset = TRUE, onefile = FALSE)
# pdf("results//Manuscript V5/figures/Figure_sc3_more_preds_alt_GS.pdf", width=4, height=9)
# fig_sc3_gs
# dev.off()

# Note: the scenario with more preds in altered results in natural river habitats
# being so much better than river altered and the ocean, that it results in 
# unrealistic outmigration behavior.









