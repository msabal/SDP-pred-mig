

# Figures for Manuscript Draft



# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)



## Figure 1: Conceptual model


## Figure 2: (a) model output of baseline tracks, (b) stacked bar-plot of diversity of movements


#### Run MAIN_FUN_TRACKS for 1 set of parameters (habitat-hypotheses)


data_tracks <- MAIN_FUN_TRACKS(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                               E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                               ya, yn, yo, dn0, Ba, Bn, Bo, ka, kn, # vars that vary by habitat (h.vec)
                               Ws, r, Smax, W, # vars for Terminal fitness function
                               Wstep.n, Wstep, tmax, seeds, F.vec, N)

data_tracks


###################

# Figure 2a - Tracks: A ~ Time

# See code from inside main function.
h.vec <- rep(NA, Amax) # create blank vector for habitats for each Area.
h.vec[Amax] <- "o" # make the last area (Amax) the ocean: "o"
set.seed(seeds)  # set.seed to keep altered and natural habitat distribution constant for now.
h.vec[1:Amax-1] <- sample(0:1, Amax-1, replace=T, prob=c(1-N,N))  # randomly sample
# Amax-1 number of values 0 or 1 with a 50% probability between the two values.
h.vec[h.vec == "1"] <- "a"  # change 1 from sample function to "a"
h.vec[h.vec == "0"] <- "n"  # change 0 from sample function to "n"

# Make dataframe with Area habitat types and color for plotting.
h.vec <- data_frame(h=h.vec, A=seq(1, Amax, by=1)) %>% as_tibble() %>% 
  mutate(h.col = ifelse(.$h == "a", "mediumpurple",
                        ifelse(.$h == "n", "forestgreen", "blue3")))


# Fig 2a: Simulated salmon tracks.

# expand Blues color palette to 10 sizes
getPalette <- colorRampPalette(brewer.pal(9, "Blues"))
getPalette(10)

fig_2a <- ggplot(data=data_tracks, aes(x=Time, y=A, color=as.factor(Wstart))) +
  geom_line(size=1, position=position_dodge(0.4)) + geom_point(position=position_dodge(0.4)) +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.position="bottom", legend.background=element_blank(), legend.text = element_text(size=8)) + 
  coord_equal() +
  scale_x_continuous(breaks = seq(1,tmax,2)) +
  scale_y_continuous(breaks = seq(1,Amax,1)) +
  theme(axis.text.y = element_text(color=h.vec$h.col, face="bold")) +
  ylab("Area") + 
  scale_color_manual(values = getPalette(10), name= "Starting salmon size (g)") +
  annotate(geom="text", label="(a)", x=3, y=26, size=6); fig_2a



# Fig 2b: Proportion of moves by habitat type: Beh ~ h
base_dat2 <- base_dat %>%
  select(-1) %>% 
  left_join(param_dat) %>% 
  mutate(yn = ifelse(param_name == "yn", param_value, yn), # replace the joined column with baseline yn with the nullated value.
         ya = ifelse(param_name == "ya", param_value, ya),
         Bn = ifelse(param_name == "Bn", param_value, Bn),
         Ba = ifelse(param_name == "Ba", param_value, Ba),
         kn = ifelse(param_name == "kn", param_value, kn),
         ka = ifelse(param_name == "ka", param_value, ka),
         dn0 = ifelse(param_name == "dn0", param_value, dn0),
         d = ifelse(param_name == "d", param_value, d)) %>% 
  mutate(yn_ya = log(yn/ya),  # Calculate natural:altered habitat ratios
         Bn_Ba = log(Bn/Ba),
         kn_ka = log(kn/ka),
         dn0_d = log(dn0/d))

ag_dat <- base_dat2 %>% filter(baseline == "habitat_hypoth" & h != "o") %>%
  group_by(baseline, h, Beh) %>% summarise(mean = mean(p),
                                           sd = sd(p),
                                           n = length(p))

ag_dat$h<-as.factor(ag_dat$h)
ag_dat$Beh<-as.factor(ag_dat$Beh)
ag_dat <- ag_dat[order(ag_dat$h, ag_dat$Beh),]

fig_2b <-  ggplot(data=ag_dat, aes(x=h, y=mean, fill=Beh)) + geom_bar(stat="identity", color="black") +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0\n(0 km/d)", "move 1\n(20 km/d)", "move 2\n(40 km/d)")) +
  theme(legend.title = element_blank()) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices") +
  theme(legend.position = "bottom", legend.text=element_text(size=8)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", label ="(b)", x=0.7, y=1.1, size=6); fig_2b




pdf.options(reset = TRUE, onefile = FALSE)
setwd("C:/Users/sabalm/Desktop/")
pdf("Figure2.pdf", width=10, height=4)

ggarrange(fig_2a, fig_2b, ncol=2, widths = c(3,1.5), heights=c(1.3,1)) # align = "h"

dev.off()


# my way: 3rd try here.
bar_dat <- data_tracks %>% as_tibble() %>% filter(h != "o") %>% 
  group_by(Wstart, h) %>% summarise(n_hab = length(Time)) #%>% 
  #pivot_wider(names_from = h, values_from= n_hab)

bar_dat <- data_tracks %>% as_tibble() %>% filter(h != "o") %>% 
  group_by(Wstart, h, Beh) %>% summarise(n_beh = length(Time)) %>% 
  left_join(bar_dat) %>% 
  mutate(p = n_beh / n_hab) %>% 
  group_by(h, Beh) %>% summarise(mean = mean(p),
                                 sd = sd(p)) %>% 
  mutate(n = 10) %>%  # mean over the 10 fish sizes of Wstart
  bind_rows(data_frame(h="a", Beh = 0, mean =0, sd=0, n=10)) %>% 
  mutate(se = sd/sqrt(n))


fig_2b <-  ggplot(data=bar_dat, aes(x=h, y=mean, fill=as.factor(Beh))) + geom_bar(stat="identity", color="black") +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0\n(0 km/d)", "move 1\n(20 km/d)", "move 2\n(40 km/d)")) +
  theme(legend.title = element_blank()) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices") +
  theme(legend.position = "bottom", legend.text=element_text(size=8)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", label ="(b)", x=0.7, y=1.1, size=6); fig_2b





# summarize data for barplot
bar.beh<-aggregate(Time ~ h + Beh, data=subset(data_tracks, h != "o"), length)
bar.beh$p <- bar.beh$Time/ (sum(bar.beh$Time))
sum(bar.beh$p) # this should equal 1.

bar.beh$h<-as.factor(bar.beh$h)
bar.beh$Beh<-as.factor(bar.beh$Beh)

bar.beh <- bar.beh[order(bar.beh$h, bar.beh$Beh),]

fig_p <- ggplot(data=bar.beh, aes(x=h, y=p, fill=Beh)) + geom_bar(stat="identity", color="black") +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
  theme(axis.text.y = element_text(size=13), axis.text.x = element_text(size=13),
        axis.title.y = element_text(size=13), axis.title.x = element_text(size=13),
        legend.title = element_blank()) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices") +
  theme(legend.position = "bottom"); fig_p




## Load data from R script 6_plots_from_server_output.R


### Plot: only iterations where Natural benefit > Altered benefit

#freq_fun(data=base_dat, x="yn_ya", iter = "null", beh=2)

base_dat2 <- base_dat %>%
  select(-1) %>% 
  left_join(param_dat) %>% 
  mutate(yn = ifelse(param_name == "yn", param_value, yn), # replace the joined column with baseline yn with the nullated value.
         ya = ifelse(param_name == "ya", param_value, ya),
         Bn = ifelse(param_name == "Bn", param_value, Bn),
         Ba = ifelse(param_name == "Ba", param_value, Ba),
         kn = ifelse(param_name == "kn", param_value, kn),
         ka = ifelse(param_name == "ka", param_value, ka),
         dn0 = ifelse(param_name == "dn0", param_value, dn0),
         d = ifelse(param_name == "d", param_value, d)) %>% 
  mutate(yn_ya = log(yn/ya),  # Calculate natural:altered habitat ratios
         Bn_Ba = log(Bn/Ba),
         kn_ka = log(kn/ka),
         dn0_d = log(dn0/d))

base_sub <- base_dat2 %>% select(Wstart, Beh, h, baseline, p.tot, yn_ya, Bn_Ba, kn_ka, dn0_d)

# yn_ya
ag_dat <- base_sub %>% filter(baseline == "null" & h != "o" & Beh == 0 & yn_ya != 0) %>% # UQ(sym(x)) takes x provided with quotes and makes it a symbol.
  group_by(baseline, yn_ya, h) %>% summarise(mean = mean(p.tot),
                                             sd = sd(p.tot),
                                             n = n()) %>% 
  mutate(se = sd / sqrt(n))

ag_dat$h<-as.factor(ag_dat$h)
levels(ag_dat$h) <- c("Altered", "Natural")

# for plot only plot even to benefit of natural ()  
ag_dat <- ag_dat %>% filter(yn_ya < 0) %>% 
  mutate(yn_ya2 = -1*yn_ya) # reserve the sign to increasing yn_ya2 values mean there is relatively fewer predators in natural.



yn_ya_plot <- ggplot(ag_dat, aes(x=yn_ya2, y=(mean), fill=h, color=h)) +
  geom_line(size=1, aes(color=h)) + geom_errorbar(aes(ymax=mean+se, ymin=mean-se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") + theme_classic() +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab(str_c("Frequency of pauses")) + xlab("Relatively fewer predators in natural ->") +
  theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1)) +
  annotate(geom="text", label="(a) Predator abundance", x=0.2, y=1, size=6, hjust=0); yn_ya_plot


## Bn_Ba
ag_dat <- base_sub %>% filter(baseline == "null" & h != "o" & Beh == 0 & Bn_Ba != 0) %>% # UQ(sym(x)) takes x provided with quotes and makes it a symbol.
  group_by(baseline, Bn_Ba, h) %>% summarise(mean = mean(p.tot),
                                             sd = sd(p.tot),
                                             n = n()) %>% 
  mutate(se = sd / sqrt(n))

ag_dat$h<-as.factor(ag_dat$h)
levels(ag_dat$h) <- c("Altered", "Natural")

# for plot only plot even to benefit of natural ()  
ag_dat <- ag_dat %>% filter(Bn_Ba < 0) %>% 
  mutate(Bn_Ba2 = -1*Bn_Ba) # reserve the sign to increasing yn_ya2 values mean there is relatively fewer predators in natural.



Bn_Ba_plot <- ggplot(ag_dat, aes(x=Bn_Ba2, y=(mean), fill=h, color=h)) +
  geom_line(size=1, aes(color=h)) + geom_errorbar(aes(ymax=mean+se, ymin=mean-se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") + theme_classic() +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab(str_c("Frequency of pauses")) + xlab("Relatively higher escape ability in natural ->") +
  theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1)) +
  annotate(geom="text", label="(b) Escape ability", x=0.2, y=1, size=6, hjust=0); Bn_Ba_plot


## kn_ka
ag_dat <- base_sub %>% filter(baseline == "null" & h != "o" & Beh == 0 & kn_ka != 0) %>% # UQ(sym(x)) takes x provided with quotes and makes it a symbol.
  group_by(baseline, kn_ka, h) %>% summarise(mean = mean(p.tot),
                                             sd = sd(p.tot),
                                             n = n()) %>% 
  mutate(se = sd / sqrt(n))

ag_dat$h<-as.factor(ag_dat$h)
levels(ag_dat$h) <- c("Altered", "Natural")

# for plot only plot even to benefit of natural ()  
ag_dat <- ag_dat %>% filter(kn_ka > 0) #%>% 
#mutate(Bn_Ba2 = -1*Bn_Ba) # reserve the sign to increasing yn_ya2 values mean there is relatively fewer predators in natural.



kn_ka_plot <- ggplot(ag_dat, aes(x=kn_ka, y=(mean), fill=h, color=h)) +
  geom_line(size=1, aes(color=h)) + geom_errorbar(aes(ymax=mean+se, ymin=mean-se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") + theme_classic() +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab(str_c("Frequency of pauses")) + xlab("Relatively more food in natural ->") +
  theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1)) +
  annotate(geom="text", label="(c) Foraging gain", x=0.03, y=1, size=6, hjust=0); kn_ka_plot



## dn0_d
ag_dat <- base_sub %>% filter(baseline == "null" & h != "o" & Beh == 0 & dn0_d != 0) %>% # UQ(sym(x)) takes x provided with quotes and makes it a symbol.
  group_by(baseline, dn0_d, h) %>% summarise(mean = mean(p.tot),
                                             sd = sd(p.tot),
                                             n = n()) %>% 
  mutate(se = sd / sqrt(n))

ag_dat$h<-as.factor(ag_dat$h)
levels(ag_dat$h) <- c("Altered", "Natural")

# for plot only plot even to benefit of natural ()  
ag_dat <- ag_dat %>% filter(dn0_d < 0) %>% 
  mutate(dn0_d2 = -1*dn0_d) # reserve the sign to increasing yn_ya2 values mean there is relatively fewer predators in natural.



dn0_d_plot <- ggplot(ag_dat, aes(x=dn0_d2, y=(mean), fill=h, color=h)) +
  geom_line(size=1, aes(color=h)) + geom_errorbar(aes(ymax=mean+se, ymin=mean-se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") + theme_classic() +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab(str_c("Frequency of pauses")) + xlab("Relatively more energy savings in natural ->") +
  theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1)) +
  annotate(geom="text", label="(d) Energy savings", x=0.2, y=1, size=6, hjust=0); dn0_d_plot


library(ggpubr)
ggarrange(yn_ya_plot, Bn_Ba_plot, kn_ka_plot, dn0_d_plot, ncol=2, nrow=2, common.legend=TRUE)


pdf.options(reset = TRUE, onefile = FALSE)
setwd("C:/Users/sabalm/Desktop/")
pdf("Freq_plots_null_habitat.pdf", width=8, height=6)

ggarrange(yn_ya_plot, Bn_Ba_plot, kn_ka_plot, dn0_d_plot, ncol=2, nrow=2, common.legend=TRUE)

dev.off()




# ........................................................................................................

## habitat-hypotheses
# yn_ya
ag_dat <- base_sub %>% filter(baseline == "habitat_hypoth" & h != "o" & Beh == 0 & yn_ya != 0) %>% # UQ(sym(x)) takes x provided with quotes and makes it a symbol.
  group_by(baseline, yn_ya, h) %>% summarise(mean = mean(p.tot),
                                             sd = sd(p.tot),
                                             n = n()) %>% 
  mutate(se = sd / sqrt(n))

ag_dat$h<-as.factor(ag_dat$h)
levels(ag_dat$h) <- c("Altered", "Natural")

# for plot only plot even to benefit of natural ()  
ag_dat <- ag_dat %>% filter(yn_ya < 0) %>% 
  mutate(yn_ya2 = -1*yn_ya) # reserve the sign to increasing yn_ya2 values mean there is relatively fewer predators in natural.



yn_ya_plot <- ggplot(ag_dat, aes(x=yn_ya2, y=(mean), fill=h, color=h)) +
  geom_line(size=1, aes(color=h)) + geom_errorbar(aes(ymax=mean+se, ymin=mean-se, color=h), width=0, size=0.5) +
  geom_point(size=4, shape=21, color="black") + theme_classic() +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab(str_c("Frequency of pauses")) + xlab("Relatively fewer predators in natural ->") +
  theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1)); yn_ya_plot


## Bn_Ba
ag_dat <- base_sub %>% filter(baseline == "habitat_hypoth" & h != "o" & Beh == 0 & Bn_Ba != 0) %>% # UQ(sym(x)) takes x provided with quotes and makes it a symbol.
  group_by(baseline, Bn_Ba, h) %>% summarise(mean = mean(p.tot),
                                             sd = sd(p.tot),
                                             n = n()) %>% 
  mutate(se = sd / sqrt(n))

ag_dat$h<-as.factor(ag_dat$h)
levels(ag_dat$h) <- c("Altered", "Natural")

# for plot only plot even to benefit of natural ()  
ag_dat <- ag_dat %>% filter(Bn_Ba < 0) %>% 
  mutate(Bn_Ba2 = -1*Bn_Ba) # reserve the sign to increasing yn_ya2 values mean there is relatively fewer predators in natural.



Bn_Ba_plot <- ggplot(ag_dat, aes(x=Bn_Ba2, y=(mean), fill=h, color=h)) +
  geom_line(size=1, aes(color=h)) + geom_errorbar(aes(ymax=mean+se, ymin=mean-se, color=h), width=0, size=0.5) +
  geom_point(size=4, shape=21, color="black") + theme_classic() +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab(str_c("Frequency of pauses")) + xlab("Relatively higher escape ability in natural ->") +
  theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1)); Bn_Ba_plot


## kn_ka
ag_dat <- base_sub %>% filter(baseline == "habitat_hypoth" & h != "o" & Beh == 0 & kn_ka != 0) %>% # UQ(sym(x)) takes x provided with quotes and makes it a symbol.
  group_by(baseline, kn_ka, h) %>% summarise(mean = mean(p.tot),
                                             sd = sd(p.tot),
                                             n = n()) %>% 
  mutate(se = sd / sqrt(n))

ag_dat$h<-as.factor(ag_dat$h)
levels(ag_dat$h) <- c("Altered", "Natural")

# for plot only plot even to benefit of natural ()  
ag_dat <- ag_dat %>% filter(kn_ka > 0) #%>% 
#mutate(Bn_Ba2 = -1*Bn_Ba) # reserve the sign to increasing yn_ya2 values mean there is relatively fewer predators in natural.



kn_ka_plot <- ggplot(ag_dat, aes(x=kn_ka, y=(mean), fill=h, color=h)) +
  geom_line(size=1, aes(color=h)) + geom_errorbar(aes(ymax=mean+se, ymin=mean-se, color=h), width=0, size=0.5) +
  geom_point(size=4, shape=21, color="black") + theme_classic() +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab(str_c("Frequency of pauses")) + xlab("Relatively more food in natural ->") +
  theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1)); kn_ka_plot



## dn0_d
ag_dat <- base_sub %>% filter(baseline == "habitat_hypoth" & h != "o" & Beh == 0 & dn0_d != 0) %>% # UQ(sym(x)) takes x provided with quotes and makes it a symbol.
  group_by(baseline, dn0_d, h) %>% summarise(mean = mean(p.tot),
                                             sd = sd(p.tot),
                                             n = n()) %>% 
  mutate(se = sd / sqrt(n))

ag_dat$h<-as.factor(ag_dat$h)
levels(ag_dat$h) <- c("Altered", "Natural")

# for plot only plot even to benefit of natural ()  
ag_dat <- ag_dat %>% filter(dn0_d < 0) %>% 
  mutate(dn0_d2 = -1*dn0_d) # reserve the sign to increasing yn_ya2 values mean there is relatively fewer predators in natural.



dn0_d_plot <- ggplot(ag_dat, aes(x=dn0_d2, y=(mean), fill=h, color=h)) +
  geom_line(size=1, aes(color=h)) + geom_errorbar(aes(ymax=mean+se, ymin=mean-se, color=h), width=0, size=0.5) +
  geom_point(size=4, shape=21, color="black") + theme_classic() +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab(str_c("Frequency of pauses")) + xlab("Relatively more energy savings in natural ->") +
  theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1)); dn0_d_plot


library(ggpubr)
ggarrange(yn_ya_plot, Bn_Ba_plot, kn_ka_plot, dn0_d_plot, ncol=2, nrow=2, common.legend=TRUE)




# ........................................................................................................
# how many scenarios of parameter combinations (pick one starting salmon size) is there more
# pauses in natural vs. altered?

ag_dat1 <- yn_greater_dat_tog %>% filter(h != "o" & Beh == 0) %>%
  group_by(h, iter_index) %>% summarise(mean = mean(p.tot)) %>%
  pivot_wider(id_cols = iter_index, names_from = h, values_from = mean) %>%
  mutate(more_n = n - a) %>% 
  mutate(more_n_cat = ifelse(more_n > 0, 1, 2)) # 1 is more in natural, 2 is more in altered

ag_dat2 <- yn_greater_dat %>% filter(h!="o" & Beh == 0) %>% 
  group_by(h, param_value, param_name) %>% summarise(mean=mean(p.tot)) %>% 
  pivot_wider(id_cols = c(param_value, param_name), names_from = h, values_from = mean) %>%
  mutate(more_n = n - a) %>% 
  mutate(more_n_cat = ifelse(more_n > 0, 1, 2))  # 1 is more in natural, 2 is more in altered

ag_dat1 <- ag_dat1 %>% select(-iter_index)
ag_dat2 <- ag_dat2 %>% ungroup() %>% select(3:6)
all <- left_join(ag_dat1, ag_dat2)

all # 210 parameter scenarios: solo and in combination

num_nat <- all %>% filter(more_n > 0) %>% count() %>% pull()
num_nat / 210 * 100 # 37% of the time still natural 

# combos together
num_nat <- ag_dat1 %>% filter(more_n > 0) %>% count() %>% pull()
num_nat / length(ag_dat1$iter_index) * 100 # 62.4% of the time still natural 

# one at a time
num_nat <- ag_dat2 %>% filter(more_n > 0) %>% count() %>% pull()
num_nat / length(ag_dat2$more_n) * 100 # 8% of the time still natural 

(78+2) / (125+25) *100  # 53.33% of the time still optimal to pause in natural



## .................................................................
####### p


ag_dat <- base_dat2 %>% filter(baseline == "habitat_hypoth" & h != "o") %>%
  group_by(baseline, h, Beh) %>% summarise(mean = mean(p))

ag_dat$h<-as.factor(ag_dat$h)
ag_dat$Beh<-as.factor(ag_dat$Beh)
ag_dat <- ag_dat[order(ag_dat$h, ag_dat$Beh),]

div_plot <-  ggplot(data=ag_dat, aes(x=h, y=mean, fill=Beh)) + geom_bar(stat="identity", color="black") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
  theme(legend.title = element_blank()) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices") +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5)); div_plot


pdf.options(reset = TRUE, onefile = FALSE)
setwd("C:/Users/sabalm/Desktop/")
pdf("Diversity_plot.pdf", width=4, height=4)

div_plot

dev.off()

