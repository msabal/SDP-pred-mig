### Plots for NOAA Monster Jam presentation


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
    geom_point(size=4, shape=21, color="black") + theme_classic() +
    scale_color_manual(values=c("mediumpurple", "forestgreen")) +
    scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
    ylab(str_c("Frequency of pauses")) + xlab("Relatively fewer predators in natural ->") +
    theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1)); yn_ya_plot


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
  geom_point(size=4, shape=21, color="black") + theme_classic() +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab(str_c("Frequency of pauses")) + xlab("Relatively higher escape ability in natural ->") +
  theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1)); Bn_Ba_plot

  
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
  geom_point(size=4, shape=21, color="black") + theme_classic() +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab(str_c("Frequency of pauses")) + xlab("Relatively more food in natural ->") +
  theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1)); kn_ka_plot



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
  geom_point(size=4, shape=21, color="black") + theme_classic() +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab(str_c("Frequency of pauses")) + xlab("Relatively more energy savings in natural ->") +
  theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1)); dn0_d_plot


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

