
# Make plots from OSU Server outputs


# Load libraries ----
library(tidyverse)

#remove scientific notation
options(scipen=999)


# load files

# OSU server ran on 6/1/2022: null and habitat hypotheses with original parameters.
base_dat <- read_csv("G://My Drive//Professional//GIT Repositories//SDP-pred-mig//results//OSU_server_output//iterate_sum_null_and_hab.csv")

param_dat <- read_csv("G://My Drive//Professional//GIT Repositories//SDP-pred-mig//raw-data//Parameter_Iterations_Tracking.csv")


# Manipulate data

base_dat2 <- base_dat %>%
  select(-1) %>% 
  left_join(param_dat) %>% 
  mutate(yn = ifelse(param_name == "yn", param_value, yn), # replace the joined column with baseline yn with the iterated value.
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

colnames(base_dat2) # all ratio columns were calculated, just can't see them all in View.

View(base_dat %>% select(baseline, param_value, param_name) %>% distinct()) # hmmm missing a bunch of null iterations...





######################
## Copy and paste parameter column (e.g., ya_ya, Bn_Ba, dn0) AND iteration (e.g., null, habitat_hypoth)


# First plot:
y_dat <- base_dat2 %>% 
  filter(param_name == "dn0" & h != "o" &  # param_name %in% c("yn", "ya")
           baseline == "habitat_hypoth")
  
  # 1 - Proportion of moves by habitat: Tot
  
  # summarize data for barplot
  bar.beh<-aggregate(p.tot ~ h + Beh + dn0, data= y_dat, mean) #+ baseline
sum(bar.beh$p) # this should equal the number of iterations because sum to 1 for each of them.

bar.beh$h<-as.factor(bar.beh$h)
bar.beh$Beh<-as.factor(bar.beh$Beh)

bar.beh <- bar.beh[order(bar.beh$h, bar.beh$Beh),]

fig_p1 <- ggplot(data=bar.beh, aes(x=h, y=p.tot, fill=Beh)) + geom_bar(stat="identity", color="black") +
  facet_wrap(~dn0, ncol=10) + theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
  theme(legend.title = element_blank()) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices") +
  theme(legend.position = "bottom") + ggtitle(label = "Iteration: habitat_hypoth") +
  theme(plot.title = element_text(hjust = 0.5))
fig_p1


# 2 - Proportion of moves by habitat: By habitat

# summarize data for barplot
bar.beh<-aggregate(p ~ h + Beh + dn0, data=y_dat, mean)
sum(bar.beh$p) # this should equal the number of iterations x2 because sum to 1 for each of them.

bar.beh$h<-as.factor(bar.beh$h)
bar.beh$Beh<-as.factor(bar.beh$Beh)

bar.beh <- bar.beh[order(bar.beh$h, bar.beh$Beh),]

fig_p2 <- ggplot(data=bar.beh, aes(x=h, y=p, fill=Beh)) + geom_bar(stat="identity", color="black") +
  facet_wrap(~dn0, ncol=10) + theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
  theme(legend.title = element_blank()) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices") +
  theme(legend.position = "bottom") + ggtitle(label = "Iteration: habitat_hypoth") +
  theme(plot.title = element_text(hjust = 0.5))
fig_p1
fig_p2


# 3 - Freq of move 0 by habitat by iterations

# Aggregate  by Wstart (salmon size)
ag.param<-aggregate(p.tot ~ h + dn0, data=y_dat, mean)
a<-aggregate(p.tot ~ h + dn0, data=y_dat, sd); colnames(a)[3]<-"sd"
b<-aggregate(p.tot ~ h + dn0, data=y_dat, length); colnames(b)[3]<-"n"
ag.param<-join(ag.param, a); ag.param<-join(ag.param, b)
ag.param$se<-ag.param$sd / sqrt(ag.param$n)

ag.param$h<-as.factor(ag.param$h)
levels(ag.param$h) <- c("Altered", "Natural")


# Plots!

plot_var <- ggplot(data=ag.param, aes(x=dn0, y=p.tot, fill=h, color=h)) + 
  geom_line(size=0.5, aes(color=h)) +
  geom_errorbar(aes(ymax=p.tot + se, ymin=p.tot - se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") + 
  theme_classic() + ylim(c(0,1)) +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab("Frequency of move 0")  +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom", legend.background = element_rect(fill="transparent")) + 
  ggtitle(label = "Iteration: habitat_hypoth") +
  theme(plot.title = element_text(hjust = 0.5))
fig_p1

plot_var
