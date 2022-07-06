
# Make plots from OSU Server outputs


# Load libraries ----
library(tidyverse); library(gridExtra)

#remove scientific notation
options(scipen=999)


# load files
param_dat <- read_csv("G://My Drive//Professional//GIT Repositories//SDP-pred-mig//raw-data//Parameter_Iterations_Tracking.csv")

# OSU server ran on 6/1/2022: null and habitat hypotheses with original parameters.
base_dat <- read_csv("G://My Drive//Professional//GIT Repositories//SDP-pred-mig//results//OSU_server_output//iterate_sum_null_and_hab.csv")

# OSU server ran on 7/5/2022: when yn > ya - vary Bn, kn, dn0 SEPARATELY.
yn_greater_dat <- read_csv("G://My Drive//Professional//GIT Repositories//SDP-pred-mig//results//OSU_server_output//iterate_when_yn_greater_ya.csv")


# Manipulate/check data

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

#View(base_dat2 %>% select(baseline, param_value, param_name, yn_ya, Bn_Ba, kn_ka, dn0_d) %>% distinct()) # hmmm missing a bunch of null iterations...
base_dat %>% select(baseline, param_value, param_name) %>% distinct() %>% group_by(baseline,param_name) %>% count() # good, each baseline + param_name has 5 iterations.


####

freq_fun <- function(data, x, iter, beh){
  
  base_dat2 <- data %>%
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
  
  ag_dat <- base_dat2 %>% filter(baseline == iter & h != "o" & Beh == beh & UQ(sym(x)) != 0) %>% # UQ(sym(x)) takes x provided with quotes and makes it a symbol.
    group_by(baseline, .data[[x]], h) %>% summarise(mean = mean(p.tot),
                                                    sd = sd(p.tot),
                                                    n = n()) %>% 
    mutate(se = sd / sqrt(n))
  
  ag_dat$h<-as.factor(ag_dat$h)
  levels(ag_dat$h) <- c("Altered", "Natural")
  
  vline_base <- param_dat %>% filter(baseline == iter) %>% select(x) %>% pull()
  
  ggplot(ag_dat, aes(x=.data[[x]], y=(mean), fill=h, color=h)) +
    geom_vline(xintercept = vline_base, linetype="dashed", size=1, color="lightblue") +
    geom_vline(xintercept = 0, color="gray80", size=1, linetype="dashed") +
    geom_line(size=0.5, aes(color=h)) + geom_errorbar(aes(ymax=mean+se, ymin=mean-se, color=h), width=0, size=0.5) +
    geom_point(size=2, shape=21, color="black") + theme_classic() +
    scale_color_manual(values=c("mediumpurple", "forestgreen")) +
    scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
    ylab(str_c("Frequency of move ", beh)) + xlab(str_c("log(", x, ")")) +
    theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1))
}


# freq_fun <- function(x, iter, beh){
#   ag_dat <- base_dat2 %>% filter(baseline == iter & h != "o" & Beh == beh & UQ(sym(x)) != 0) %>% # UQ(sym(x)) takes x provided with quotes and makes it a symbol.
#     group_by(baseline, .data[[x]], h) %>% summarise(mean = mean(p.tot),
#                                                                  sd = sd(p.tot),
#                                                                  n = n()) %>% 
#     mutate(se = sd / sqrt(n))
#   
#   ag_dat$h<-as.factor(ag_dat$h)
#   levels(ag_dat$h) <- c("Altered", "Natural")
#   
#   vline_base <- param_dat %>% filter(baseline == iter) %>% select(x) %>% pull()
#   
#   ggplot(ag_dat, aes(x=.data[[x]], y=(mean), fill=h, color=h)) +
#     geom_vline(xintercept = vline_base, linetype="dashed", size=1, color="lightblue") +
#     geom_vline(xintercept = 0, color="gray80", size=1, linetype="dashed") +
#     geom_line(size=0.5, aes(color=h)) + geom_errorbar(aes(ymax=mean+se, ymin=mean-se, color=h), width=0, size=0.5) +
#     geom_point(size=2, shape=21, color="black") + theme_classic() +
#     scale_color_manual(values=c("mediumpurple", "forestgreen")) +
#     scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
#     ylab(str_c("Frequency of move ", beh)) + xlab(str_c("log(", x, ")")) +
#     theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1))
# }

p_y1 <- freq_fun(data=base_dat, x="yn_ya", iter = "null", beh=2); p_y1
p_b1 <- freq_fun(data=base_dat, x="Bn_Ba", iter = "null", beh=2); p_b1
p_k1 <- freq_fun(data=base_dat, x="kn_ka", iter = "null", beh=2); p_k1
p_d1 <- freq_fun(data=base_dat, x="dn0_d", iter = "null", beh=2); p_d1

p_y2 <- freq_fun(data=base_dat, x="yn_ya", iter = "habitat_hypoth", beh=2); p_y2
p_b2 <- freq_fun(data=base_dat, x="Bn_Ba", iter = "habitat_hypoth", beh=2); p_b2
p_k2 <- freq_fun(data=base_dat, x="kn_ka", iter = "habitat_hypoth", beh=2); p_k2
p_d2 <- freq_fun(data=base_dat, x="dn0_d", iter = "habitat_hypoth", beh=2); p_d2

grid.arrange(p_y1, p_b1, p_k1, p_d1, 
             p_y2, p_b2, p_k2, p_d2, ncol=4)

setwd("C:/Users/sabalm/Desktop/")
pdf("Freq_plots_null_habitat.pdf", width=12, height=6)

grid.arrange(p_y1, p_b1, p_k1, p_d1, 
             p_y2, p_b2, p_k2, p_d2, ncol=4)

dev.off()

# make a plot when the x-axis is not log transformed.
freq_fun_no_log <- function(data, x, iter, beh, vline){
  
  base_dat2 <- data %>%
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
  
  ag_dat <- base_dat2 %>% filter(baseline == iter & h != "o" & Beh == beh & param_name == x) %>% # UQ(sym(x)) takes x provided with quotes and makes it a symbol.
    group_by(param_value, h) %>% summarise(mean = mean(p.tot),
                                                    sd = sd(p.tot),
                                                    n = n()) %>% 
    mutate(se = sd / sqrt(n))
  
  ag_dat$h<-as.factor(ag_dat$h)
  levels(ag_dat$h) <- c("Altered", "Natural")
  
  vline_base <- param_dat %>% filter(baseline == iter) %>% select(x) %>% pull()
  
  ggplot(ag_dat, aes(x=param_value, y=(mean), fill=h, color=h)) +
    geom_vline(xintercept = vline, color="gray80", size=1, linetype="dashed") + # if not log-transformed than the equal benchmark is 1
    geom_line(size=0.5, aes(color=h)) + geom_errorbar(aes(ymax=mean+se, ymin=mean-se, color=h), width=0, size=0.5) +
    geom_point(size=2, shape=21, color="black") + theme_classic() +
    scale_color_manual(values=c("mediumpurple", "forestgreen")) +
    scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
    ylab(str_c("Frequency of move ", beh)) + xlab(x) +
    theme(legend.title=element_blank(), legend.position= "bottom") + ylim(values=c(0,1))
}

f1 <- freq_fun_no_log(data=yn_greater_dat, x="Bn", iter="null", beh=0, vline=1); f1 # same data: actual paramater value as x-axis
freq_fun(data=yn_greater_dat, x="Bn_Ba", iter="null", beh=0) # same data: log ratio as x-axis

f2 <- freq_fun_no_log(data=yn_greater_dat, x="kn", iter="null", beh=0, vline=1.3); f2
f3 <- freq_fun_no_log(data=yn_greater_dat, x="dn0", iter="null", beh=0, vline=1); f3


setwd("C:/Users/sabalm/Desktop/")
pdf("Freq_plots_yn_greater_ya.pdf", width=3, height=8)

grid.arrange(f1, f2, f3, ncol=1)

dev.off()
# update with shared legend for multiple plots (I did this for my bycatch plots, but can't remember which package/code.)






#######

p.tot_fun <- function(x, iter){
  ag_dat <- base_dat2 %>% filter(baseline == iter & h != "o" & UQ(sym(x)) != 0) %>%
    group_by(baseline, .data[[x]], h, Beh) %>% summarise(mean = mean(p.tot))
  
  ag_dat$h<-as.factor(ag_dat$h)
  ag_dat$Beh<-as.factor(ag_dat$Beh)
  ag_dat <- ag_dat[order(ag_dat$h, ag_dat$Beh),]
  
  ggplot(data=ag_dat, aes(x=h, y=mean, fill=Beh)) + geom_bar(stat="identity", color="black") +
    facet_wrap(~.data[[x]], ncol=10) + theme_bw() +
    theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
    theme(legend.key=element_blank(), legend.background=element_blank()) +
    scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
    theme(legend.title = element_blank()) +
    scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
    scale_y_continuous("Frequency of movement choices") +
    theme(legend.position = "bottom") + ggtitle(label = str_c("Iteration: null & ", x)) +
    theme(plot.title = element_text(hjust = 0.5))
}

p.tot_fun(x="yn_ya", iter="null") # this works, but we perhaps don't want all these plots.
p.tot_fun(x="Bn_Ba", iter="null") # this works, but we perhaps don't want all these plots.
p.tot_fun(x="kn_ka", iter="null") # this works, but we perhaps don't want all these plots.
p.tot_fun(x="dn0_d", iter="null") # this works, but we perhaps don't want all these plots.



####### p.tot

p.tot_fun <- function(x, iter){
  ag_dat <- base_dat2 %>% filter(baseline == iter & h != "o" & UQ(sym(x)) != 0) %>%
    group_by(baseline, .data[[x]], h, Beh) %>% summarise(mean = mean(p.tot))
  
  ag_dat$h<-as.factor(ag_dat$h)
  ag_dat$Beh<-as.factor(ag_dat$Beh)
  ag_dat <- ag_dat[order(ag_dat$h, ag_dat$Beh),]
  
  ggplot(data=ag_dat, aes(x=h, y=mean, fill=Beh)) + geom_bar(stat="identity", color="black") +
    facet_wrap(~.data[[x]], ncol=10) + theme_bw() +
    theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
    theme(legend.key=element_blank(), legend.background=element_blank()) +
    scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
    theme(legend.title = element_blank()) +
    scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
    scale_y_continuous("Frequency of movement choices") +
    theme(legend.position = "bottom") + ggtitle(label = str_c("Iteration: null & ", x)) +
    theme(plot.title = element_text(hjust = 0.5))
}

p.tot_fun(x="yn_ya", iter="null") # this works, but we perhaps don't want all these plots.
p.tot_fun(x="Bn_Ba", iter="null") # this works, but we perhaps don't want all these plots.
p.tot_fun(x="kn_ka", iter="null") # this works, but we perhaps don't want all these plots.
p.tot_fun(x="dn0_d", iter="null") # this works, but we perhaps don't want all these plots.

p.tot_fun(x="yn_ya", iter="habitat_hypoth") # this works, but we perhaps don't want all these plots.
p.tot_fun(x="Bn_Ba", iter="habitat_hypoth") # this works, but we perhaps don't want all these plots.
p.tot_fun(x="kn_ka", iter="habitat_hypoth") # this works, but we perhaps don't want all these plots.
p.tot_fun(x="dn0_d", iter="habitat_hypoth") # this works, but we perhaps don't want all these plots.


####### p

p_fun <- function(x, iter){
  ag_dat <- base_dat2 %>% filter(baseline == iter & h != "o" & UQ(sym(x)) != 0) %>%
    group_by(baseline, .data[[x]], h, Beh) %>% summarise(mean = mean(p))
  
  ag_dat$h<-as.factor(ag_dat$h)
  ag_dat$Beh<-as.factor(ag_dat$Beh)
  ag_dat <- ag_dat[order(ag_dat$h, ag_dat$Beh),]
  
  ggplot(data=ag_dat, aes(x=h, y=mean, fill=Beh)) + geom_bar(stat="identity", color="black") +
    facet_wrap(~.data[[x]], ncol=10) + theme_bw() +
    theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
    theme(legend.key=element_blank(), legend.background=element_blank()) +
    scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
    theme(legend.title = element_blank()) +
    scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
    scale_y_continuous("Frequency of movement choices") +
    theme(legend.position = "bottom") + ggtitle(label = str_c("Iteration: null & ", x)) +
    theme(plot.title = element_text(hjust = 0.5))
}

p_fun(x="yn_ya", iter="null") # this works, but we perhaps don't want all these plots.
p_fun(x="Bn_Ba", iter="null") # this works, but we perhaps don't want all these plots.
p_fun(x="kn_ka", iter="null") # this works, but we perhaps don't want all these plots.
p_fun(x="dn0_d", iter="null") # this works, but we perhaps don't want all these plots.

p_fun(x="yn_ya", iter="habitat_hypoth") # this works, but we perhaps don't want all these plots.
p_fun(x="Bn_Ba", iter="habitat_hypoth") # this works, but we perhaps don't want all these plots.
p_fun(x="kn_ka", iter="habitat_hypoth") # this works, but we perhaps don't want all these plots.
p_fun(x="dn0_d", iter="habitat_hypoth") # this works, but we perhaps don't want all these plots.









































#########################
# First plot:
y_dat <- base_dat2 %>% 
  filter(param_name %in% c("yn", "ya") & h != "o" &  # param_name %in% c("yn", "ya")
           baseline == "null")





######################
## Copy and paste parameter column (e.g., ya_ya, Bn_Ba, dn0) AND iteration (e.g., null, habitat_hypoth)


# First plot:
y_dat <- base_dat2 %>% 
  filter(param_name %in% c("yn", "ya") & h != "o" &  # param_name %in% c("yn", "ya")
           baseline == "null")
  
  # 1 - Proportion of moves by habitat: Tot
  
  # summarize data for barplot
  bar.beh<-aggregate(p.tot ~ h + Beh + yn_ya, data= y_dat, mean) #+ baseline
sum(bar.beh$p) # this should equal the number of iterations because sum to 1 for each of them.

bar.beh$h<-as.factor(bar.beh$h)
bar.beh$Beh<-as.factor(bar.beh$Beh)

bar.beh <- bar.beh[order(bar.beh$h, bar.beh$Beh),]

fig_p1 <- ggplot(data=bar.beh, aes(x=h, y=p.tot, fill=Beh)) + geom_bar(stat="identity", color="black") +
  facet_wrap(~yn_ya, ncol=10) + theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
  theme(legend.title = element_blank()) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices") +
  theme(legend.position = "bottom") + ggtitle(label = "Iteration: null & yn_ya") +
  theme(plot.title = element_text(hjust = 0.5))
fig_p1


# 2 - Proportion of moves by habitat: By habitat

# summarize data for barplot
bar.beh<-aggregate(p ~ h + Beh + yn_ya, data=y_dat, mean)
sum(bar.beh$p) # this should equal the number of iterations x2 because sum to 1 for each of them.

bar.beh$h<-as.factor(bar.beh$h)
bar.beh$Beh<-as.factor(bar.beh$Beh)

bar.beh <- bar.beh[order(bar.beh$h, bar.beh$Beh),]

fig_p2 <- ggplot(data=bar.beh, aes(x=h, y=p, fill=Beh)) + geom_bar(stat="identity", color="black") +
  facet_wrap(~yn_ya, ncol=10) + theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
  theme(legend.title = element_blank()) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices") +
  theme(legend.position = "bottom") + ggtitle(label = "Iteration: null") +
  theme(plot.title = element_text(hjust = 0.5))
fig_p2


# 3 - Freq of move 0 by habitat by iterations

# Aggregate  by Wstart (salmon size)
ag.param<-aggregate(p.tot ~ h + yn_ya, data=y_dat, mean)
a<-aggregate(p.tot ~ h + yn_ya, data=y_dat, sd); colnames(a)[3]<-"sd"
b<-aggregate(p.tot ~ h + yn_ya, data=y_dat, length); colnames(b)[3]<-"n"
ag.param<-join(ag.param, a); ag.param<-join(ag.param, b)
ag.param$se<-ag.param$sd / sqrt(ag.param$n)

ag.param$h<-as.factor(ag.param$h)
levels(ag.param$h) <- c("Altered", "Natural")


# Plots!

plot_var <- ggplot(data=ag.param, aes(x=(yn_ya), y=p.tot, fill=h, color=h)) + 
  geom_line(size=0.5, aes(color=h)) +
  geom_errorbar(aes(ymax=p.tot + se, ymin=p.tot - se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") + 
  theme_classic() + 
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab("Frequency of move 0")  +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom", legend.background = element_rect(fill="transparent")) + 
  ggtitle(label = "Iteration: null") +
  theme(plot.title = element_text(hjust = 0.5))

plot_var


# FIGURE OUT HOW TO LOOP THROUGH TO MAKE PLOTS
# https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/
# https://datascience.fm/multiple-plots-using-ggplot2/

