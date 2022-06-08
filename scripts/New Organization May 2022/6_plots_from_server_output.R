
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
         dn0_d = log(dn0/d)) %>% 
  mutate(hab_it = ifelse(param_name %in% c("yn", "ya"), "yn_ya", 
                         ifelse(param_name %in% c("Bn", "Ba"), "Bn_Ba", 
                                ifelse(param_name %in% c("kn", "ka"), "kn_ka", "dn0_d"))))

colnames(base_dat2) # all ratio columns were calculated, just can't see them all in View.

View(base_dat %>% select(baseline, param_value, param_name) %>% distinct()) # hmmm missing a bunch of null iterations...



# First plot:
y_dat <- base_dat2 %>% 
  filter(hab_it == "Bn_Ba" & h != "o")
  
  # 1 - Proportion of moves by habitat: Tot
  
  # summarize data for barplot
  bar.beh<-aggregate(p.tot ~ h + Beh + Bn_Ba + baseline, data= y_dat, mean)
sum(bar.beh$p) # this should equal the number of iterations because sum to 1 for each of them.

bar.beh$h<-as.factor(bar.beh$h)
bar.beh$Beh<-as.factor(bar.beh$Beh)

bar.beh <- bar.beh[order(bar.beh$h, bar.beh$Beh),]

fig_p1 <- ggplot(data=bar.beh, aes(x=h, y=p.tot, fill=Beh)) + geom_bar(stat="identity", color="black") +
  facet_wrap(~baseline + Bn_Ba, ncol = 1) + theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
  theme(legend.title = element_blank()) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices") +
  theme(legend.position = "bottom")
fig_p1


