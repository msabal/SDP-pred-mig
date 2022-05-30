
# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)


#### Run MAIN_FUN_TRACKS for 1 set of parameters


data_tracks <- MAIN_FUN_TRACKS(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
              E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
               ya, yn, yo, dn0, Ba, Bn, Bo, ka, kn, # vars that vary by habitat (h.vec)
               Ws, r, Smax, W, # vars for Terminal fitness function
               Wstep.n, Wstep, tmax, seeds, F.vec, N)

data_tracks


#### Key plots for tracks

# 1 - Tracks: A ~ Time
# 2 - Growth: W ~ Time
# 3 - Growth: SGR ~ Time
# 4 - Survival: S.day ~ Time
# 5 - Survival: S.cum ~ Time
# 6 - Proportion of moves by habitat: Beh ~ h

###################

# 1 - Tracks: A ~ Time

# Make vector of how to color A x-axis labels by habitat type.
h.col <- ifelse(data_tracks$h == "a", "mediumpurple",
                ifelse(data_tracks$h == "n", "forestgreen", "blue3"))

# Simulated salmon tracks.

fig_tracks <- ggplot(data=data_tracks, aes(x=Time, y=A, color=as.factor(Wstart))) +
  geom_line(size=1, position=position_dodge(0.4)) + geom_point(position=position_dodge(0.4)) +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.position="bottom", legend.background=element_blank()) + coord_equal() +
  scale_x_continuous(breaks = seq(1,tmax,2)) +
  scale_y_continuous(breaks = seq(1,Amax,1)) +
  theme(axis.text.y = element_text(color=h.col, face="bold"), axis.text.x = element_text(face = "bold")) +
  ylab("Area") + scale_color_brewer(palette = "Set3", name= "Starting salmon size (g)")
fig_tracks


# 2 - Growth: W ~ Time

fig_g1 <- ggplot(data=data_tracks, aes(x=Time, y=W, color=as.factor(Wstart))) +
  geom_line(size=1, position=position_dodge(0.4)) + geom_point(position=position_dodge(0.4)) +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.position="bottom", legend.background=element_blank()) + coord_equal() +
  scale_x_continuous(breaks = seq(1,tmax,2)) +
  ylab("Salmon weight (g)") + scale_color_brewer(palette = "Set3", name= "Starting salmon size (g)")
fig_g1

# 3 - Growth: SGR ~ Time

data_tracks$SGR <- NA # output to store calculated SGR (specific growth rates)

# for loop for specific growth rates (SGR)

  for(t in 1:(length(data_tracks$Time))){
    
    data_tracks[t,10] <- ifelse(data_tracks[t,2] == data_tracks[t+1,2],
                                (log(data_tracks[t+1,9]) - log(data_tracks[t,9])) *100,
                                NA)
  } # end of loop.

fig_g2 <- ggplot(data=data_tracks, aes(x=Time, y=SGR, color=as.factor(Wstart))) +
  geom_line(size=1, position=position_dodge(0.4)) + geom_point(position=position_dodge(0.4)) +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.position="bottom", legend.background=element_blank()) + coord_equal() +
  scale_x_continuous(breaks = seq(1,tmax,2)) +
  scale_color_brewer(palette = "Set3", name= "Starting salmon size (g)")
fig_g2
# these look weird. maybe because of my math equations but also my growth increments.
  # there are many time steps where salmon do not grow, which gives a specific growth rate of 0.
  # plus some where they lose weight, so these are negative.

aggregate(SGR ~ Wstart, data = data_tracks, mean)
# Larger fish have a lower specific growth rate (this is correct.)

aggregate(SGR ~ h, data = data_tracks, mean)
# SGR: a < n < o (this is also correct)


# 4 - Survival: S.day ~ Time

fig_s1 <- ggplot(data=data_tracks, aes(x=Time, y=S.day, color=as.factor(Wstart))) +
  geom_line(size=1, position=position_dodge(0.4)) + geom_point(position=position_dodge(0.4)) +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.position="bottom", legend.background=element_blank()) +
  scale_x_continuous(breaks = seq(1,tmax,2)) +
  scale_color_brewer(palette = "Set3", name= "Starting salmon size (g)")
fig_s1

aggregate(S.day ~ Wstart, data = data_tracks, mean)
aggregate(S.day ~ h, data = data_tracks, mean)


# 5 - Survival: S.cum ~ Time

fig_s2 <- ggplot(data=data_tracks, aes(x=Time, y=S.cum, color=as.factor(Wstart))) +
  geom_line(size=1, position=position_dodge(0.4)) + geom_point(position=position_dodge(0.4)) +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.position="bottom", legend.background=element_blank()) +
  scale_x_continuous(breaks = seq(1,tmax,2)) +
  scale_color_brewer(palette = "Set3", name= "Starting salmon size (g)")
fig_s2

aggregate(S.cum ~ Wstart, data = data_tracks, mean)
aggregate(S.cum ~ h, data = data_tracks, mean)


# 6 - Proportion of moves by habitat: Beh ~ h

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
  theme(legend.position = "bottom")
fig_p

#scale_fill_manual(values=c("lightsteelblue2", "paleturquoise2", "thistle2"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)","move 2 (40 km/d)")) +


## Save all plots together
library(gridExtra); library(grid)

grid.arrange(fig_tracks, fig_p, fig_g1, fig_s2,
             nrow = 2,
             bottom=textGrob("PARAMETERS: X, Y, Z"))


ggsave(
  "C:/Users/sabalm/Desktop//sdp_ggplot.png",
  plot = grid.arrange(fig_tracks, fig_p, fig_g1, fig_s2,
                      nrow = 2,
                      bottom=textGrob("PARAMETERS: X, Y, Z")),# put ggplot in here...
  width = 10,
  height = 10,
  dpi = 1200
)



