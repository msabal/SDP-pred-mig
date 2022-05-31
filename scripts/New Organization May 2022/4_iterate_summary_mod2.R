#### Iterate main function over multiple values for one parameter

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)
library(gridExtra); library(grid)

#remove scientific notation
options(scipen=999)

#### Iterate Main Function over varying parameters
kn <- 1.3

ka <- seq(0.9, 1.3, by=0.3)

OUT.SUM <- list() # make object to save function output

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different parameter values
for(i in 1:length(ka)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0, Ba, Bn, Bo, ka[i], kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$ka <- rep(ka[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.SUM[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration

DF.SUM<-ldply(OUT.SUM, as.vector)


#### Iterate Main Function over varying parameters: IF looking at ratio of two parameters.
ka <- 1.3

kn <- seq(0.9, 1.3, by=0.1)

OUT.SUM2 <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different qa values
for(i in 1:length(kn)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0, Ba, Bn, Bo, ka, kn[i], # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$kn <- rep(kn[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.SUM2[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration 

DF.SUM2<-ldply(OUT.SUM2, as.vector)

## Export DF.QN
#write.csv(DF.KN, "H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.KN.V2.csv")

# Join the two dataframes

DF.SUM$kn <- rep(1.3, length(DF.SUM$Wstart)) # make kn column and set all values to 1
DF.SUM2$ka <- rep(1.3, length(DF.SUM2$Wstart)) # make ka column and set all values to 1

DF.ALL <- rbind(DF.SUM, DF.SUM2)

# Calculate Qn/Qa ratios!
DF.ALL$kn_ka <- DF.ALL$kn / DF.ALL$ka



#### Key plots for tracks

# 1 - Proportion of moves by habitat - Total: Beh ~ h
# 2 - Proportion of moves by habitat - By Habitat: Beh ~ h
# 3 - Freq of move 0 by habitat by iterations


# Select all graph code: find last var and replace with either new param (e.g., ka) and data (DF.SUM)
  # Or replace with ratio (e.g., kn_ka) and data (DF.ALL)

###################

# 1 - Proportion of moves by habitat: Tot

# summarize data for barplot
bar.beh<-aggregate(p.tot ~ h + Beh + ka, data=subset(DF.SUM, h != "o"), mean)
sum(bar.beh$p) # this should equal the number of iterations becuase sum to 1 for each of them.

bar.beh$h<-as.factor(bar.beh$h)
bar.beh$Beh<-as.factor(bar.beh$Beh)

bar.beh <- bar.beh[order(bar.beh$h, bar.beh$Beh),]

fig_p1 <- ggplot(data=bar.beh, aes(x=h, y=p.tot, fill=Beh)) + geom_bar(stat="identity", color="black") +
  facet_wrap(~ka) + theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
  theme(legend.title = element_blank()) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices") +
  theme(legend.position = "bottom")
fig_p1


# 2 - Proportion of moves by habitat: By habitat

# summarize data for barplot
bar.beh<-aggregate(p ~ h + Beh + ka, data=subset(DF.SUM, h != "o"), mean)
sum(bar.beh$p) # this should equal the number of iterations x2 because sum to 1 for each of them.

bar.beh$h<-as.factor(bar.beh$h)
bar.beh$Beh<-as.factor(bar.beh$Beh)

bar.beh <- bar.beh[order(bar.beh$h, bar.beh$Beh),]

fig_p2 <- ggplot(data=bar.beh, aes(x=h, y=p, fill=Beh)) + geom_bar(stat="identity", color="black") +
  facet_wrap(~ka) + theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "steelblue1", "royalblue"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
  theme(legend.title = element_blank()) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices") +
  theme(legend.position = "bottom")
fig_p2


# 3 - Freq of move 0 by habitat by iterations

# Aggregate  by Wstart (salmon size)
ag.param<-aggregate(p.tot ~ h + ka, data=subset(DF.SUM, Beh == 0), mean)
a<-aggregate(p.tot ~ h + ka, data=subset(DF.SUM, Beh == 0), sd); colnames(a)[3]<-"sd"
b<-aggregate(p.tot ~ h + ka, data=subset(DF.SUM, Beh == 0), length); colnames(b)[3]<-"n"
ag.param<-join(ag.param, a); ag.param<-join(ag.param, b)
ag.param$se<-ag.param$sd / sqrt(ag.param$n)

ag.param$h<-as.factor(ag.param$h)
levels(ag.param$h) <- c("Altered", "Natural")


# Plots!

plot_var <- ggplot(data=ag.param, aes(x=ka, y=p.tot, fill=h, color=h)) + 
  geom_line(size=0.5, aes(color=h)) +
  geom_errorbar(aes(ymax=p.tot + se, ymin=p.tot - se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") + 
  theme_classic() + ylim(c(0,1)) +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab("Frequency of move 0")  +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom", legend.background = element_rect(fill="transparent"))

plot_var


# Av. growth plots

fig_gr <- ggplot(data=DF.SUM, aes(x=ka, y=G.riv/dur, color=Wstart)) +
  geom_line() + geom_point() + theme_classic() + 
  theme(legend.position = "bottom") + ylim(c(0,1.5))
fig_gr

fig_go <- ggplot(data=DF.SUM, aes(x=ka, y=G.ocean/(tmax - dur), color=Wstart)) +
  geom_line() + geom_point() + theme_classic() +
  theme(legend.position = "bottom") + ylim(c(0,1.5))
fig_go

## Save all plots together

grid.arrange(fig_p1, plot_var,
             fig_gr, fig_go,
             nrow = 2,
             bottom=textGrob("PARAMETERS: X, Y, Z"))


ggsave(
  "C:/Users/sabalm/Desktop//sdp_ggplot.png",
  plot = grid.arrange(fig_p1, plot_var,
                      fig_gr, fig_go,
                      nrow = 2,
                      bottom=textGrob("PARAMETERS: X, Y, Z")),# put ggplot in here...
  width = 10,
  height = 10,
  dpi = 1200
)


