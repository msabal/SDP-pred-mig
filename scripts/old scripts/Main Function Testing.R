
# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)


#### Parameters

# seeds for h.vec
seeds <- 1 # can change

# W: salmon weight (g)
Wmin <- 7
Wmax <- 50
Wstep <- 0.1 
Wstep.n <- ((Wmax-Wmin)/Wstep)

# A: salmon area
Amin <- 1
Amax <- 26

# t: time
tmin <- 1
tmax <- 60
# Behavioral choice
U <- c(0, 1, 2)

# Terminal fitness
Ws    <- 40
r     <- 0.1
Smax  <- 0.3

# Growth
E     <- 0.04
qa    <- 0.7 #can change
qn    <- 1    # can change
a     <- 0.86
Alpha <- 0.00607
d     <- 1
dn0   <- 0.7
v     <- 0.027
f     <- 0.5
g     <- 2
c     <- 40
j     <- 0.07

# Risk
Bu    <- c(0.7, 1, 0.7) # B0, B1, B2 (can concatenate because we will loop over behavior choices?)
Ba    <- 1
Bn    <- 0.7 #can change
Bo    <- 1
Bw    <- 2
M     <- 0.002
m     <- -0.37
ya    <- 1  #can change
yn    <- 1  #can change
yo    <- 1  #can change
P     <- 20


# Check Main Function works.

OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
         E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
         qa, qn, ya, yn, yo, # vars that vary by habitat (h.vec)
         Ws, r, Smax, W, # vars for Terminal fitness function
         Wstep.n, Wstep, tmax, seeds, F.vec)

OUT

colnames(OUT) <- c("Wstart", "S.cum.riv", "G.riv", "G.ocean", "dur", "p0.n", "p1.n", "p2.n", "p0.a", "p1.a"," p2.a")



#### Iterate Main Function over varying parameters

seeds <- seq(1,10, by=1)

OUT.SEEDS <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different h.vec seed values (seeds)
for(i in 1:length(seeds)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                     E, q, a, Alpha, d, v, f, g, c, j, Bu, Bh, Bw, M, m, y, P, # vars in functions
                     qa, qn, ya, yn, yo, # vars that vary by habitat (h.vec)
                     Ws, r, Smax, W, # vars for Terminal fitness function
                     Wstep.n, Wstep, tmax, seeds[i], F.vec)

  colnames(OUT) <- c("Wstart", "S.cum.riv", "G.riv", "G.ocean", "dur", "p0.n", "p1.n", "p2.n", "p0.a", "p1.a"," p2.a")
  
  OUT$seeds <- rep(seeds[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.SEEDS[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 1.83 hours for seeds length = 10!

DF.10SEEDS<-ldply(OUT.SEEDS, as.vector)

## Export DF.SEEDS for Figures 1 and 2!
write.csv(DF.10SEEDS, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.10SEEDS.FIGS1&2.csv")


## Summarize DF.10SEEDS
mean(DF.10SEEDS$dur)
sd(DF.10SEEDS$dur)

mean(DF.10SEEDS$S.cum.riv)
sd(DF.10SEEDS$S.cum.riv)


DF.10SEEDS$mean.G.riv <- DF.10SEEDS$G.riv / DF.10SEEDS$dur
mean(DF.10SEEDS$mean.G.riv)
sd(DF.10SEEDS$mean.G.riv)

DF.10SEEDS$mean.G.ocean <- DF.10SEEDS$G.ocean / (60 - DF.10SEEDS$dur)
mean(DF.10SEEDS$mean.G.ocean)
sd(DF.10SEEDS$mean.G.ocean)


## Make bar plot of choices

# Melt dataframe to get into long format
df.l.beh <- DF.10SEEDS[,c("Wstart", "seeds", "p0.n", "p1.n", "p2.n")]
df.l.beh <- melt(df.l.beh, variable.name = "Beh", value.name = "p", id.vars = c("Wstart", "seeds"))
levels(df.l.beh$Beh) <- c("0", "1", "2")
df.l.beh$h <- rep("n", length(df.l.beh$Wstart))

# by habitat and movement choice: altered
df.l.beh1 <- DF.10SEEDS[,c(1,9,10,11,12)]
df.l.beh1 <- melt(df.l.beh1, variable.name = "Beh", value.name = "p", id.vars = c("Wstart", "seeds"))
levels(df.l.beh1$Beh) <- c("0", "1", "2")
df.l.beh1$h <- rep("a", length(df.l.beh1$Wstart))

df.l.beh.h <- rbind(df.l.beh, df.l.beh1)



# summarize data for barplot
bar.cp<-aggregate(p ~ h + Beh, data=df.l.beh.h, mean)
a<-aggregate(p ~ h + Beh, data=df.l.beh.h, sd); colnames(a)[3]<-"sd"
b<-aggregate(p ~ h + Beh, data=df.l.beh.h, length); colnames(b)[3]<-"n"
bar.cp<-join(bar.cp, a); bar.cp<-join(bar.cp, b)
bar.cp$se<-bar.cp$sd / sqrt(bar.cp$n)

bar.cp$h<-as.factor(bar.cp$h)
bar.cp$Beh<-as.factor(bar.cp$Beh)

#levels(bar.cp$h)<-c("hatchery", "wild-us", "wild-ds")
#levels(bar.cp$Beh)<-c("no predator", "predator")

bar.cp <- bar.cp[order(bar.cp$h, bar.cp$Beh),]
bar.cp$pcum <-  NA
bar.cp$pcum[1] <- bar.cp$p[1] + bar.cp$p[2] + bar.cp$p[3]
bar.cp$pcum[2] <- bar.cp$p[3] + bar.cp$p[2]
bar.cp$pcum[3] <- bar.cp$p[3]
bar.cp$pcum[4] <- bar.cp$p[4] + bar.cp$p[5] + bar.cp$p[6]
bar.cp$pcum[5] <- bar.cp$p[6] + bar.cp$p[5]
bar.cp$pcum[6] <- bar.cp$p[6]


setwd("C:/Users/Megan/Desktop/")
pdf("Fig2_habitat_summary.pdf", width=5, height=4)

ggplot(data=bar.cp, aes(x=h, y=p, fill=Beh)) + geom_bar(stat="identity") +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "paleturquoise2", "thistle2"), labels = c("move 0 (0 km/d)", "move 1 (20 km/d)", "move 2 (40 km/d)")) +
  theme(axis.text.y = element_text(size=13), axis.text.x = element_text(size=13),
        axis.title.y = element_text(size=13), axis.title.x = element_text(size=13),
        legend.title = element_blank()) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices") +
  geom_errorbar(aes(ymax=pcum + se, ymin=pcum - se), width=0.03, size=0.1)

dev.off()






# Plot - stacked bar
ggplot(data=df.l.beh.h, aes(x=h, y=p, fill=Beh)) + geom_bar(stat="summary", fun = "mean") +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "paleturquoise2", "thistle2")) +
  theme(axis.text.y = element_text(size=13), axis.text.x = element_text(size=13),
        axis.title.y = element_text(size=13), axis.title.x = element_text(size=13)) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices")




# Plot - boxplots
ggplot(data=df.l.beh.h, aes(x=h, y=p, fill=Beh)) + geom_boxplot() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.key=element_blank(), legend.background=element_blank()) +
  scale_fill_manual(values=c("lightsteelblue2", "paleturquoise2", "thistle2")) +
  theme(axis.text.y = element_text(size=13), axis.text.x = element_text(size=13),
        axis.title.y = element_text(size=13), axis.title.x = element_text(size=13)) +
  scale_x_discrete("Shoreline Habitat", labels = c("a" = "Altered","n" = "Natural")) +
  scale_y_continuous("Frequency of movement choices")