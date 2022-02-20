
#### Iterate baseline parameters over n=10 randomly pulled 50% probability SEEDS

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

#### Iterate Main Function over varying parameters

seeds <- seq(1,10, by=1)

OUT.SEEDS <- list()

start.time <- Sys.time() # time how long the while loop takes

# Start looping MAIN_FUN over different h.vec seed values (seeds)
for(i in 1:length(seeds)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, # vars in functions
                  qa, qn, ya, yn, yo, dn0, Ba, Bn, Bo, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds[i], F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$seeds <- rep(seeds[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
  
  OUT.SEEDS[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 1.83 hours for seeds length = 10!

DF.10SEEDS<-ldply(OUT.SEEDS, as.vector)

## Export DF.SEEDS for Figures 1 and 2!
write.csv(DF.10SEEDS, "C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.10SEEDS.FIGS1&2.csv")
DF.10SEEDS <- read.csv("C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.10SEEDS.FIGS1&2.csv", sep=",")

## Summarize DF.10SEEDS
mean(DF.10SEEDS[!duplicated(DF.10SEEDS[c(1,9)]), 9])
sd(DF.10SEEDS[!duplicated(DF.10SEEDS[c(1,9)]), 9])

mean(DF.10SEEDS$S.cum.riv)
sd(DF.10SEEDS$S.cum.riv)


DF.10SEEDS$mean.G.riv <- DF.10SEEDS$G.riv / DF.10SEEDS$dur
mean(DF.10SEEDS$mean.G.riv)
sd(DF.10SEEDS$mean.G.riv)

DF.10SEEDS$mean.G.ocean <- DF.10SEEDS$G.ocean / (60 - DF.10SEEDS$dur)
mean(DF.10SEEDS$mean.G.ocean)
sd(DF.10SEEDS$mean.G.ocean)


## Make bar plot of choices

# # Melt dataframe to get into long format
# df.l.beh <- DF.10SEEDS[,c("Wstart", "seeds", "p0.n", "p1.n", "p2.n")]
# df.l.beh <- melt(df.l.beh, variable.name = "Beh", value.name = "p", id.vars = c("Wstart", "seeds"))
# levels(df.l.beh$Beh) <- c("0", "1", "2")
# df.l.beh$h <- rep("n", length(df.l.beh$Wstart))
# 
# # by habitat and movement choice: altered
# df.l.beh1 <- DF.10SEEDS[,c(1,9,10,11,12)]
# df.l.beh1 <- melt(df.l.beh1, variable.name = "Beh", value.name = "p", id.vars = c("Wstart", "seeds"))
# levels(df.l.beh1$Beh) <- c("0", "1", "2")
# df.l.beh1$h <- rep("a", length(df.l.beh1$Wstart))
# 
# df.l.beh.h <- rbind(df.l.beh, df.l.beh1)



# summarize data for barplot
bar.cp<-aggregate(p ~ h + Beh, data=DF.10SEEDS, mean)
a<-aggregate(p ~ h + Beh, data=DF.10SEEDS, sd); colnames(a)[3]<-"sd"
b<-aggregate(p ~ h + Beh, data=DF.10SEEDS, length); colnames(b)[3]<-"n"
bar.cp<-join(bar.cp, a); bar.cp<-join(bar.cp, b)
bar.cp$se<-bar.cp$sd / sqrt(bar.cp$n)

bar.cp$h<-as.factor(bar.cp$h)
bar.cp$Beh<-as.factor(bar.cp$Beh)


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