#### Iterate baseline parameters over Ba and Bn

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

#### Iterate Main Function over varying parameters
ya    <- 1  #can change
yn    <- 1  #can change

yo <- seq(0.5,1.5, by=0.2)

OUT.YO <- list()

start.time <- Sys.time() # time how long the while loop takes


# Start looping MAIN_FUN over different qa values
for(i in 1:length(yo)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo[i], dn0, Ba, Bn, Bo, ka, kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N)
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$yo <- rep(yo[i], length(OUT$Wstart)) # add column with var value for that iteration.
  
  OUT.YO[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 53 mins

DF.YO<-ldply(OUT.YO, as.vector)

## Export DF.YO
write.csv(DF.YO, "G:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.YO.csv")



# #### Iterate Main Function over varying parameters
# Ba <- 1
# 
# Bn <- seq(0.1,0.9, by=0.2)
# 
# OUT.BN <- list()
# 
# start.time <- Sys.time() # time how long the while loop takes
# 
# # Start looping MAIN_FUN over different qa values
# for(i in 1:length(Bn)) {
#   
#   OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
#                   E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
#                   ya, yn, yo, dn0, Ba, Bn[i], Bo, ka, kn, # vars that vary by habitat (h.vec)
#                   Ws, r, Smax, W, # vars for Terminal fitness function
#                   Wstep.n, Wstep, tmax, seeds, F.vec, N)
#   
#   colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
#   
#   OUT$Bn <- rep(Bn[i], length(OUT$Wstart)) # add column with seeds value for that iteration.
#   
#   OUT.BN[[i]] <- OUT
#   
# } # end loop.
# 
# end.time <- Sys.time() # time how long the while loop takes
# program.duration <- end.time - start.time
# program.duration # 1.83 hours for seeds length = 10!
# 
# DF.BN<-ldply(OUT.BN, as.vector)
# 
# ## Export DF.QA
# write.csv(DF.BN, "H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.BN.csv")
# 
# # Join DF.BA and DF.BN
# 
# DF.BA$Bn <- rep(1, length(DF.BA$Wstart)) # make Bn column and set all values to 1
# DF.BN$Ba <- rep(1, length(DF.BN$Wstart)) # make Ba column and set all values to 1
# 
# DF.B <- rbind(DF.BA, DF.BN)
# 
# # Calculate Dn0/d ratios!
# DF.YO$Bn_Ba <- DF.YO$Bn / DF.YO$Ba   # have ratios up to 10 -  log transform in plots!
# 
# ## Export DF.Y
# write.csv(DF.YO, "H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.YO.csv")
# DF.YO <- read.csv("H:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.YO.csv", sep=",")

# Aggregate  by Wstart (salmon size)
# summarize data for barplot
bar.cp<-aggregate(p.tot ~ h + yo, data=subset(DF.YO, Beh == 0), mean)
a<-aggregate(p.tot ~ h + yo, data=subset(DF.YO, Beh == 0), sd); colnames(a)[3]<-"sd"
b<-aggregate(p.tot ~ h + yo, data=subset(DF.YO, Beh == 0), length); colnames(b)[3]<-"n"
bar.cp<-join(bar.cp, a); bar.cp<-join(bar.cp, b)
bar.cp$se<-bar.cp$sd / sqrt(bar.cp$n)

bar.cp$h<-as.factor(bar.cp$h)
levels(bar.cp$h) <- c("Altered", "Natural")


# Plots!
plot_yo <- ggplot(data=bar.cp, aes(x=yo, y=p.tot, fill=h, color=h)) +
  geom_vline(xintercept = 1, linetype="dashed",  color = "gray24", size=0.5) +
  geom_line(size=0.5, aes(color=h)) +
  geom_errorbar(aes(ymax=p.tot + se, ymin=p.tot - se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") +
  theme_classic() + ylim(c(0,1)) +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab("Frequency of move 0") + xlab("yo when ya=1 and yn=1") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.8, 0.2), legend.background = element_rect(fill="transparent")) +
  annotate(geom="text", x=0.75, y=1, label="ocean safer than river", color="skyblue", fontface="bold") +
  annotate(geom="text", x=1.25, y=1, label="ocean riskier than river", color="skyblue", fontface="bold")

plot_yo

setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_yo.pdf", width=4.5, height=4)

plot_yo

dev.off()
