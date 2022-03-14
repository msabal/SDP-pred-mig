#### Iterate baseline parameters over Ba and Bn

# load libraries
library(abind); library(ggplot2); library(plyr); library(reshape2)

#remove scientific notation
options(scipen=999)

#### Iterate Main Function over varying parameters: N (proportion of natural habitat.)

N <- seq(0.1, 0.9, by=0.2)

OUT.N <- list()

start.time <- Sys.time() # time how long the while loop takes


# Start looping MAIN_FUN over different qa values
for(i in 1:length(N)) {
  
  OUT <- MAIN_FUN(Wc, A, t, U, Wmax, Wmin, Amax, # state vars, constraints & beh choice (vars we will for loop over)
                  E, q, a, Alpha, d, v, f, g, c, j, Bu, Bw, M, m, y, P, z, # vars in functions
                  ya, yn, yo, dn0, Ba, Bn, Bo, ka, kn, # vars that vary by habitat (h.vec)
                  Ws, r, Smax, W, # vars for Terminal fitness function
                  Wstep.n, Wstep, tmax, seeds, F.vec, N[i])
  
  colnames(OUT) <- c("Wstart", "Beh", "p", "h", "p.tot", "S.cum.riv", "G.riv", "G.ocean", "dur", "Fit")
  
  OUT$N <- rep(N[i], length(OUT$Wstart)) # add column with var value for that iteration.
  
  OUT.N[[i]] <- OUT
  
} # end loop.

end.time <- Sys.time() # time how long the while loop takes
program.duration <- end.time - start.time
program.duration # 53 mins

DF.N<-ldply(OUT.N, as.vector)

## Export DF.YO
write.csv(DF.N, "G:\\My Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\results\\DF.N.csv")




# Aggregate  by Wstart (salmon size)
# summarize data for barplot
bar.cp<-aggregate(p.tot ~ h + N, data=subset(DF.N, Beh == 0), mean)
a<-aggregate(p.tot ~ h + N, data=subset(DF.N, Beh == 0), sd); colnames(a)[3]<-"sd"
b<-aggregate(p.tot ~ h + N, data=subset(DF.N, Beh == 0), length); colnames(b)[3]<-"n"
bar.cp<-join(bar.cp, a); bar.cp<-join(bar.cp, b)
bar.cp$se<-bar.cp$sd / sqrt(bar.cp$n)

bar.cp$h<-as.factor(bar.cp$h)
levels(bar.cp$h) <- c("Altered", "Natural")


# Plots!
plot_N <- ggplot(data=bar.cp, aes(x=N, y=p.tot, fill=h, color=h)) +
  geom_vline(xintercept = 0.5, linetype="dashed",  color = "gray24", size=0.5) +
  geom_line(size=0.5, aes(color=h)) +
  geom_errorbar(aes(ymax=p.tot + se, ymin=p.tot - se, color=h), width=0, size=0.5) +
  geom_point(size=2, shape=21, color="black") +
  theme_classic() + ylim(c(0,1)) +
  scale_color_manual(values=c("mediumpurple", "forestgreen")) +
  scale_fill_manual(values=c("mediumpurple", "forestgreen")) +
  ylab("Frequency of move 0") + xlab("N (proportion of altered habitat)") +
  theme(axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=11), axis.title.x = element_text(size=11),
        legend.title = element_blank(), legend.text = element_text(size=11)) +
  theme(legend.position = c(0.8, 0.2), legend.background = element_rect(fill="transparent"))
plot_N

setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_N.pdf", width=4.5, height=4)

plot_N

dev.off()



