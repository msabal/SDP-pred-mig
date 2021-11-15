

smfish <- subset(bigfish, Weight_ > 7 & Weight_ < 50)

plot(Weight_ ~ Length, data=smfish) # full relevant dataset

# STEP 3: Fit function to the data (non-linear).
# Do I need to use the function nls? Formula from literature: W=a*L^b

# Try nls model with form W=a*L^b
lt.wt.mod <- nls(Weight_ ~ a*Length^b, data = smfish, start = list(a=0.05, b=1))
summary(lt.wt.mod) #fitted function is highly significant
coef(lt.wt.mod) # a=0.00001836394, b=2.89840722188 fitted parameter values



################# MY RISK EQUATIONS ######################
CAP.W <- function(W, a, b, P, c){ c-(1.82*((W/a)^(1/b))/P) }
curve(CAP.W(W, a=0.000018, b=2.89, P=236, c=0.861), xname="W", xlim=c(7,20), ylim=c(-0.1,0.4), ylab="cature probability (Nobriga et al. 2020)")
abline(h=0,lty="dashed")

curve(CAP.W(W, a=0.000018, b=2.89, P=336, c=0.861), xname="W", xlim=c(7,20), add=T, col="slateblue")


MY.LIN.SSM <- function(W, a, b){ a*W + b}
curve(MY.LIN.SSM(W, a=-0.125, b=1.875), xname="W", xlim=c(7,20), ylim=c(-0.1,1),  ylab="contribution to mortality")
abline(h=0, lty="dashed")


SURV.FUN <- function(W, Bu, Bh, Bw, M, m, y, P){ (1-M*(Bu + Bh + Bw*W^m))^(y*P) }

#Test RISK.FUN
SURV.FUN(W=12, Bu=1, Bh=1, Bw=2, M=0.002, m=-0.37, y=1, P=20) # good.

# solving for Beta-W
Beta.W <- function(X, Bw, m){ Bw*X^m }

setwd("C:/Users/Megan/Desktop/")
pdf("SSM_curves.pdf", width=4.5, height=3.5)

curve(Beta.W(X, Bw=2, m=-0.37), xname="X", xlim=c(7,20), ylim=c(0,1.1), ylab="contrib. daily mortality rate")
abline(h=1,lty="dashed")
      
curve(Beta.W(X, Bw=20, m=-1.5), xname="X", xlim=c(7,20), ylim=c(0,1.1), add=T, col="violet")
curve(MY.LIN.SSM(W, a=-0.125, b=1.875), xname="W", xlim=c(7,20), ylim=c(-0.1,1),  add=T, col="slateblue")

dev.off()
