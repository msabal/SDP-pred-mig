

smfish <- subset(bigfish, Weight_ > 7 & Weight_ < 50)

plot(Weight_ ~ Length, data=smfish) # full relevant dataset

# STEP 3: Fit function to the data (non-linear).
# Do I need to use the function nls? Formula from literature: W=a*L^b

# Try nls model with form W=a*L^b
lt.wt.mod <- nls(Weight_ ~ a*Length^b, data = smfish, start = list(a=0.05, b=1))
summary(lt.wt.mod) #fitted function is highly significant
coef(lt.wt.mod) # a=0.00001836394, b=2.89840722188 fitted parameter values



################# MY RISK EQUATIONS ######################
CAP.W <- function(W, a, b, P){ 0.861-(1.82*((W/a)^(1/b))/P) }
curve(CAP.W(W, a=0.000018, b=2.89, P=236), xname="W", xlim=c(7,20), ylim=c(-0.1,0.5), ylab="contrib. daily mortality rate")
abline(h=0,lty="dashed")

curve(CAP.W(W, a=0.000018, b=2.89, P=336), xname="W", xlim=c(7,20), add=T, col="slateblue")


