 # Developing Parameters and State Dynamics
  ## background code in figuring out how to choose final model parameters and dynamics



# State variable X(t) body mass (g)
##  For two scenarios (160 mm) and (220 mm), need to estimate range of body weights (g).
##  I could do this with my salmon data from my field experiments and then extrapolate
##  to the bigger sizes. I need to plot WT by FL, determine the 95% prediction interval
##  (NOT confidence interval) and then predict the upper and lower estimates for 160 and 220.
##  These values would be my xcrit and xmax for the model.

# Here is the code I used on my Big Enclosure dataset

# https://rpubs.com/Bio-Geek/71339

WT<-data$WT
FL<-data$FL

mod1 <- lm(WT ~ FL)
summary(mod1)
predict(mod1, newdata = data.frame(FL = 220), interval = "prediction", level = 0.99)

plot(data$WT ~ data$FL)

set.seed(4172018)
x <- runif(30, 0, 10)
y <- 1 + 2 * x + rnorm(30, 0, 3)
mod <- lm(y~x)
predict(mod, newdata = data.frame(x = 5), interval = "pre")




# Plotting functions while exploring state dynamic relationships

## X(t+1) = X(t) + energy gained - metabolic costs
## Metabolic costs: two components, standard metabolic rate (SMR) and active swimming

### SMR: a*W^b (Claireaux et al. 2018; Healey et al. 2000; Enders et al.2003)

SMR <- function(X, a, b){  a*X^b }
curve(SMR(X, a=0.2, b=0.6), xlim=c(10, 50), ylab="SMR (mg O2*h-1)",
      xlab="Body weight (g)", main="Body weight influences SMR\n(Enders et al. 2003) juvenile sockeye",
      xname = "X")
      #a=0.227 and b=0.653 from Enders et al. 2003: juvenile sockeye btw 4-10 g at 15C.

SWIM.COST <- function(v, U) {exp(v*U)}
curve(SWIM.COST(U, v=0.02), xlim=c(0,50), ylab="Metabolic cost",
      xlab="U swimming speed (cm/s)", main="Swimming speed (U) influences metabolic cost",
      xname = "U")
abline(h=0, v=0, col = "mediumslateblue", lty="dashed")
abline(h=0, v=20, col = "mediumslateblue", lty="dashed")
abline(h=0, v=40, col = "mediumslateblue", lty="dashed")


### Ocean foraging gain: starts low, rapidly increases, but with tail that is more gradual
    # match pattern from (Satterthwaite et al. match-mismatch)

# Try beta distribution
e.vec <- seq(0, 7, by=0.01)
OCEAN.ENERGY.BETA <- dbeta(e.vec, shape1=2.2, shape2=3.5)
plot(OCEAN.ENERGY.BETA, ylab="ocean energy gained per day", xlim=c(0,60),
     xlab="time (days since start of simulation)")

# Try Gamma distribution - this is pretty darn good.
OCEAN.ENERGY.GAMMA <- dgamma(e.vec, shape = 3, rate = 5)
plot(OCEAN.ENERGY.GAMMA, ylab="ocean energy gained per day", xlim=c(0,60),
     xlab="time (days since start of simulation)")
# make 

# Try lognormal distribution



# Try normal distribution
timeseq <- seq(0,60, by=1)

OCEAN.ENERGY.GAUS <- dnorm(timeseq, mean = 30, sd = 10)
plot(OCEAN.ENERGY.GAUS, ylab="density",
     xlab="time (days since start of simulation)")
# would make mean stochastic...


# Let's try to find algebraic functions instead of distributions...not sure I understand the difference completely.
# See bell curve equations  here: https://en.wikipedia.org/wiki/Bell_shaped_function#:~:text=Gaussian%20function%2C%20the%20probability%20density,of%20the%20central%20limit%20theorem.

# Gaussian function - this seems promising!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OCEAN.ENERGY.GAUS2 <- function(X, a, b, c){  a*exp(-(X-b)^2/2*c^2) }
curve(OCEAN.ENERGY.GAUS2(X, a=2, b=40, c=0.08), xlim=c(0, 60), ylab="Daily energy gained by a salmon (g/day)",
      xlab="Time (days)", main="Ocean food over time",
      xname = "X")
curve(OCEAN.ENERGY.GAUS2(X, a=2, b=35, c=0.08), xlim=c(0, 60), ylab="Daily energy gained by a salmon (g/day)",
      xlab="Time (days)", main="Ocean food over time",
      xname = "X", add=TRUE, col="mediumslateblue")
abline(h=0.9, col="gray24", lty="dashed") #river 3.5% body weight for 26 g fish (YOLO max)


# ooo this is good! a parameter shits the y max, b parameter shifts the mean along the x-axis, c parameter shifts the width!!!
# would need to make parameter b stochastic!!

# another one to try...from (Archontoulis and Miguez 2015)
# OCEAN.ENERGY.BELL <- function(.....){  .... }
