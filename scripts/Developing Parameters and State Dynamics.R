 # Developing Parameters and State Dynamics
  ## background code in figuring out how to choose final model parameters and dynamics

library(ggplot2)

#####################################################################################
# EQUATION 3 - Xcrit is a function of individual prior maximum body size (X)
##  I need to estimate range of body weights (g) for a given fork length
##  I need to plot WT by FL, determine the 95% prediction interval
##  (NOT confidence interval) and then plot the predicted xmax against the xcrit

# EQUATION 3:
# Steps I think we need to do to figure out Eq. 3
  # 1: Get dataset with migrating salmon length-weight relationships
  # 2: Plot Weight by Length
  # 3: Fit function to the data (exponential, non-linear)
  # 4: Use function to estimate prediction intervals (not confidence intervals) to capture the range
      # of weights that a single length salmon can be. These bounds are estimates of Xcrit and Xmax in my model.
  # 5: Plot the minimum prediction interval values (Xcrit) by the maximum prediction interval values (Xmax)
  # 6: Fit function to this relationship - use this function as Equation 3. Aka how once a salmon reaches an
      # Xmax value this will determine its Xcrit value in the model.


# STEPS 1 & 2: Get dataset with migrating salmon length-weight relationships and plot the data.
  # I have this dataset from Arnold that includes Release data (including weight and lengths) of ALL
      # acoustically tagged fish (ALL species). I put this file in this SDP-pred-mig repository in the
      # raw-data folder called "qry_ReleaseInfo".

#Import salmon tagging release data (including weight and lengths)
samdata<-read.csv("C:\\Users\\megan\\Google Drive\\Professional\\GIT Repositories\\SDP-pred-mig\\raw-data\\qry_ReleaseInfo.txt", header=T, sep=",")

plot(Weight_ ~ Length, data=samdata) #ALL species (including other than salmon) and ALL sizes

#Subset data to what is relevant to our model: want fall-run, late-fall-run, spring-run, steelhead of outmigration sizes.
  # To subset, make new column called "PopGroup" with only the first 7 characters from StudyID, I picked 7 so that I get  
  # "Coleman" and "MillCk_" by themselves across all years.
  # Coleman includes fall-run and late-fall, all of hatchery origin.
  # MillCk_ includes spring-run and steelhead of wild origin.
  # Both Coleman and Mill Creek fish are from/released near the Upper Sacramento River (similar to how our model is set up)

samdata$PopGroup <- substr(samdata$StudyID, 1, 7) #Make new column PopGroup with only first 7 characters from StudyID, I picked 7 so that I get "Coleman" and "MillCk_" by themselves.

coleman <- subset(samdata, PopGroup == "Coleman") #Subset Coleman fish from all years.
millcreek <- subset(samdata, PopGroup == "MillCk_") #Subset Mill_Ck fish from all years.
col.mill <- rbind(coleman, millcreek) # and here is them together.
rm(coleman); rm(millcreek) #remove temp objects

# Now plot only the relevant data
plot(Weight_ ~ Length, data=col.mill) # full relevant dataset
plot(Weight_ ~ Length, data=subset(col.mill, Length > 150)) # to play around with looking at different subsets

summary(col.mill$Length)
summary(col.mill$Weight_) # could use this to figure out what range of mass (X) we want to run the model for (aka create the decision matrix for)

# STEP 3: Fit function to the data (exponential, non-linear)?
  # Do I need to use the function nls? To what formula?

# Try nls model with form Weight = e^b*Length - What is the right formula to use?
lt.wt.mod <- nls(Weight_ ~ exp(b*Length), data = col.mill, start = list(b=0.2))
summary(lt.wt.mod)
coef(lt.wt.mod) # b=0.02274157 fitted parameter value

#Plot fitted function on top of raw data
lt.wt.fun <- function(Length, b){  exp(Length*b) }
curve(lt.wt.fun(Length, b=0.023), xlim=c(50, 260), ylab="Weight",
      xlab="Length", xname = "Length",col="blue")
points(Weight_ ~ Length, data=col.mill, col="gray50") # Hmm is this the best we can do?


# STEP 4: Use function to estimate prediction intervals (not confidence intervals) to capture the range
    # of weights that a single length salmon can be. These bounds are estimates of Xcrit and Xmax in my model.r)

# Some resources for calculating non-linear prediction intervals
    # https://www.r-bloggers.com/2018/05/monte-carlo-based-prediction-intervals-for-nonlinear-regression/
    # http://sia.webpopix.org/nonlinearRegression.html#confidence-intervals-and-prediction-intervals





#####################################################################################
## EQUATION 6: TOTAL METABOLIC COSTS

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

#####################################################################################
## EQUATION 5: Ocean daily foraging gain

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
