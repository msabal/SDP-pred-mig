 # Developing Parameters and State Dynamics
  ## background code in figuring out how to choose final model parameters and dynamics

library(ggplot2); library(investr); library(tolerance)

options(scipen=999)

#####################################################################################
# EQUATION 3 - Xcrit is a function of individual prior maximum body size (X)
##  I need to estimate range of body weights (g) for a given fork length
##  I need to plot WT by FL, determine the Tolerance Interval
##  (NOT confidence interval or prediction interval) and then plot the predicted xmax against the xcrit

# EQUATION 3:
# Steps I think I need to do to figure out Eq. 3
  # 1: Get dataset with migrating salmon length-weight relationships
  # 2: Plot Weight by Length
  # 3: Fit function to the data (non-linear)
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


# FIRST, tried more salmon groups, but then data distribution is all wonky.
  samdata$PopGroup <- substr(samdata$StudyID, 1, 7) #Make new column PopGroup with only first 7 characters from StudyID, I picked 7 so that I get "Coleman" and "MillCk_" by themselves.

  coleman <- subset(samdata, PopGroup == "Coleman") #Subset Coleman fish from all years.
  millcreek <- subset(samdata, PopGroup == "MillCk_") #Subset Mill_Ck fish from all years.
  col.mill <- rbind(coleman, millcreek) # and here is them together.

#SECOND, tried just latefall and steelhead (that is what the observation are based on too!)
latefall <- samdata[grep("LateFall", samdata$StudyID), ]
sh <-  samdata[grep("SH", samdata$StudyID), ]
bigfish <- rbind(latefall, sh)

bigfish <-subset(bigfish, Length < 300)  #drop two really big steelhead tagged (> 300 mm FL)

hist(bigfish$Length) #normal data distribution!


# Now plot only the relevant data
plot(Weight_ ~ Length, data=bigfish) # full relevant dataset

summary(bigfish$Length)
summary(bigfish$Weight_) # could use this to figure out what range of mass (X) we want to run the model for (aka create the decision matrix for)

#one fish really seems to be an outlier: potentially error in weight. Let's try and remove it and see if it helps model predictions later. Doesn't help later (but still probably should be dropped.)
bigfish <- bigfish[!(bigfish[,17] > 125 & bigfish[,17] < 140 & bigfish[,18] > 40),] # drop the individual fish (MC2015-045)

# STEP 3: Fit function to the data (non-linear).
  # Do I need to use the function nls? Formula from literature: W=a*L^b

# Try nls model with form W=a*L^b
lt.wt.mod <- nls(Weight_ ~ a*Length^b, data = bigfish, start = list(a=0.005, b=2))
summary(lt.wt.mod) #fitted function is highly significant
coef(lt.wt.mod) # a=0.00001943926, b=2.89226591734 fitted parameter values

####
# Try nls model with form W=a*L^b but with weighted analysis because my data is has heteroscedasticity (variation in Weights changes across Lengths)

# make dataset of only response and predictor variables (Length, Weight_)
mod.dat <- bigfish[, c("Weight_", "Length")]

# make column for values for weighting
mod.dat$w <- 1/mod.dat$Weight_  # try 1/y for now, just to see if it makes a difference in tolerance intervals later.

mod.dat$resid <- residuals(lt.wt.mod) # get residuals from nls object...could these residuals be used as weights??
mod.dat$resid <- abs(mod.dat$resid)  #weights can't be negative, so take absolute value.
attr(mod.dat$resid, "Residuals") <- NULL # Why isn't this working???


# How to weight values? Can't calculate variance by sub-group (e.g., treatment) because all continuous variables.
  # Here (https://www.r-bloggers.com/2012/07/a-weighting-function-for-nls-nlslm/) gives common examples like
    # Weighting by inverse of response 1/y or Weighting by square root of predictor \sqrt{x}, etc...how how do I choose among these?  
      # it also gives R code for a function wfct() that returns a vector of weights that are calculated from a user-defined expression and transfers this vector within nls.


# Try nls model with weights
lt.wt.modw <- nls(Weight_ ~ a*Length^b, data = mod.dat, start = list(a=0.005, b=2), weights = mod.dat$resid)
summary(lt.wt.modw) #fitted function is highly significant
coef(lt.wt.modw) # 0.00005562708, b=2.69175758515 slightly different fitted parameter values

####

# Plot weight by length with fitted equation
ggplot(data=bigfish, aes(x=Length, y=Weight_, color=StudyID)) + theme_classic() + 
  geom_point() + theme(legend.title = element_blank(), legend.position = "bottom") +
  stat_function(fun= function(x) 0.00001943926*x^2.89226591734, color="black", size = 1) +
  stat_function(fun= function(x) 0.00005562708*x^2.69175758515, color="blue", size = 1)
  # can tell that once I figure out how to estimate the tolerance interval, the interval
    # should increase with length (aka bigger salmon have a bigger gas tank)
  # I also think it is okay that late fall are on the bigger side (they are hatchery fish), while
    # steelhead are less heavy for their weight because they are wild. Maybe together they are a
    # good estimate of the real range of possibilities.

    # blue line is weighted nls, black line is non-weighted nls


# STEP 4: Use function to estimate prediction intervals (not confidence intervals) to capture the range
    # of weights that a single length salmon can be. These bounds are estimates of Xcrit and Xmax in my model.)
# Some resources for calculating non-linear tolerance intervals
      # https://statisticsbyjim.com/hypothesis-testing/confidence-prediction-tolerance-intervals/
      # https://cran.r-project.org/web/packages/tolerance/tolerance.pdf
      # file:///C:/Users/megan/Downloads/v36i05%20(1).pdf (page 34)


# in package tolerance, function nlregtol.int: "provides 1-sided or 2-sided nonlinear
  #regression tolerance bounds." This is what I want! Now how to do it...


x <- bigfish$Length
y <- bigfish$Weight_

formula <- as.formula(y ~ a*x^b)
xy.data <- data.frame(cbind(bigfish$Weight_, bigfish$Length))
colnames(xy.data) <- c("y", "x")


out2 <- nlregtol.int(formula, xy.data = xy.data, x.new=cbind(c(100, 150)), 
                     side = 2, alpha = 0.05, P = 0.95, start = list(a=0.00002, b=3))

#try with weights
out2w <- nlregtol.int(formula, xy.data = xy.data, x.new=cbind(c(100, 150)), 
                     side = 2, alpha = 0.05, P = 0.95, start = list(a=0.00002, b=3), weights=mod.dat$resid)
# ERROR trying to pass weights to nls inside nlregtol.int...


head(out2)
#nlregtol.int returns a data frame with items:
#  alpha The specified significance level.
#P The proportion of the population covered by the tolerance bound(s).
#y.hat The predicted value of the response for the fitted nonlinear regression model.
#y The value of the response given in the first column of xy.data. This data frame
#is sorted by this value.
#1-sided.lower The 1-sided lower tolerance bound. This is given only if side = 1.
#1-sided.upper The 1-sided upper tolerance bound. This is given only if side = 1.
#2-sided.lower The 2-sided lower tolerance bound. This is given only if side = 2.
#2-sided.upper The 2-sided upper tolerance bound. This is given only if side = 2.
      
plottol(out2, x = xy.data$x, y = xy.data$y, side = "two", x.lab = "Length", y.lab = "Weight", col="lightskyblue")
# Looks good! However, tolerance intervals are even across salmon Length even though it should increase.
  # I think this is because it uses an ordinary least squares analysis and I really want
  # the tolerance intervals derived from an estimated weighted least squares analysis -
  # I got this from this paper (https://www.hindawi.com/journals/jqre/2009/126283/#sec1.2)
  # See Figure 1 and the description below it. They plot both types of tolerance intervals
  # and it is obvious that one is even and the other changes width.
  # HOWEVER, I can't figure out how to do a weighted analysis in the R tolerance package...
  # OOO, nls has an option to add weights=... ! Explore what this is. Can pass nls stuff into the tolerance functions.
    # go up and try above with nls! (https://www.r-bloggers.com/2014/01/ill-take-my-nls-with-weights-please/)

# Make tolerance dataset
tol.dat <- out2
colnames(tol.dat) <- c("alpha", "P", "pred.Wt", "obs.Wt", "lower.tol", "upper.tol")

### When I tried STEP4 with prediction intervals...

# Some resources for calculating non-linear prediction intervals
    # https://www.r-bloggers.com/2018/05/monte-carlo-based-prediction-intervals-for-nonlinear-regression/
    # http://sia.webpopix.org/nonlinearRegression.html#confidence-intervals-and-prediction-intervals
    # https://stackoverflow.com/questions/52973626/how-to-calculate-95-prediction-interval-from-nls  (This one I could follow below!)

#create new dataframe to make predictions
newdata <- data.frame(Length=seq(70, 230, by=1))

pred.dat <- predFit(lt.wt.mod, newdata= newdata, interval="prediction", level=0.99)
      # "Confidence/prediction bands for nonlinear regression (i.e., objects of class ‘nls’) are based on a linear approximation
      # as described in Bates & Watts (2007). This fun[c]tion was in[s]pired by the ‘plotfit’ function from the ‘nlstools’ package.
      # also known as the Delta method (e.g. see emdbook::deltavar)."

      # ISSUES: prediction intervals are consistent across salmon Lengths although they should get larger.

#Put predicted values and estimates in a dataframe: pred.dat
pred.dat <- as.data.frame(pred.dat)               #make a dataframe
pred.dat <- cbind(newdata, pred.dat)              #add new data column (Length values) used for the predictions

# Add prediction intervals to ggplot
ggplot() + theme_classic() + 
  geom_point(data=col.mill, aes(x=Length, y=Weight_, color=StudyID)) + theme(legend.title = element_blank(), legend.position = "bottom") +
  stat_function(fun= function(x) 0.00001387173*x^2.95846603870, color="black", size = 1) +
  geom_ribbon(data=pred.dat, aes(x=Length, ymin=lwr, ymax=upr), alpha=0.1, fill="blue")


# STEP 5: Plot the minimum tolerance interval values (Xcrit) by the maximum tolerance interval values (Xmax)
ggplot(data=tol.dat, aes(x=upper.tol, y=lower.tol)) + theme_classic() + 
  geom_point(size=1.6) + xlab("upr.pred aka individual salmon prior Xmax") +
  ylab("lwr.pred aka Xcrit") +
  stat_function(fun= function(x) 0.9986592*x - 17.6462223, color="blue", size = 1)
  # Cool! But I think I need to figure out how to make weighted tolerance intervals


# STEP 6: Fit function to this relationship - use this function as Equation 3. Aka how once a salmon reaches an
    # Xmax value this will determine its Xcrit value in the model.

    #Based on plot in Step 5: pick formula for this equation: linear, y = mx+b

summary(lm(lower.tol ~ upper.tol, tol.dat))
# intercept = -17.6462223, slope = 0.9986592
# BUT this will run into trouble for small salmon. and it is not biologically right. Sigh, will need to figure out the weighted nls/tolerance intervals.


#####################################################################################
## EQUATION 4: ENERGY GAINED IN THE RIVER IS A FUNCTION OF CURRENT BODY SIZE

simfish4 <- seq(15, 180, by=2)
ehab_per <- seq(0, .08, by=0.02)

Ehab_g <- simfish4*0.08 # 8%

plot(simfish4, Ehab_g)

# Can a 180 g salmon gain 14 grams per day??? NO.
    # (MacFarlane 2020) salmon max gain in the ocean in the summer is 1.5 g/day
      # so function asymptotes to 1.5 g/day

E.size <- function(X, a, b){  a-b/X }
curve(E.size(X, a=1.5, b=12), xlim=c(0, 150), ylim=c(0, 1.8), ylab="E (g/day)",
      xlab="Body weight (g)", main="Body weight influences Daily energy gained",
      xname = "X")
abline(h=1.5, col = "mediumslateblue", lty="dashed")
# a defines maximum value (1.5 g/d)




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
