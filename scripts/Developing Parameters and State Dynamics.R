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


# FIRST, tried more slamon groups, but then data distribution is all wonky.
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



# Plot weight by length with fitted equation
ggplot(data=bigfish, aes(x=Length, y=Weight_, color=StudyID)) + theme_classic() + 
  geom_point() + theme(legend.title = element_blank(), legend.position = "bottom") +
  stat_function(fun= function(x) 0.00001943926*x^2.89226591734, color="black", size = 1)
  # can tell that once I figure out how to estimate the tolerance interval, the interval
    # should increase with length (aka bigger salmon have a bigger gas tank)
  # I also think it is okay that late fall are on the bigger side (they are hatchery fish), while
    # steelhead are less heavy for their weight because they are wild. Maybe together they are a
    # good estimate of the real range of possibilities.


# STEP 4: Use function to estimate prediction intervals (not confidence intervals) to capture the range
    # of weights that a single length salmon can be. These bounds are estimates of Xcrit and Xmax in my model.)
# Some resources for calculating non-linear tolerance intervals
      # https://statisticsbyjim.com/hypothesis-testing/confidence-prediction-tolerance-intervals/
      # https://cran.r-project.org/web/packages/tolerance/tolerance.pdf
      # file:///C:/Users/megan/Downloads/v36i05%20(1).pdf (page 34)


# in package tolerance, function nlregtol.int: "provides 1-sided or 2-sided nonlinear
  #regression tolerance bounds." This is what I want! Now how to do it...

formula <- as.formula(y ~ a*x^b)
xy.data <- data.frame(cbind(bigfish$Weight_, bigfish$Length))
colnames(xy.data) <- c("y", "x")

out2 <- nlregtol.int(formula, xy.data = xy.data, x.new=cbind(c(100, 150)), 
                     side = 2, alpha = 0.05, P = 0.95, start = list(a=0.00002, b=3))
head(out2)
# ISSUES: Error if don't use x.new argument (Error in P.mat[i, ] : subscript out of bounds)
        #If use x.new = cbind(c(10, 20)), Why are tolerance bounds so huge?! Not right.
          # also do not understand what x.new is doing.
          # lots of warnings: not converged in 10000 iter. - maybe this is why I'm getting weird estimates!
            # Yes, directions say "It is highly recommended that the user specify starting values for the nls routine."
            # BUT, I do not know how/where to specify the starting values in nlregtol.int -- OKAY did this, but still same issue and warnings :()

plottol(out2, x = xy.data$x, y = xy.data$y, side = "two", x.lab = "X", y.lab = "Y")
#ISSUES: can't get this plot to make sense

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


# STEP 5: Plot the minimum prediction interval values (Xcrit) by the maximum prediction interval values (Xmax)
ggplot(data=pred.dat, aes(x=upr, y=lwr)) + theme_classic() + 
  geom_line(size=1) + xlab("upr.pred aka individual salmon prior Xmax") +
  ylab("lwr.pred aka Xcrit")
  # Cool! With the right prediction intervals, this is the shape/function for Xcrit (lwr.pred) in my model.
  # But this is linear??? Should not be...?


# STEP 6: Fit function to this relationship - use this function as Equation 3. Aka how once a salmon reaches an
    # Xmax value this will determine its Xcrit value in the model.

    #Based on plot in Step 5: pick formula for this equation: 



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
