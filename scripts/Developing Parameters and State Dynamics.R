 # Developing Parameters and State Dynamics
  ## background code in figuring out how to choose final model parameters and dynamics

library(ggplot2); library(investr); library(tolerance)

options(scipen=999)

#####################################################################################
# EQUATION NA - Xcrit is a function of individual prior maximum body size (X)
##  I need to estimate range of body weights (g) for a given fork length
##  I need to plot WT by FL, determine the Tolerance Interval
##  (NOT confidence interval or prediction interval) and then plot the predicted xmax against the xcrit


###  DECIDED I DO NOT NEED XCRIT TO BE A FUNCTION!


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
## EQUATION 3: ENERGY GAINED IN THE RIVER IS A FUNCTION OF CURRENT BODY SIZE

# make dataframe with points I know from the literature for final checks that models make sense
gday.dat <- data.frame(g.per.day = c(NA,NA, 0.07, 0.79, 1.47), 
                       p.wt.per.day = c(0.035, 0.028, NA, NA, NA), 
                       mean.X = c(2.1, 2.4, 6.8, 7.4, 117), 
                       t = c(5, 10, 30, 60, 90),
                       source = c("Henery: floodplain free-ranging chinook", 
                                  "Henery: Sac river free-ranging chinook",
                                  "MacFarlane: chinook estuary entry-exit",
                                  "MacFarlane: chinook estuary exit-ocean summer",
                                  "MacFarlane: chinook summer-fall ocean"))

gday.dat$g.per.day[1:2] <-gday.dat$mean.X[1:2] * gday.dat$p.wt.per.day[1:2] # estimate g/day for Henery from reported percent body weight and mean body weights.
    # *MacFarlane salmon weight entering the estuary was calculated from reported FL (82.5 mm FL) using our nls model above = 6.8
    #  These values are also heavily influenced by the environment aka food availability, BUT they give us a good check to see if we are in the right universe with our models.

plot(g.per.day ~ mean.X, gday.dat)
lines(g.per.day ~ mean.X, gday.dat)
  # okay, take away is that GROWTH should look like this, which INCLUDES metabolic costs. Perhaps need to
    # put all of Equation 1 together (or at least Equations 5 and 6 together) before checking to see if the shape is right.


# Trying different functions.
E.size <- function(X, a, b){  a-b/X }
curve(E.size(X, a=1.5, b=12), xlim=c(0, 150), ylim=c(0, 1.8), ylab="E (g/day)",
      xlab="Body weight (g)", main="Body weight influences Daily energy gained",
      xname = "X")
abline(h=1.5, col = "mediumslateblue", lty="dashed")
# a defines maximum value (1.5 g/d)

# linear function from (Handeland et al. 2008) relationships between Atlantic salmon smolt size and daily % growth rate.
E.size.Hand <- function(X, a, b){  b + a*X }
curve(E.size.Hand(X, a=0.0006, b=3), xlim=c(0, 200), ylab="E (% X/day)",
      xlab="Body weight (g)",
      xname = "X")
  # a (slope) is pretty consistent across temp treatments in (Handeland et al. 2008): 0.004, 0.004, 0.006, 0.007 (mean= 0.005!)
    # use 0.005 as the parameter in this model to account for size relationship to percent energy gained daily.
  # b (intercept) is going to be a function of shoreline habitat (and maybe temp if I add that in)

# now plot same function converted to grams/day
E.size.Hand.g <- function(X, a, b){  ((b + a*X)/100)*X }
curve(E.size.Hand.g(X, a=0.006, b=0.253), xlim=c(0, 200), ylab="E (g/day)",
      xlab="Body weight (g)",
      xname = "X")
  # Hmm but food/biology limits the amount an animal can put on a day?? right?
    # But the maximum here is our biggest fish will be putting on 3 g/day?? NO becuase habitat is more important!? 

# Try logistic curve - maybe gets too confusing?
E.size.log <- function(X, k, L, b){  L/(1+exp(-k*(X-b))) }
curve(E.size.log(X, k=0.018, L=1.5, b=150), xlim=c(0, 200), ylab="E (g/day)",
      xlab="Body weight (g)",
      xname = "X")

curve(E.size.log(X, k=0.05, L=3, b=150), xlim=c(0, 200), ylab="E (g/day)",
      xlab="Body weight (g)",
      xname = "X", add=TRUE, col="mediumslateblue")

# Okay stop over thinking and keep it simple based on the biology I want in my model

E.size.Hand <- function(X, a, b){  b + a*X }
curve(E.size.Hand(X, a=0.005, b=3.5), xlim=c(0, 200), ylim=c(0, 5), ylab="E (% X/day)",
      xlab="Body weight (g)", xname = "X", lwd=2, col="limegreen")

curve(E.size.Hand(X, a=0.005, b=1), xlim=c(0, 200), ylim=c(0, 5), ylab="E (% X/day)",
      xlab="Body weight (g)", xname = "X", lwd=2, col="mediumslateblue", add=TRUE)

# there is a linear relationship between salmon size (X) and percent body weight gained (Handeland et al. 2008)
  # at satiation there can be a difference in 0.5 %wt/day between a 100g and 200g salmon! (slope = 0.005) 
# maximum river growth in percent body weight is 3.5% at floodplains, salmon ~80 mm FL (~7 g) (intercept = 3.5) (Henery et al. 2010)

# Okay, but now does this make sense for actual g/day??
curve(E.size.Hand.g(X, a=0.005, b=3.5), xlim=c(0, 200), ylab="E (g/day)",
      xlab="Body weight (g)", xname = "X", lwd=2, col="limegreen")
curve(E.size.Hand.g(X, a=0.005, b=1), xlim=c(0, 200), ylab="E (g/day)",
      xlab="Body weight (g)", xname = "X", lwd=2, col="mediumslateblue", add=TRUE)
  # Hmmm, this seems like too high of grams per day?! MacFarlane observes only up to 1.5 or 2 grams/day in summer ocean salmon?!!!?!

# Okay, but now does this make sense for actual g/day??
curve(E.size.Hand.g(X, a=0.005, b=3.5), xlim=c(0, 200), ylab="E (g/day)",
      xlab="Body weight (g)", xname = "X", lwd=2, col="limegreen")
curve(E.size.Hand.g(X, a=0.005, b=1), xlim=c(0, 200), ylab="E (g/day)",
      xlab="Body weight (g)", xname = "X", lwd=2, col="mediumslateblue", add=TRUE)
  # Hmmm, this seems like too high of grams per day?! MacFarlane observes only up to 1.5 or 2 grams/day in summer ocean salmon?!!!?!


# Okay! Even though (Handeland et al. 2008) shows bigger salmon increasing their percent growth rate, this results in
  # exponential growth in g/day by salmon size, which is NOT biologically accurate. Many sources and growth curves
  # show that growth is asymptotic where mass-specific growth DECREASES as fish grow larger.


E.size.Satt2008 <- function(X, a, q){  q*X^a }
curve(E.size.Satt2008(X, a=0.86, q=0.05), xlim=c(0, 200), ylab="Anabolic gains (g/day)",
      xlab="Body weight (g)", xname = "X", lwd=2, col="limegreen")
  # What is q? what units? What does it represent biologically?


### Okay, try this. Energy intake is Eh (Energy per habitat g/day) times q (scaling parameter).
    # q is function of body size (X) following asympotic relationship: Either X^b or  a-b/X??

E.size.meg <- function(X, a, b){  a-b/X }
curve(E.size.meg(X, a=2, b=20), xlim=c(0, 150), ylim=c(0, 2), ylab="coefficient q (% of baseline Energy gained)",
      xlab="Body weight (g)", xname = "X")
abline(h=1.5, col = "mediumslateblue", lty="dashed")


# Try a FULL dX/dt equation because THAT is what should match my observed g/day growth.
Full.dxdt <- function(X, q, a, A, B, f, S, v, U){q*X^a - A*X^B*exp(f*S)*exp(v*U)}
curve(Full.dxdt(X, q=0.04, a=0.86, A=0.000525, B=1, f=0.071, S=12, v=0.027, U=20),
      ylab="g.per.day", xlab="Body weight (g): X", xname="X",
      xlim=c(0,200))
points(g.per.day ~ mean.X, gday.dat, pch=16)
# q values around 0.04 are somewhat near the data from the papers?!



#####################################################################################
## EQUATION 5: TOTAL METABOLIC COSTS

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



### add that parameter v is a function of shoreline habitat!

cost.Uh.FUN <- function(v, U) { exp(v*U) }

curve(cost.Uh.FUN(U, v=0.027),
      xname="U", ylab = "g/day", col="purple", xlim=c(0,40))

curve(cost.Uh.FUN(U, v=0.037),
      xname="U", ylab = "g/day", col="purple", lty="dashed", add=T)

curve(cost.Uh.FUN(U, v=0.017),
      xname="U", ylab = "g/day", col="purple", lty="dotted", add=T)

 # Notes: v changes the slope differently by speed. Increases costs for go fast, but NO change for move 0.
    # and I want the exact opposite. Energy benefit from going move 0 AT natural sites.

# Try changing alpha ONLY for when move 0 at altered sites.
costs.FUN <- function(A, v, U) { A*X*exp(v*U) }

curve(costs.FUN(U, v=0.017, A=0.00607),
      xname="U", ylab = "g/day", col="purple", xlim=c(0,40), ylim=c(0,0.13))

curve(costs.FUN(U, v=0.017, A=0.00407),
      xname="U", ylab = "g/day", col="purple", lty="dashed", add=T)


#####################################################################################
## EQUATIONS 3+4 TOGETHER: Total daily growth dX/dt

#a <- 0.86       # 0.86 (Satterthwaite et al. 2009 Evo Apps - Central Valley steelhead)
#A <- 0.000525   # Values used for coefficient 𝞪 include 0.00143 (g O2/d) (Beauchamp et al. 1989). We need values in grams (Carbon), so I converted 0.00143 g O2/day / 32 g [atomic weight of 2 O molecules] O2 per mole * 12 g C [atomic weight of C and definition of a mole] = 0.000525 g C/day.
#v <- 0.027      # Values used for swimming speed coefficient v include 0.0234 (s/cm) (Healey et al. 2000). We use speeds in km/day, so I converted 0.0234 s/cm * 100000 cm per km / 86400 s per day = 0.027 d/km.


GROWTH.FUN <- function(X, a, q, A, v, U) {X^a*q - A*X*exp(v*U) }

# Check to see if q changes shape aka if salmon are in different habitats does
  # it change their growth rates? Yes.

curve(GROWTH.FUN(X, a=0.86, A=0.00607, v=0.027, q=0.02, U=20),
      xname="X", ylab = "g/day", col="red", xlim=c(7,20), ylim=c(-0.1,0.6))

curve(GROWTH.FUN(X, a=0.86, A=0.00607, v=0.027, q=0.04, U=20),
      xname="X", ylab = "g/day", add=T, col="limegreen")

curve(GROWTH.FUN(X, a=0.86, A=0.00607, v=0.027, q=0.02, U=40),
      xname="X", ylab = "g/day", add=T, col="red", lty="dashed")

curve(GROWTH.FUN(X, a=0.86, A=0.00607, v=0.027, q=0.04, U=40),
      xname="X", ylab = "g/day", add=T, col="limegreen", lty="dashed")

curve(GROWTH.FUN(X, a=0.86, A=0.00607, v=0.027, q=0.02, U=0),
      xname="X", ylab = "g/day", add=T, col="red", lty="dotted")

curve(GROWTH.FUN(X, a=0.86, A=0.00407, v=0.027, q=0.04, U=0),
      xname="X", ylab = "g/day", add=T, col="limegreen", lty="dotted")




### Okay. Let's simulate some salmon growth in the river only.
  # use for loops!

sim.move0 <- data.frame(t=seq(1,30, by=1), X=rep(NA, 30), 
                        h=rep("a", 30), U=rep(0, 30))     # set baseline values.

sim.move0[1,2] <- 10 # set starting values for mass, 10g

GROWTH.FUN2 <- function(X, a, q, A, v, U) {X + X^a*q - A*X*exp(v*U) }


#for loop to simulate salmon mass over 30 days.
for(t in 1:29){
  sim.move0[t+1,2]<-GROWTH.FUN2(X = sim.move0[t,2], a=0.86, q=0.02, A=0.00607, 
                                v=0.027, U=sim.move0[t,4])
}

# salmon that moves 1 each day
sim.move20 <- data.frame(t=seq(1,30, by=1), X=rep(NA, 30), 
                        h=rep("a", 30), U=rep(20, 30))     # set baseline values.

sim.move20[1,2] <- 10 # set starting values for mass, 10g

#for loop to simulate salmon mass over 30 days.
for(t in 1:29){
  sim.move20[t+1,2]<-GROWTH.FUN2(X = sim.move20[t,2], a=0.86, q=0.02, A=0.00607, 
                                v=0.027, U=sim.move20[t,4])
}

# salmon that moves 2 each day
sim.move40 <- data.frame(t=seq(1,30, by=1), X=rep(NA, 30), 
                         h=rep("a", 30), U=rep(40, 30))     # set baseline values.

sim.move40[1,2] <- 10 # set starting values for mass, 10g

#for loop to simulate salmon mass over 30 days.
for(t in 1:29){
  sim.move40[t+1,2]<-GROWTH.FUN2(X = sim.move40[t,2], a=0.86, q=0.02, A=0.00607, 
                                 v=0.027, U=sim.move40[t,4])
}

# salmon that moves 0 each day in natural
sim.move0n <- data.frame(t=seq(1,30, by=1), X=rep(NA, 30), 
                         h=rep("n", 30), U=rep(0, 30))     # set baseline values.

sim.move0n[1,2] <- 10 # set starting values for mass, 10g

#for loop to simulate salmon mass over 30 days.
for(t in 1:29){
  sim.move0n[t+1,2]<-GROWTH.FUN2(X = sim.move0n[t,2], a=0.86, q=0.025, A=0.00607, 
                                 v=0.027, U=sim.move0n[t,4])
}

# salmon that moves 1 each day in natural
sim.move20n <- data.frame(t=seq(1,30, by=1), X=rep(NA, 30), 
                         h=rep("n", 30), U=rep(20, 30))     # set baseline values.

sim.move20n[1,2] <- 10 # set starting values for mass, 10g

#for loop to simulate salmon mass over 30 days.
for(t in 1:29){
  sim.move20n[t+1,2]<-GROWTH.FUN2(X = sim.move20n[t,2], a=0.86, q=0.025, A=0.00607, 
                                 v=0.027, U=sim.move20n[t,4])
}

# salmon that moves 2 each day in natural
sim.move40n <- data.frame(t=seq(1,30, by=1), X=rep(NA, 30), 
                          h=rep("n", 30), U=rep(40, 30))     # set baseline values.

sim.move40n[1,2] <- 10 # set starting values for mass, 10g

#for loop to simulate salmon mass over 30 days.
for(t in 1:29){
  sim.move40n[t+1,2]<-GROWTH.FUN2(X = sim.move40n[t,2], a=0.86, q=0.025, A=0.00607, 
                                  v=0.027, U=sim.move40n[t,4])
}

# compare plots!
plot(X~t, sim.move0, col="red", ylim=c(9,15)) # growth over time of a salmon that never moves.
points(X~t, sim.move20, col="orange") # growth over time of a salmon that moves 20km/day.
points(X~t, sim.move40, col="limegreen") # growth over time of a salmon that moves 40km/day.
points(X~t, sim.move0n, col="red", pch=16)
points(X~t, sim.move20n, col="orange", pch=16)
points(X~t, sim.move40n, col="limegreen", pch=16)


### THOUGHTS: Maybe alpha = 0.00607 (from Satterthwaite et al. 2010 Appendix A), with
    # q values around 0.02 are somewhat realistic?!
    # Success that different movement choices and river shoreline habitats result in different
      # growth trajectories.

### Now let's add q for the ocean.
OCEAN.Q <- function(t, a, b, c,d){  d+a*exp(-(t-b)^2/2*c^2) }
curve(OCEAN.Q(t, a=0.07, b=40, c=0.07, d=0.02), xlim=c(0, 60), ylab="q (ocean)",
      xlab="Time (days)", xname = "t")
abline(h=0.0225, col="gray24", lty="dashed") #river value


### Simulate a salmon in the ocean for 30 days
# salmon that moves 0 each day in ocean
sim.move0o <- data.frame(t=seq(20,49, by=1), X=rep(NA, 30), 
                          h=rep("o", 30), U=rep(0, 30))     # set baseline values.

sim.move0o[1,2] <- 12 # set starting values for mass, 10g

#for loop to simulate salmon mass over 30 days.
for(t in 1:29){
  sim.move0o[t+1,2]<-GROWTH.FUN2(X = sim.move0o[t,2], a=0.86, 
                                 q=OCEAN.Q(t=sim.move0o[t,1], a=0.07, b=40, c=0.07, d=0.02), A=0.00607, 
                                  v=0.027, U=sim.move0o[t,4])
}

#compare plots
plot(X~t, sim.move0o, col="slateblue", ylim=c(9,50), pch=16) # growth over time of a salmon in ocean days 1 to 30



# Let's simulate a salmon with different habitats
sim.mix <- data.frame(t=seq(1,60, by=1), X=rep(NA, 60), 
                         h=rep(NA, 60), U=rep(NA, 60))     # set baseline values.

sim.mix[1,2] <- 10 # set starting values for mass, 10g

sim.mix[31:60, 3] <- rep("o", 30)   # final 30 days in ocean
sim.mix[1:30, 3] <- sample(0:1, 30, replace=T, prob=c(0.5,0.5))  # first 30 days random between altered and natural
sim.mix$h[sim.mix$h == "1"] <- "a"   # change 1 from sample function to "a"
sim.mix$h[sim.mix$h == "0"] <- "n"   # change 0 from sample function to "n"

sim.mix[31:60, 4] <- rep(0, 30)   # last 30 days move 0 in ocean
sim.mix[1:30, 4] <- sample(0:2, 30, replace=T)  # first 30 days random sample between 0,1,2.
sim.mix$U[sim.mix$U == "1"] <- 20  # change 1 from sample function to "20" km/day
sim.mix$U[sim.mix$U == "2"] <- 40  # change 2 from sample function to "40" km/day


#for loop to simulate salmon mass over 60 days in a mix of habitats.
for(t in 1:59){
  sim.mix[t+1,2]<-GROWTH.FUN2(X = sim.mix[t,2], a=0.86, 
                                 q= ifelse(sim.mix[t,3] == "o", OCEAN.Q(t=sim.mix[t,1], a=0.07, b=40, c=0.07, d=0.02),
                                           ifelse(sim.mix[t,3] == "a", 0.02, 0.04)), 
                              A= ifelse(sim.mix[t,3] == "n" & sim.mix[t,4] == 0, 0.00407, 0.00607), 
                                 v=0.027, U=sim.mix[t,4])
}

#compare plots

#color by U
plot(X~t, sim.mix, col=as.factor(U), ylim=c(9,50), pch=16) # growth over time of a salmon for 30 days in river, and 30 days in ocean
points(mean.X~t, gday.dat)

plot(X~t, sim.mix, col=as.factor(U), ylim=c(9,15), xlim=c(0,30), pch=16) # growth over time of a salmon for 30 days in river, and 30 days in ocean

# color by h
plot(X~t, sim.mix, col=as.factor(h), ylim=c(9,50), pch=16) # growth over time of a salmon for 30 days in river, and 30 days in ocean
points(mean.X~t, gday.dat)

plot(X~t, sim.mix, col=as.factor(h), ylim=c(9,15), xlim=c(0,30), pch=16) # growth over time of a salmon for 30 days in river, and 30 days in ocean



#####################################################################################
## EQUATION 4: Ocean daily foraging gain

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


#####################################################################################
## EQUATION 6,7,9: RISK EQUATIONS

# Plot (Nobriga et al. 2021) equation for CAP (capture probability kind of like my Beta)
CAP <- function(X){  0.861-(1.82*X/236) }
curve(CAP(X), xname = "X", xlim=c(0,150))

# Plot (Abrahams & Mangel 2007) equation for weekly mortality rates
Mtrout <- function(X){ 0.01 + 0.03*X^-0.37 }
curve(Mtrout(X), xname="X", xlim=c(0,150))

# Plot (Satterthwaite et al. 2010) equation for length-dependent ocean survival
Satt.surv <- function(X){ 0.84*((exp(-8.657+0.0369*X)/(1+exp(-8.657+0.0369*X)))) }
curve(Satt.surv(X), xname="X", xlim=c(0,150))

# Plot (Mangel & Satterthwaite 2008) equation for mortality rate
Mangel.mort <- function(X){ 0.2 + 2/X }
curve(Mangel.mort(X), xname="X", xlim=c(0,150))


# My adjustments/combinations
Meg.mort <- function(X, mh, mu){ mh + mu + 0.001*X^-0.37 }
curve(Meg.mort(X, mh=0.0003, mu=0.0003), xname="X", xlim=c(0,20), ylim=c(0,0.1), ylab="daily mortality rate")
curve(Meg.mort(X, mh=0.02, mu=0.02), xname="X", ylab="daily mortality rate", add=T, col="slateblue")

