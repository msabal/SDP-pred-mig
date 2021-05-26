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
