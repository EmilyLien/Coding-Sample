#STAT 5170, Time Series Analysis
#Homework 2
#Emily Lien

#Question 1

berk<-scan("berkeley.dat", what=list(double(0),double(0),double(0)))
time<-berk[[1]]
berkeley<-berk[[2]]

#Part a: Plotting Berkeley vs. Time
plot(berkeley~time, main="Berkeley vs. Time")

#There appears to be a vaguely positive linear trend

#Part b: Regress Berkeley against Time
linfit <- lm(berkeley~time)
anova(linfit)
summary(linfit)
AIC(linfit)
acf(linfit)

#Examining residuals
plot(linfit$fitted.values,linfit$residuals, main='Residual plot of Berkeley against Time')
abline(h=0, col='blue')

#Alternate plotting method
plot(linfit) 
#This gives me an equivalent residual plot, and is the plot included in the document. Also produced the QQ plot

#Part c: Time series plot and ACF plot
plot.ts(berkeley)
acf(berkeley)

#Part d: Differencing the data and trying again

difBerk <- diff(berkeley)
plot.ts(difBerk)
acf(difBerk)

#Question 3

engtemp<-scan("tpmon.dat",skip=1)

#Part a: Plot the first 300 data points, create an ACF plot
engtemp2<-engtemp[1:300]
plot.ts(engtemp2)
acf(engtemp2)

#Part b: Fit all points to a specified model
t<- (1:length(engtemp))

englm <- lm(engtemp ~ cos(2*pi*t/12) + sin(2*pi*t/12))
summary(englm)

#Part c: Plot the first 300 residuals
res <-englm$residuals[1:300]
fit <- englm$fitted.values[1:300]

plot(fit,res)
abline(h=0, col="blue")

acf(res)

#Question 4
library(astsa)
oildat<- oil

#Part a: Apply a moving average smoother and state the weights used
#Use k=51 on a one-sided average to detrend the weekly data for observing an annual pattern
oilMA<-filter(oildat,sides=1,c(0.5, rep(1,51), 0.5)/52)

#Part b: Plot the data with the moving average overlaid

plot(oildat)
lines(oilMA, col='blue')

#Question 5

#Part a: Check for parameter redundancy
phiz<-c(1,-9/4,-9/4)
polyroot(phiz)

thetaz <- c(1,-3,1/9,-1/3)
polyroot(thetaz)

