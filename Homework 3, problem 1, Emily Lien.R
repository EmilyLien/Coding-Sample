#Question 1, part a:
library(leaps)
attach(swiss)
swiss
allreg <- regsubsets(Fertility ~., data=swiss, nbest=5)

best <- as.data.frame(summary(allreg)$outmat)
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(data)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic
best

best[order(best$adjr2),]
best[order(best$mse),]
best[order(best$cp),]
best[order(best$bic),]

#Part b: Choosing a model
full<-lm(Fertility~Agriculture+Education+Catholic+Infant.Mortality+Examination)
reduced<-lm(Fertility~Agriculture+Education+Catholic+Infant.Mortality)
summary(full) #Examination is not significant in the presence of other regressors
anova(reduced,full) #Fail to reject the null, therefore we may drop Examination from the model
qt(.975,41) #Double-checking that we fail to reject the null. The t-value is less than the critical value, so we're good

#Part c: Checking for outliers
res<-reduced$residuals ##residuals

student.res<-rstandard(reduced) ##studentized residuals

ext.student.res<-rstudent(reduced) ##externally studentized residuals
par(mfrow=c(1,3))
plot(reduced$fitted.values,res,main="Residuals")
plot(reduced$fitted.values,student.res,main="Studentized Residuals")
plot(reduced$fitted.values,ext.student.res,main="Externally  Studentized Residuals")
#The plots appear to be generally the same shape along the different scales

n<-length(Fertility)
p<-5 #number of parameters: one intercept + three predictors

##critical value using Bonferroni procedure
qt(1-0.05/(2*n), n-p-1)

ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)] #returns null, confirms that there are no response outliers

#Part d: Checking for Leverage 
lev<-lm.influence(reduced)$hat

2*p/n

lev[lev>2*p/n]
#indexes 19 and 45, which correspond to La Vallee and V. De Geneve

#Part e: Influential observations with DFFITS
DFFITS<-dffits(reduced)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]

#Part f: Influential observations with Cooks Distance
COOKS<-cooks.distance(reduced)
COOKS[COOKS>qf(0.5,p,n-p)]
#this indicates there are no outliers

#Part h: Backwards selection

##intercept only model
regnull <- lm(Fertility~1, data=swiss)
##model with all predictors
regfull <- lm(Fertility~., data=swiss)

#backward selection:
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
