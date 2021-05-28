library(MASS)
data<-birthwt
attach(data)
is.factor(race)
race<-factor(race)

#creating the subsets
d1<-subset(data,data$race=="1")
d2<-subset(data,data$race=="2")
d3<-subset(data,data$race=="3")

r1<-lm(bwt~age,data=d1)
r2<-lm(bwt~age,data=d2)
r3<-lm(bwt~age,data=d3)

#plotting the subsets
plot(age,bwt, main="Birth Weight against Age by Race of Mother")
points(d2$age,d2$bwt, pch=2, col="red") 
points(d3$age,d3$bwt, pch=12, col="blue")
abline(r1,lty=1)
abline(r2,lty=2, col="red") 
abline(r3,lty=3, col="blue")
legend("topleft", c("White","Black","Other"), lty=c(1,2,3), pch=c(1,2,12), col=c("black","red","blue")) 

#creating the model with interaction
results<-lm(bwt~age*race)
summary(results)
