################################################################################
#                              STAT 480            
#                         Unit 4 ---  Lecture 1
################################################################################

################################################################################
# Example 1
################################################################################
n=100
par(mfrow=c(1,3))
x1=c(runif((n/2),0,0.4),runif((n/2),0.6,1.0))
y1=c(rep(0,n/2),rep(1,n/2))
m1=lm(y1~x1)
summary(m1)
plot(x1,y1,xlab="x",ylab="y",main="0 mistakes")
abline(m1,col=1)
abline(v=(0.5-m1$coefficient[1])/m1$coefficient[2],col=2)
sum((m1$fitted>0.5)!=y1)

x2=c(runif((n/2),0,0.6),runif((n/2),0.5,1.0))
y2=c(rep(0,n/2),rep(1,n/2))
m2=lm(y2~x2)
summary(m2)
plot(x2,y2,xlab="x",ylab="y",main="7 mistakes")
abline(m2,col=1)
abline(v=(0.5-m2$coefficient[1])/m2$coefficient[2],col=2)
sum((m2$fitted>0.5)!=y2)

x3=c(runif(33,0,0.5),runif(34,0.5,1.0),runif(33,1.0,1.5))
y3=c(rep(0,33),rep(1,34),rep(0,33))
m3=lm(y3~x3)
summary(m3)
plot(x3,y3,xlab="x",ylab="y",main="34 mistakes")
abline(m3,col=1)
abline(v=((0.5-m3$coefficient[1])/m3$coefficient[2]),col=2)
sum((m3$fitted>0.5)!=y3)


################################################################################
## Example: Default dataset 
################################################################################

library("ISLR")
names(Default)
summary(Default)

attach(Default)

plot(balance,income,col="white",xlab="Balance",ylab="Income")
points(balance[default=="No"],income[default=="No"],
       col="blue",pch="N")
points(balance[default=="Yes"],income[default=="Yes"],
       col="red",pch="Y")

###logistic regression
fit1=glm(default~balance,family=binomial,data=Default)
summary(fit1)

newdata1=data.frame(balance=c(1000,2000))
predict(fit1,newdata1,type="response")

###logistic regression
fit2=glm(default~balance+income,family=binomial,data=Default)
summary(fit2)

newdata2=data.frame(balance=1500,income=4000)
predict(fit2,newdata2,type="response")


