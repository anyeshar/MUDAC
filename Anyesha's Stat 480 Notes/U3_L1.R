################################################################################
#                              STAT 480            
#                         Unit 3 ---  Lecture 1
################################################################################


################################################################################
## Example: cars dataset  
################################################################################

names(cars)
### scatter plot
plot(cars$speed,cars$dist,xlab="speed",ylab="distance")
n=nrow(cars)
### linear regression
m1=lm(dist~speed,data=cars)
summary(m1)

### scatter plot with regression line
plot(cars$speed,cars$dist,xlab="speed",ylab="distance")
abline(m1,col=4)

s2=sum((cars$dist-m1$fitted)^2)/(n-2)
sqrt(s2)
c(3.9324-2*0.4155,3.9324+2*0.4155)

### overall model fit
anova(m1)

### confidence interval at speed=22
newdata1=data.frame(speed=22) 
predict(m1,newdata1,interval="confidence")


### prediction interval at speed=22
newdata1=data.frame(speed=22) 
predict(m1,newdata1,interval="prediction")