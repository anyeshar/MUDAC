################################################################################
#                              STAT 480            
#                         Unit 3 --- Lecture 2
#                      Example. LifeCycleSavings 
################################################################################

### Data Description
LifeCycleSavings
names(LifeCycleSavings)

# Basic Scatterplot Matrix
plot(LifeCycleSavings)
pairs(LifeCycleSavings, main="LifeCycleSavings data")
round(cor(LifeCycleSavings),3)

# Fit the model and examine the output
m=lm(sr~pop15+pop75+dpi+ddpi,data=LifeCycleSavings)
summary(m)

### Obtain the regression using formula
attach(LifeCycleSavings)
X=cbind(1,pop15,pop75,dpi,ddpi)
Y=sr
beta.hat=solve(t(X)%*%X)%*%t(X)%*%Y
H=X%*%solve(t(X)%*%X)%*%t(X)
Y.hat=H%*%Y

