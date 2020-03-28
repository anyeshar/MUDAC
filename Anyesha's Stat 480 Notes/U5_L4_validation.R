##########################################################
# STAT 480
# Unit 5. Resampling Methods
# Lecture 4. Cross Validation
##########################################################

Auto=read.table("auto.txt",head=TRUE)
attach(Auto)

##########################################################
# 1. The Validation Set Approach

#Set a seed for so that everyone will obtain same results 
set.seed(1)

#Select a random subset of 196 obs out of 392 obs
train=sample(392,196)

#Use the subset option to fit a lm using only the training set
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)

#Use the mean() function to calculate the MSE  the validation set
mean((mpg-predict(lm.fit,Auto))[-train]^2)

#Use the poly() function to fit a quadratic regression
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#Use the poly() function to fit a cubic regressions
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
#Calculate the MSE of the cubic regression 
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


#Now try a different training set instead
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

##########################################################
# 2. k-Fold Cross-Validation
library(boot)
set.seed(3)

glm.fit=glm(mpg~horsepower,data=Auto)
cv.error=cv.glm(Auto,glm.fit,K=5)$delta[1]
cv.error
	
cv.error=rep(0,5)
for (i in 1:5){
	glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
	cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error.5
plot(cv.error.5,type="b",xlab="polynomial")

cv.error.10=rep(0,10)
for (i in 1:10){
	glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
	cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
plot(cv.error.10,type="b",xlab="polynomial")

##########################################################
# Leave-One-Out Cross-Validation
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)

#the delta vector contain the cross-validation results
cv.err$delta[1]






