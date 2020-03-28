################################################################################
#                              STAT 480   
#                      Unit 6. Model Selection
#             Lecture 3. More on Ridge Regression and LASSO
################################################################################

################################################################################
## Example 1
################################################################################

n=50
p=30
set.seed(0)
x=matrix(rnorm(n*p),n,p)
beta=c(runif(10,0.5,1),runif(20,0,0.3))
hist(beta,breaks=seq(0,1,length=20))
y=x%*%beta+rnorm(n)

# Linear Regression
lm.mod=lm(y~x)
lm.coef=coef(lm.mod)

# Ridge Regression
library(glmnet)
grid=seq(5,0,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
ridge.coef.all=coef(ridge.mod)


# lambda selection by cv
ridge.cv=cv.glmnet(x,y,alpha=0)
bestlam=ridge.cv$lambda.min
plot(ridge.cv)
ridge.pred=predict(ridge.mod,s=bestlam,newx=x)
ridge.coef=predict(ridge.mod,type="coefficients",s=bestlam)

# Lasso Regression
library(glmnet)
grid=seq(5,0,length=100)
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef.all=coef(lasso.mod)


# lambda selection by cv
lasso.cv=cv.glmnet(x,y,alpha=1)
bestlam=lasso.cv$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x)
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam)


plot(rep(0,30),beta,xlab="", ylab="coefficients",col=2, xlim=c(0,2),ylim=c(-0.3,1.2))
points(rep(1,30),ridge.coef[-1],pch="*",col=3)
points(rep(2,30),lasso.coef[-1],pch="+",col=4)

coef.all=cbind(beta, ridge.coef[-1], lasso.coef[-1])
for(i in 1:30){
lines(c(0,1,2), coef.all[i,], lty=3)
}
text(0, 1.2, "True")
text(1, 1.2, "Ridge")
text(2, 1.2, "Lasso")

################################################################################
## Example 2
################################################################################

n=50
p=30
set.seed(0)
x=matrix(rnorm(n*p),n,p)
beta=c(runif(10,0.5,1),rep(0,20))
hist(beta,breaks=seq(0,1,length=20))
y=x%*%beta+rnorm(n)

# Linear Regression
lm.mod=lm(y~x)
lm.coef=coef(lm.mod)

# Ridge Regression
library(glmnet)
grid=seq(5,0,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
ridge.coef.all=coef(ridge.mod)


# lambda selection by cv
ridge.cv=cv.glmnet(x,y,alpha=0)
bestlam=ridge.cv$lambda.min
ridge.pred=predict(ridge.mod,s=bestlam,newx=x)
ridge.coef=predict(ridge.mod,type="coefficients",s=bestlam)

# Lasso Regression
library(glmnet)
grid=seq(5,0,length=100)
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef.all=coef(lasso.mod)


# lambda selection by cv
lasso.cv=cv.glmnet(x,y,alpha=1)
bestlam=lasso.cv$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x)
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam)


plot(rep(0,30),beta,xlab="", ylab="coefficients",col=2, xlim=c(0,2),ylim=c(-0.3,1.2))
points(rep(1,30),ridge.coef[-1],pch="*",col=3)
points(rep(2,30),lasso.coef[-1],pch="+",col=4)

coef.all=cbind(beta, ridge.coef[-1], lasso.coef[-1])
for(i in 1:30){
lines(c(0,1,2), coef.all[i,], lty=3)
}
text(0, 1.2, "True")
text(1, 1.2, "Ridge")
text(2, 1.2, "Lasso")

