################################################################################
#                              STAT 480   
#                      Unit 6. Model Selection
#               Lecture 2. Ridge Regression and LASSO
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
grid=seq(25,0,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
ridge.coef=coef(ridge.mod)
dim(ridge.coef)

xplot=c(rep(0,30),rep(1,30),rep(2,30))
yplot=c(beta,lm.coef[-1],ridge.coef[-1,51])
plot(xplot,yplot,ylab="coefficients")

# lambda selection by cv
ridge.cv=cv.glmnet(x,y,alpha=0)
plot(ridge.cv)
bestlam=ridge.cv$lambda.min
ridge.pred=predict(ridge.mod,s=bestlam,newx=x)
mean((ridge.pred-y)^2)

## Example 2
n=50
p=30
set.seed(0)
x=matrix(rnorm(n*p),n,p)
beta=runif(30,0.5,1)
hist(beta,breaks=seq(0,1,length=20))
y=x%*%beta+rnorm(n)

# Linear Regression
lm.mod=lm(y~x)
lm.coef=coef(lm.mod)

# Ridge Regression
library(glmnet)
grid=seq(25,0,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
ridge.coef=coef(ridge.mod)
dim(ridge.coef)

xplot=c(rep(0,30),rep(1,30),rep(2,30))
yplot=c(beta,lm.coef[-1],ridge.coef[-1,51])
plot(xplot,yplot,ylab="coefficients")

# lambda selection by cv
ridge.cv=cv.glmnet(x,y,alpha=0)
plot(ridge.cv)
bestlam=ridge.cv$lambda.min
ridge.pred=predict(ridge.mod,s=bestlam,newx=x)
mean((ridge.pred-y)^2)

################################################################################
## Example 3: The Lasso
################################################################################
library(glmnet)
n=50
p=30
x=matrix(rnorm(n*p),n,p)
beta=c(rep(1,10),rep(0,20))
y=x%*%beta+rnorm(n)

grid=seq(25,0,length=100)
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=coef(lasso.mod)
set.seed(1)
lasso.cv=cv.glmnet(x,y,alpha=1)
bestlam=lasso.cv$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x)
mean((lasso.pred-y)^2)
predict(lasso.mod,type="coefficients",s=bestlam)

aic=bic=rep(0,100)
for(i in 1:100){
	lam=grid[i]
	y.hat=predict(lasso.mod,s=lam,newx=x)
	beta.hat=predict(lasso.mod,type="coefficients",s=lam)
	p=sum(as.numeric(beta.hat)!=0)
	rss=sum((y-y.hat)^2)
	aic[i]=n*log(rss/n)+2*p
	bic[i]=n*log(rss/n)+log(n)*p
}
which.min(aic)
which.min(bic)