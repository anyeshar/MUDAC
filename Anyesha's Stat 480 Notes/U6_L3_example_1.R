################################################################################
#                              STAT 480   
#                      Unit 6. Model Selection
#                         Lecture 3. Hitters
################################################################################

library(ISLR)
#The package ISLR contains the data set
#There are some missing values here, so before we proceed we will remove them.
fix(Hitters)
names(Hitters)

Hitters=na.omit(Hitters)
with(Hitters,sum(is.na(Salary)))

################################################################################
#  Stepwise regression 
################################################################################

library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
regfit.back=regsubsets(Salary~.,Hitters,method="backward")
back.summary=summary(regfit.back)
back.summary$bic
which.min(back.summary$bic)
coef(regfit.back, id=8)


################################################################################
#  Ridge regression 
################################################################################

library(glmnet)
x=model.matrix(Salary~.,data=Hitters)[,-1]
y=Hitters$Salary

grid=10^(seq(10,-2,length=100))
fit.ridge=glmnet(x,y,alpha=0,lambda=grid)
plot(fit.ridge,xvar="lambda",label=TRUE)
dim(coef(fit.ridge))
coef(fit.ridge)

#Train/validation set
set.seed (1)
train=sample(1: nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid) 
ridge.pred=predict(ridge.mod,newx=x[test,],s=0)
mean((ridge.pred-y.test)^2)

#Ridge regression gives a whole path of lambda, we need to pick one.
#glmnet's got a built-in function, called CV.glmnet that will do cross validation

cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

ridge.pred=predict(ridge.mod,newx=x[test,],s=bestlam)
mean((ridge.pred-y.test)^2)

################################################################################
# Lasso
################################################################################

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod,xvar="lambda",label=TRUE)

set.seed (1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

lasso.pred=predict(lasso.mod,newx=x[test,],s=bestlam)
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef[lasso.coef!=0]



