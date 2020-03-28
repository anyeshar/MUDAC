################################################################################
#                              STAT 480   
#                      Unit 6. Model Selection
#                           Subset Selection 
################################################################################

library(leaps)
n = 50 # sample size
p = 4 # data dimension
set.seed(2015)
x <- matrix(rnorm(n*p),ncol=p) # generate design matrix
y <- x[,1]+x[,2]+rnorm(n)*0.5 # true regression model

## forward selection
for1 <- regsubsets(x,y,method="forward")
for1.summary <- summary(for1)
names(for1.summary)
for1.summary$cp
plot(for1.summary$cp,xlab="Number of Variables", ylab="Cp",type="l")
which.min(for1.summary$cp)

## backward elimination
back1 <- regsubsets(x,y,method="backward")
back1.summary <- summary(back1)
back1.summary$bic
plot(back1.summary$bic,xlab="Number of Variables", ylab="bic",type="l")
which.min(back1.summary$bic)
coef(back1, id=1:4)

## exhaustive search
ex1 <- regsubsets(x,y,method="exhaustive")
ex1.summary <- summary(ex1)
ex1.summary$bic
plot(ex1.summary$bic,xlab="Number of Variables", ylab="bic",type="l")
which.min(ex1.summary$bic)
coef(ex1,id=1:4)

# four candidate models
m1 <- lm(y~x[,1])
m2 <- lm(y~x[,1]+x[,2])
m3 <- lm(y~x[,1]+x[,2]+x[,4])
m4 <- lm(y~x)

# compute RSS for the four models
rss <- rep(0,4)
rss[1] <- sum((y-predict(m1))^2)
rss[2] <- sum((y-predict(m2))^2)
rss[3] <- sum((y-predict(m3))^2)
rss[4] <- sum((y-predict(m4))^2)
AIC(m1)
BIC(m1)
# compute AIC and BIC
bic <- rep(0,4)
aic <- rep(0,4)
for (i in 1:4){
bic[i]=n+n*log(2*pi)+n*log(rss[i]/n)+log(n)*(1+i)
aic[i]=n+n*log(2*pi)+n*log(rss[i]/n)+2*(1+i)
}
# find the optimal model
which.min(bic)
which.min(aic)