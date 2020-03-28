################################################################################
#                              STAT 480            
#                         Unit 3 ---  Lecture 3
#                       Example. LifeCycleSavings 
################################################################################

# Data Description
LifeCycleSavings
# Basic Scatterplot Matrix
pairs(LifeCycleSavings, main="LifeCycleSavings data")
round(cor(LifeCycleSavings),3)
# Fit the model and examine the output
m=lm(sr~pop15+pop75+dpi+ddpi,data=LifeCycleSavings)
summary(m)
n=nrow(LifeCycleSavings)

# Plot of Residuals
plot(m$res,ylab="Residuals",main="Index Plot of Residuals", pch=20)
countries=row.names(LifeCycleSavings)
identify(1:n,m$res,countries)

# Plot of Leverages
x = model.matrix(m)
lev = hat(x)
plot(lev,ylab="Leverages", main = "Index plot of Leverages", pch=20)
abline(h=2*5/50)
identify(1:50,lev,countries)

# Plot of Studentized Residuals
ms = summary(m)
stud = m$res/(ms$sig*sqrt(1-lev))
plot(stud,ylab="Studentized Residuals",main="Studentized Residuals",pch=20)
identify(1:50,stud,countries)

# Plot of Jackknife Residuals
jack = rstudent(m)
plot(jack,ylab="Jackknife Residuals", main="Jackknife Residuals",pch=20)
identify(1:50,jack,countries)

# Plot of Cooks Distance
cook=cooks.distance(m)
plot(cook,ylab="Cook Distance",main="Cooks Distance",pch=20)
segments(1:50,0,1:50,cook)
identify(1:50,cook,countries)

# QQ Plot
qqnorm(m$res,ylab="Residuals",pch=20)
qqline(m$res)

