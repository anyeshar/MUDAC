################################################################################
#                              STAT 480            
#                         Unit 5 ---  Lecture 1
#                         Bootstrap: Correlation 
################################################################################

# First, enter the data:
law=matrix(0,15,2)
law[,1]=c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
law[,2]=c(3.39,3.30,2.81,3.03,3.44,3.07,3,3.43,3.36,3.13,3.12,2.74,2.76,2.88,2.96)

# Next, calculate the usual pearson's correlation
cor(law[,1],law[,2])

# Take B=1000 bootstrap samples
B=1000

# Initialize the vector in which we will store the bootstrap results
law.boot=rep(0,B)
for(i in 1:B){
	sam=sample(1:15, replace=T)
	law.boot[i]=cor(law[sam,1],law[sam,2])
}

# Find the histogram of all the correlation coefficients of the 1000 samples
hist(law.boot)

# Calculate the bootstrap estimate of the standard deviation of the estimate
sqrt(var(law.boot))

# 95% confidence interval
rho_low=quantile(law.boot, 0.025)
rho_up=quantile(law.boot, 0.975)
