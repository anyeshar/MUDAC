
##########################################################
# STAT 480
# Unit 5. Resampling Methods
# Jackknife: Correlation 
##########################################################

# First, enter the data:
law=matrix(0,15,2)
law[,1]=c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
law[,2]=c(3.39,3.30,2.81,3.03,3.44,3.07,3,3.43,3.36,3.13,3.12,2.74,2.76,2.88,2.96)
n=nrow(law)

law.jack=rep(0,n)
for(i in 1:n){
	sam=(1:n)[-i]
	law.jack[i]=cor(law[sam,1],law[sam,2])
}

# Calculate the Jackknife estimate of the variance of the estimate
law.jack.var=(n-1)/n*sum((law.jack-mean(law.jack))^2)
