################################################################################
#                              STAT 480            
#                         Unit 5 ---  Lecture 1
#       A Simple Example: Estimate the Variance of Sample Mean
################################################################################

## Install combinat package first
library(combinat) 
x=c(2,4,9,12)
n=length(x)
x.bar=mean(x)
x.bar.var=var(x)/n*(n-1)/n

################################################################################
#  Enumerate all possible samples of size 4 
################################################################################
dim=rep(4,4)
index=hcube(dim, scale=1, transl=0)
K=nrow(index)

#Initialize the vector in which we will store the means from K possible samples
xbar.all=rep(0,K)
for(k in 1:K){
	index_k=index[k,]
	x.k.star=x[index_k]
	xbar.all[k]=mean(x.k.star)
}

mean(xbar.all)
var(xbar.all)

################################################################################
#  Bootstrap Approach 
################################################################################

#Take B=1000 bootstrap samples
B=1000
#Initialize the vector in which we will store the means from the resample
mean.boot=rep(0,B)
for(k in 1:B){
	sample.k=sample(x, replace=T)
	mean.boot[k]=mean(sample.k)
}
#Calculate the bootstrap estimate of the variance of the sample mean
var(mean.boot)


 

