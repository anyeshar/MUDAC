################################################################################
#                              STAT 480            
#                         Unit 1 ---  Lecture 10
################################################################################

# 1. Find the mean of 100 random standard normal numbers and repeat it 10 times
replicate(10, mean(rnorm(100)))

# 2. Find the mean of 10 random standard normal numbers, repeat it 100 times, 
# and draw the histgram of 100 means
hist(replicate(100, mean(rnorm(10))))

# 3. Normal Approximation to Binomials
pbinom(45,100,0.4)-pbinom(35,100,0.4)

mu=100*0.4
sig=sqrt(100*0.4*(1-0.4))
pnorm(45.5, mu, sig)-pnorm(35.5, mu, sig)

# 4. Sampling
# Roll a die
sample(1:6,10,replace=TRUE)
# toss a coin
sample(c("H","T"),10,replace=TRUE)
# pick 6 of 54 (a lottery)
sample(1:54,6) # no replacement
# pick a card. (Fancy! Uses paste, rep)
cards=paste(rep(c("A",2:10,"J","Q","K"),4),c("H","D","S","C"))
sample(cards,5)

# 5. CLT
 
# create a single standardized average based on n exponential random variables 
# with mean mu:
f = function(n,mu) {
 (mean(rexp(n,1/mu))-mu)/(mu/sqrt(n))
}

# sample size n=5, mu=10, repeat 100 times
xvals = seq(-3,3,.01) # for the density plot
hist(replicate(100,f(5,10)),probability=TRUE,main="n=5",col=gray(.95))
points(xvals,dnorm(xvals,0,1),type="l") # plot normal curve

# sample size n=50, repeat 100 times 
xvals = seq(-3,3,.01) # for the density plot
hist(replicate(100,f(50,10)),probability=TRUE,main="n=50",col=gray(.95))
points(xvals,dnorm(xvals,0,1),type="l") # plot normal curve


sim1 = function(n){
	hist(replicate(n,mean(rnorm(100))))
}

sim2 = function(n,m){
	hist(replicate(n,mean(rnorm(m))))
}

sim3 = function(n,m,d){
	if(d=="gaussian"){
		hist(replicate(n,mean(rnorm(m))))
	}
	if(d=="uniform"){
		hist(replicate(n,mean(runif(m))))
	}
}